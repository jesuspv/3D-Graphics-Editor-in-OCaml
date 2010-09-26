(** Implementación de sombras planas sobre OpenGL *)

(** Construye la matriz de proyección de la sombra *)
let matriz_sombra ((luz0, luz1, luz2, luz3) : Gl.point4) : GlMat.t =
    let matriz = Array.make_matrix 4 4 0.0 in
    let dot = luz1 +. luz3 *. Defs.sombra_compensacion in

    matriz.(0).(0) <- dot;
    matriz.(1).(0) <- 0.0 -. luz0;
    matriz.(2).(0) <- 0.0;
    matriz.(3).(0) <- 0.0 -. luz0 *. Defs.sombra_compensacion;

    matriz.(0).(1) <- 0.0;
    matriz.(1).(1) <- dot -. luz1;
    matriz.(2).(1) <- 0.0;
    matriz.(3).(1) <- 0.0 -. luz1 *. Defs.sombra_compensacion;

    matriz.(0).(2) <- 0.0;
    matriz.(1).(2) <- 0.0 -. luz2;
    matriz.(2).(2) <- dot;
    matriz.(3).(2) <- 0.0 -. luz2 *. Defs.sombra_compensacion;

    matriz.(0).(3) <- 0.0;
    matriz.(1).(3) <- 0.0 -. luz3;
    matriz.(2).(3) <- 0.0;
    matriz.(3).(3) <- dot -. luz3 *. Defs.sombra_compensacion;

    GlMat.of_array matriz

(** Dibuja los reflejos de los objetos en +Y sobre el plano XZ *)
let dibujar
      ~(cam : Camara.camara)
      ~(objs : Dibujable.dibujable list)
      ~(luces : Luz.luz list)
      ~(vista : Togl.widget)
      ~(indice : int) : unit =
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`stencil_buffer];
   GlMisc.push_attrib [`color_buffer];
   GlMisc.push_attrib [`polygon];
   GlMisc.push_attrib [`lighting];

   GlClear.stencil 0;
   GlClear.clear [`stencil];

   (* dibujamos el suelo *)
   Gl.disable `lighting;
   Gl.enable `blend;
   GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
   Togl.make_current vista;
   Suelo.call_display_list_solido indice;
   Gl.disable `blend;

   (* marca el stencil buffer *)
   GlFunc.color_mask ~red:false ~green:false ~blue:false ~alpha:false ();
   Gl.enable `stencil_test;
   GlFunc.stencil_func `always ~ref:1 ~mask:1;
   GlFunc.stencil_op ~fail:`keep ~zfail:`keep ~zpass:`replace ();
   Gl.disable `depth_test;
   Togl.make_current vista;
   Suelo.call_display_list_solido indice;
   Gl.enable `depth_test;

   (** preparamos el dibujado de los reflejos *)
   GlFunc.color_mask ~red:true ~green:true ~blue:true ~alpha:true ();
   GlFunc.stencil_func `equal ~ref:1 ~mask:1;
   GlFunc.stencil_op ~fail:`keep ~zfail:`keep ~zpass:`keep ();

   (*
    * Dibuja las sombras
    *)
   GlMat.push ();
   Gl.enable `blend;
   (* obtiene el color de la luz ambiente *)
   let amb_r, amb_g, amb_b, _ =
      (List.hd (List.filter (fun luz -> luz # clase = "ambiental") luces)) # get_ambiente
   in
   let ambiente = amb_r, amb_g, amb_b in
   (* obtiene dos particiones con los objetos opacos y los transparentes *)
   let obj_opacos, obj_transp = List.partition (fun obj -> obj # get_material # es_opaco) objs
   in
   (* para cada luz de la escena *)
   List.iter
      (fun luz ->
         let px, py, pz = luz # get_posicion in

         match luz # clase with
            "puntual" | "focal" | "direccional" ->
               let pos =
                  if luz # clase <> "direccional" then
                     let x, y, z = luz # get_posicion in x, y, z, 1.0
                  else
                     let ox, oy, oz = luz # get_orientacion in
                     let mr = new MatRot.mat_rot in
                     mr # rotar_z oz;
                     mr # rotar_y oy;
                     mr # rotar_x ox;
                     let x, y, z = Func.mult_mv mr # get (0.0, 0.0, -1.0) in
                     -.x, -.y, -.z, 0.0
               in
               let sombra = matriz_sombra pos in
               (* dibuja los objetos opacos *)
               List.iter
               (fun (obj : Dibujable.dibujable) ->
                  GlMat.push ();
                  GlMat.mult       sombra;
                  GlMat.translate3 obj # get_t;
                  GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
                  GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
                  GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
                  GlMat.scale3     obj # get_s;
                  GlDraw.color
                     ~alpha:(let _, _, _, a = obj # get_material # get_difuso in a)
                     ambiente;
                  Togl.make_current vista;
                  obj # call_display_list indice ~normales:cam # get_normales ~cara:`front ~sombra:true ();
                  GlMat.pop ())
               obj_opacos;
            (* dibuja los objetos transparentes *)
            GlFunc.depth_mask false; (* Z-buffer sólo lectura *)
            List.iter
               (fun (obj : Dibujable.dibujable) ->
                  GlMat.push ();
                  GlMat.mult       sombra;
                  GlMat.translate3 obj # get_t;
                  GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
                  GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
                  GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
                  GlMat.scale3     obj # get_s;
                  GlDraw.color
                     ~alpha:(let _, _, _, a = obj # get_material # get_difuso in a)
                     ambiente;
                  Togl.make_current vista;
                  obj # call_display_list indice ~normales:cam # get_normales ~cara:`front ~sombra:true ();
                  GlMat.pop ())
               obj_transp;
            GlFunc.depth_mask true (* Z-buffer lectura/escritura *)
          | _ -> () (* no hace nada *))
      luces;
   GlMat.pop ();

   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ()
