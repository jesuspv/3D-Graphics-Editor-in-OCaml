(** Implementación de reflejos sobre OpenGL *)

(** Dibuja los reflejos de los objetos en +Y sobre el plano XZ *)
let dibujar
      ~(cam : Camara.camara)
      ~(objs : Dibujable.dibujable list)
      ~(luces : Luz.luz list)
      ~(sentido : [`Horario | `Antihorario])
      ~(vista : Togl.widget)
      ~(indice : int) : unit =
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`stencil_buffer];
   GlMisc.push_attrib [`color_buffer];
   GlMisc.push_attrib [`lighting];
   GlMisc.push_attrib [`polygon];

   GlClear.stencil 0;
   GlClear.clear [`stencil];

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

   Gl.enable `clip_plane0;
   GlMisc.clip_plane ~plane:0 (0.0, -1.0, 0.0, 0.0);

   (*
    * Dibuja los reflejos
    *)
   GlMat.push ();
   GlMat.scale3 (1.0, -1.0, 1.0);

   (* variable para corregir el pintado de la cara correcta en el reflejo *)
   let cara = ref `front in

   (* invierte el sentido de las normales *)
   (match sentido with
      `Horario     -> GlDraw.front_face `ccw; GlDraw.cull_face `front;
         if Gl.is_enabled `cull_face then cara := `back;
    | `Antihorario -> GlDraw.front_face `cw; GlDraw.cull_face `back);

   (* habilita la iluminación *)
   List.iter (fun luz -> luz # encender) luces;
   (* obtiene dos particiones con los objetos opacos y los transparentes *)
   let obj_opacos, obj_transp = List.partition (fun obj -> obj # get_material # es_opaco) objs
   in
   (* dibuja los objetos opacos *)
   List.iter
      (fun (obj : Dibujable.dibujable) ->
         GlMat.push ();
         GlMat.translate3 obj # get_t;
         GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
         GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
         GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
         GlMat.scale3     obj # get_s;
         Togl.make_current vista;
         obj # call_display_list indice ~normales:cam # get_normales ~cara:!cara ();
         GlMat.pop ())
      obj_opacos;
   (* dibuja los objetos transparentes *)
   GlFunc.depth_mask false; (* Z-buffer sólo lectura *)
   List.iter
      (fun (obj : Dibujable.dibujable) ->
         GlMat.push ();
         GlMat.translate3 obj # get_t;
         GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
         GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
         GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
         GlMat.scale3     obj # get_s;
         Togl.make_current vista;
         obj # call_display_list indice ~normales:cam # get_normales ~cara:!cara ();
         GlMat.pop ())
      obj_transp;
   GlFunc.depth_mask true; (* Z-buffer lectura/escritura *)
   (* deshabilita la iluminación *)
   List.iter (fun luz -> luz # apagar) luces;
   GlMat.pop ();

   Gl.disable `clip_plane0;
   Gl.disable `stencil_test;

   (* dibujamos el suelo *)
   Gl.disable `lighting;
   Gl.enable `blend;
   GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
   Togl.make_current vista;
   Suelo.call_display_list_solido indice;
   Gl.disable `blend;

   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ()
