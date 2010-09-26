open MatRot       (** Clase [mat_rot]      *)
open Serializable (** Clase [serializable] *)

(** Tipo de proyección *)
type proyeccion = Ortogonal | Perspectiva

(** Sentido en que los vértices marcan la cara frontal *)
type sentido    = Horario | Antihorario

(** Modo de visualización de los polígonos *)
type modo       = Solido | Alambrico | Puntos

(** Modelo de sombreado *)
type sombreado  = Plano | Suavizado

(** Cálculo de las normales a los polígonos *)
type normales   = Poligono | Vertice

(** Cámara *)
class camara (xml : Xml.xml) = object (self)

   inherit serializable

   (** Posición (X,Y,Z) *)
   val mutable situacion = (1.0, 1.0, 1.0)

   (** Matriz de rotación *)
   val mutable giro = new mat_rot

   (** Zoom *)
   val mutable zoom = 1.0

   (** Nombre *)
   val mutable nombre = ""

   (** Parámetros de control de la representación *)
   val mutable _proyeccion  = Ortogonal
   val mutable _sentido     = Horario
   val mutable _modo        = Solido
   val mutable _z_buffer    = true
   val mutable _culling     = true
   val mutable _sombreado   = Plano
   val mutable _normales    = Poligono
   val mutable _iluminacion = false

   (** Selector de la situación *)
   method get_situacion = situacion

   (** Modificador de la situación *)
   method set_situacion
         ?(x = self # get_x situacion)
         ?(y = self # get_y situacion)
         ?(z = self # get_z situacion) () =
      situacion <- x, y, z

   (** Selector del giro *)
   method get_giro = giro # get

   (** Modificador del giro *)
   method set_giro
         ?(x = let gx, _, _ = giro # euler in gx)
         ?(y = let _, gy, _ = giro # euler in gy)
         ?(z = let _, _, gz = giro # euler in gz) () =
      giro # reset;
      giro # rotar_z (mod_float z 360.0);
      giro # rotar_y (mod_float y 360.0);
      giro # rotar_x (mod_float x 360.0)

   (** Selector del zoom *)
   method get_zoom = zoom

   (** Modificador del zoom *)
   method set_zoom factor =
      (* corrige el zoom *)
      zoom <- (if factor < 0.0 then
            0.0
         else if _proyeccion = Perspectiva && factor < 0.25 then
            0.25 (* 45 / 0.25 = 180 grados *)
         else
            factor)

   (** Desplaza la situación en proporción a las unidades indicadas *)
   method situar ?(x = 0) ?(y = 0) ?(z = 0) () =
      let factor = Defs.pixel_factor_cam_t in
      let dx, dy, dz = float x *. factor, float y *. factor, float z *. factor in
      let x0, y0, z0 = situacion in
      situacion <- (x0 +. dx, y0 +. dy, z0 +. dz);
      Printf.sprintf "Cámara %s: Situación T(x,y,z) = (%.2f, %.2f, %.2f)"
          self # get_nombre
         (self # get_x situacion)
         (self # get_y situacion)
         (self # get_z situacion)

   (** Modifica el ángulo de giro en proporción a las unidades indicadas *)
   method girar ?(x = 0) ?(y = 0) ?(z = 0) () =
      if z <> 0 then giro # rotar_z (mod_float (float z *. Defs.pixel_factor_cam_r) 360.0);
      if y <> 0 then giro # rotar_y (mod_float (float y *. Defs.pixel_factor_cam_r) 360.0);
      if x <> 0 then giro # rotar_x (mod_float (float x *. Defs.pixel_factor_cam_r) 360.0);

      let gx, gy, gz = giro # euler in
      Printf.sprintf "Cámara %s: Giro R(x,y,z) = (%.2f, %.2f, %.2f) grados" self # get_nombre gx gy gz

   (** Modifica el zoom en proporción a las unidades indicadas *)
   method zoom incr =
      let factor = Defs.pixel_factor_cam_s in
      self # set_zoom ((float incr *. factor) +. zoom);
      if _proyeccion = Ortogonal then
         Printf.sprintf "Cámara %s: Zoom Z = %.2f"
            self # get_nombre
            self # get_zoom
    else
         if 45.0 /. self # get_zoom <> infinity then
            Printf.sprintf "Cámara %s: Campo Visual FOV = %.2f grados"
               self # get_nombre
               (45.0 /. self # get_zoom)
         else
            Printf.sprintf "Cámara %s: Campo Visual FOV = 180.00 grados"
               self # get_nombre

   (** Selector del nombre *)
   method get_nombre = nombre

   (** Modificador del nombre *)
   method set_nombre (n : string) = nombre <- n

   (** Selector del sentido de los vértices que marcan la cara frontal *)
   method get_sentido : [`Horario | `Antihorario] =
      match _sentido with Horario -> `Horario | Antihorario -> `Antihorario

   (** Selector del cálculo de las normales *)
   method get_normales : [`Poligono | `Vertice] =
      match _normales with Poligono -> `Poligono | Vertice -> `Vertice

   (** Establece los parámetros de control de la representación *)
   method set_visualizacion
         ?(proyeccion  = _proyeccion )
         ?(sentido     = _sentido    )
         ?(modo        = _modo       )
         ?(z_buffer    = _z_buffer   )
         ?(culling     = _culling    )
         ?(sombreado   = _sombreado  )
         ?(normales    = _normales   )
         ?(iluminacion = _iluminacion) () =
      _proyeccion  <- proyeccion;
      _sentido     <- sentido;
      _modo        <- modo;
      _z_buffer    <- z_buffer;
      _culling     <- culling;
      _sombreado   <- sombreado;
      _normales    <- normales;
      _iluminacion <- iluminacion;
      (* corrige el zoom para el cambio de proyección *)
      self # set_zoom zoom

   (** Representa bajo la vista de la cámara *)
   method representar ~(vista : Togl.widget) ~(indice : int) =
      let w, h = Winfo.width vista, Winfo.height vista in

      GlFunc.depth_func `less;
      GlClear.depth (1.0);
      GlClear.color Defs.escena_fondo_color;
      GlClear.clear [`color; `depth];
      GlDraw.viewport ~x:0 ~y:0 ~w ~h;
      GlMat.mode `projection;
      GlMat.load_identity ();
      (* proyección *)
      let aspecto = (float w) /. (float h) in
      (match _proyeccion with
         Ortogonal   ->
            GlMat.ortho
               ~x:(-1.0 *. aspecto /. zoom, 1.0 *. aspecto /. zoom)
               ~y:(-1.0 /. zoom,            1.0 /. zoom)
               ~z:(-1000.0 /. zoom,         1000.0 /. zoom)
       | Perspectiva ->
            GluMat.perspective ~fovy:(45.0 /. zoom) ~aspect:aspecto ~z:(1.0, 1000.0));
      GlMat.mode `modelview;
      GlMat.load_identity ();
      GlMat.mult giro # get;
      GlMat.translate3
         (-.(self # get_x situacion),
          -.(self # get_y situacion),
          -.(self # get_z situacion));

      (* sentido *)
      (match _sentido with
         Horario     -> GlDraw.front_face `cw
       | Antihorario -> GlDraw.front_face `ccw);
      (* modo *)
      (match _modo with
         Solido    -> GlDraw.polygon_mode ~face:`both `fill
       | Alambrico -> GlDraw.polygon_mode ~face:`both `line
       | Puntos    -> GlDraw.polygon_mode ~face:`both `point);
      (* z-buffer *)
      if _z_buffer then Gl.enable `depth_test else Gl.disable `depth_test;
      (* culling *)
      GlDraw.cull_face `back;
      if _culling then Gl.enable `cull_face else Gl.disable `cull_face;
      (* sombreado *)
      (match _sombreado with
         Plano     -> GlDraw.shade_model `flat
       | Suavizado -> GlDraw.shade_model `smooth);
      (* normales *)
      (match _normales with
         Poligono -> () (* de la representación se encargan los propios objetos *)
       | Vertice  -> ());
      (* iluminación *)
      if _iluminacion then Gl.enable `lighting else Gl.disable `lighting;

      (* dibuja el grid *)
      GlMat.push ();
      let gx, gy ,gz = giro # euler in
      let x, y, z =
         mod_float (abs_float gx) 90.0,
         mod_float (abs_float gy) 90.0,
         mod_float (abs_float gz) 90.0 in
      if x < (Defs.pixel_factor_cam_r /. 2.0) &&
         y < (Defs.pixel_factor_cam_r /. 2.0) &&
         z < (Defs.pixel_factor_cam_r /. 2.0) then begin
         GlMat.rotate ~angle:gx ~x:1.0 ();
         GlMat.rotate ~angle:gy ~y:1.0 ();
         GlMat.rotate ~angle:gz ~z:1.0 ()
      end else
         GlMat.rotate ~angle:(-90.0) ~x:1.0 ();
      Togl.make_current vista;
      Suelo.call_display_list_alambrico indice;
      GlMat.pop ();

      (* activa el blending *)
      Gl.enable `blend;
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;

      (* las normales serán siempre unitarias tras escalados no uniformes *)
      Gl.enable `normalize;

      (* importante para que no se produzcan errores en las caras de los polígonos *)
      GlLight.light_model (`two_side true);

      GlMisc.hint `perspective_correction `nicest

   (* Devuelve la selección actual *)
   method seleccionar ~x ~y
         ~(objs  : Dibujable.dibujable list)
         ~(luces : Luz.luz list)
         ~(vista : Togl.widget) : [`Obj | `Luz] * int =
      let w, h = Winfo.width vista, Winfo.height vista in

      let buffer = Raw.create `uint ~len:256 in
      GlMisc.select_buffer buffer;
      ignore (GlMisc.render_mode `select);

      GlDraw.viewport ~x:0 ~y:0 ~w ~h;
      GlMat.mode `projection;
      GlMat.push ();
      GlMat.load_identity ();
      GluMat.pick_matrix ~x:(float x) ~y:(float y) ~width:3.0 ~height:3.0;

      (* proyección *)
      let aspecto = (float w) /. (float h) in
      (match _proyeccion with
         Ortogonal   ->
            GlMat.ortho
               ~x:(-1.0 *. aspecto /. zoom, 1.0 *. aspecto /. zoom)
               ~y:(-1.0 /. zoom,            1.0 /. zoom)
               ~z:(-1000.0 /. zoom,         1000.0 /. zoom)
       | Perspectiva ->
            GluMat.perspective ~fovy:(45.0 /. zoom) ~aspect:aspecto ~z:(1.0, 1000.0));
      GlMat.mode `modelview;
      GlMat.load_identity ();
      GlMat.mult giro # get;
      GlMat.translate3
         (-.(self # get_x situacion),
          -.(self # get_y situacion),
          -.(self # get_z situacion));

      (* guarda el estado actual *)
      GlMisc.push_attrib [`polygon];
      Gl.disable `cull_face; (* también se seleccionarán las caras ocultas *)

      (* renderiza la escena *)
      GlMisc.init_names ();
      for i = 0 to (List.length objs) - 1 do
         let obj = List.nth objs i in
         GlMisc.push_name i;
         GlMat.push ();
         GlMat.translate ~x:obj # get_tx ~y:obj # get_ty ~z:obj # get_tz ();
         GlMat.rotate    ~angle:obj # get_rx ~x:1.0 ();
         GlMat.rotate    ~angle:obj # get_ry  ~y:1.0 ();
         GlMat.rotate    ~angle:obj # get_rz ~z:1.0 ();
         GlMat.scale     ~x:obj # get_sx ~y:obj # get_sy ~z:obj # get_sz ();
         obj # dibujar ~normales:self # get_normales ~cara:`front ();
         GlMat.pop ();
         GlMisc.pop_name ();
      done;
      for i = 1 to List.length luces do
         let luz = List.nth luces (i - 1) in
         GlMisc.push_name (List.length objs + i);
         GlMat.push ();
         GlMat.translate3 luz # get_posicion;
         let rx, ry, rz = luz # get_orientacion in
         GlMat.rotate     ~angle:rx ~x:1.0 (); 
         GlMat.rotate     ~angle:ry ~y:1.0 ();
         GlMat.rotate     ~angle:rz ~z:1.0 ();
         luz # dibujar;
         GlMat.pop ();
         GlMisc.pop_name ();
      done;

      (* restaura el estado anterior *)
      GlMisc.pop_attrib ();

      GlMat.mode `projection;
      GlMat.pop ();
      GlMat.mode `modelview;
      let hits = GlMisc.render_mode `render
      and opcion = ref (-1)
      in
      if hits > 0 then begin
         opcion := Raw.get buffer ~pos:3;
         let profundidad = ref (Raw.get buffer ~pos:1) in

         for i = 1 to hits - 1 do
            if (Raw.get buffer ~pos:(i * 4 + 1)) < !profundidad then begin
               opcion := Raw.get buffer ~pos:(i * 4 + 3);
               profundidad := Raw.get buffer ~pos:(i * 4 + 1)
            end
         done
      end;

      if !opcion = -1 then
         (`Obj, !opcion)
      else if !opcion < (List.length objs) then
         (`Obj, !opcion)
      else (* luz *)
         (`Luz, !opcion - (List.length objs) - 1);

   (** Dibuja los controles de la cámara con una disposición en 2D *)
   method dibujar_controles (vista : Togl.widget) =
      GlMisc.push_attrib [`lighting];
      GlMisc.push_attrib [`depth_buffer];
      Gl.disable `lighting;
      Gl.disable `depth_test;

      GlMat.mode `modelview;
      GlMat.push ();
      GlMat.load_identity ();
      GlMat.mode `projection;
      GlMat.push ();
      GlMat.load_identity ();
      let aspecto = (float (Winfo.width vista)) /. (float (Winfo.height vista)) in
      GlMat.ortho ~x:(-1.0 *. aspecto, 1.0 *. aspecto) ~y:(-1.0, 1.0) ~z:(-1.0, 1.0);

      (* dibuja los ejes de coordenadas globales *)
      let gx, gy, gz = giro # euler in
      GlMat.push ();
      GlMat.translate3 (-.aspecto +. 0.25, -0.75, 0.0);
      GlMat.mult giro # get;
      Func.dibujar_marcador_traslacion [`X; `Y; `Z] 0.20;
      GlMat.pop ();

      (* escribe el nombre de la cámara *)
      GlDraw.color Defs.camara_nombre_color;
      let base = Togl.load_bitmap_font vista ~font:`Helvetica_12 in
      GlPix.raster_pos ~x:(-.aspecto +. 0.10) ~y:0.85 ();
      GlList.call_lists ~base (`byte self # get_nombre);
      Togl.unload_bitmap_font vista ~base:base;

      GlMat.pop ();
      GlMat.mode `modelview;
      GlMat.pop ();

      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();

   (** Muestra un menú popup con los parámetros de visualización *)
   method mostrar_parametros ~x ~y (vista : Togl.widget) =
      let nombre = "Cámara " ^ self # get_nombre
      and m = Menu.create ~title:nombre vista in
      (* nombre *)
      Menu.add_command ~label:nombre m;
      Menu.add_separator m;
      (* proyección *)
      let var_pr = Textvariable.create () in
      Textvariable.set var_pr (match _proyeccion with Ortogonal -> "o" | Perspectiva -> "p");
      let mp = Menu.create m in
      Menu.add_cascade ~label:"Proyección" ~menu:mp ~underline:0 m;
      Menu.add_radiobutton ~label:"Ortogonal"
         ~underline:0
         ~command:(fun () -> _proyeccion <- Ortogonal;
            Estado.mostrar (nombre ^ ": proyección ortogonal seleccionada");
            Togl.render vista)
         ~value:"o"
         ~variable:var_pr
         mp;
      Menu.add_radiobutton ~label:"Perpectiva"
         ~underline:0
         ~command:(fun () -> _proyeccion <- Perspectiva;
            (* corrige el zoom *)
            self # set_zoom zoom;
            Estado.mostrar (nombre ^ ": proyección perspectiva seleccionada");
            Togl.render vista)
         ~value:"p"
         ~variable:var_pr
         mp;
      (* sentido *)
      let var_se = Textvariable.create () in
      Textvariable.set var_se (match _sentido with Horario -> "h" | Antihorario -> "a");
      let ms = Menu.create m in
      Menu.add_cascade ~label:"Sentido" ~menu:ms ~underline:0 m;
      Menu.add_radiobutton ~label:"Horario"
         ~underline:0
         ~command:(fun () -> _sentido <- Horario;
            Estado.mostrar (nombre ^ ": sentido horario de los vértices marcan la cara exterior seleccionado");
            Togl.render vista)
         ~value:"h"
         ~variable:var_se
         ms;
      Menu.add_radiobutton ~label:"Antihorario"
         ~underline:0
         ~command:(fun () -> _sentido <- Antihorario;
            Estado.mostrar (nombre ^ ": sentido antihorario de los vértices marcan la cara exterior seleccionado");
            Togl.render vista)
         ~value:"a"
         ~variable:var_se
         ms;
      (* modo *)
      let var_mo = Textvariable.create () in
      Textvariable.set var_mo (match _modo with Solido -> "s" | Alambrico -> "a" | Puntos -> "p");
      let mm = Menu.create m in
      Menu.add_cascade ~label:"Modo" ~menu:mm ~underline:0 m;
      Menu.add_radiobutton ~label:"Sólido"
         ~underline:0
         ~command:(fun () -> _modo <- Solido;
            Estado.mostrar (nombre ^ ": modo de visualización sólido seleccionado");
            Togl.render vista)
         ~value:"s"
         ~variable:var_mo
         mm;
      Menu.add_radiobutton ~label:"Alámbrico"
         ~underline:0
         ~command:(fun () -> _modo <- Alambrico;
            Estado.mostrar (nombre ^ ": modo de visualización alámbrico seleccionado");
            Togl.render vista)
         ~value:"a"
         ~variable:var_mo
         mm;
      Menu.add_radiobutton ~label:"Puntos"
         ~underline:0
         ~command:(fun () -> _modo <- Puntos;
            Estado.mostrar (nombre ^ ": modo de visualización con puntos seleccionado");
            Togl.render vista)
         ~value:"p"
         ~variable:var_mo
         mm;
      (* sombreado *)
      let var_so = Textvariable.create () in
      Textvariable.set var_so (match _sombreado with Plano -> "p" | Suavizado -> "s");
      let ms = Menu.create m in
      Menu.add_cascade ~label:"Sombreado" ~menu:ms ~underline:1 m;
      Menu.add_radiobutton ~label:"Plano"
         ~underline:0
         ~command:(fun () -> _sombreado <- Plano;
            Estado.mostrar (nombre ^ ": modelo de sombreado plano seleccionado");
            Togl.render vista)
         ~value:"p"
         ~variable:var_so
         ms;
      Menu.add_radiobutton ~label:"Suavizado"
         ~underline:0
         ~command:(fun () -> _sombreado <- Suavizado;
            Estado.mostrar (nombre ^ ": modelo de sombreado suavizado seleccionado");
            Togl.render vista)
         ~value:"s"
         ~variable:var_so
         ms;
      (* normales *)
      let var_nm = Textvariable.create () in
      Textvariable.set var_nm (match _normales with Poligono -> "p" | Vertice -> "v");
      let mn = Menu.create m in
      Menu.add_cascade ~label:"Normales" ~menu:mn ~underline:0 m;
      Menu.add_radiobutton ~label:"Polígono"
         ~underline:0
         ~command:(fun () -> _normales <- Poligono;
            Estado.mostrar (nombre ^ ": cálculo de las normales al polígono seleccionado");
            Togl.render vista)
         ~value:"p"
         ~variable:var_nm
         mn;
      Menu.add_radiobutton ~label:"Vértice"
         ~underline:0
         ~command:(fun () -> _normales <- Vertice;
            Estado.mostrar (nombre ^ ": cálculo de las normales al vértice seleccionado");
            Togl.render vista)
         ~value:"v"
         ~variable:var_nm
         mn;
      (* z-buffer *)
      let zb = Textvariable.create () in
      Textvariable.set zb (if _z_buffer then "on" else "off");
      Menu.add_checkbutton ~label:"Z-buffer"
         ~underline:0
         ~command:(fun () -> _z_buffer <- (Textvariable.get zb = "on");
            Estado.mostrar (nombre ^ if _z_buffer then ": Z-buffer activo" else ": Z-buffer inactivo");
            Togl.render vista)
         ~onvalue:"on"
         ~offvalue:"off"
         ~variable:zb
         m;
      (* culling *)
      let cu = Textvariable.create () in
      Textvariable.set cu (if _culling then "on" else "off");
      Menu.add_checkbutton ~label:"Culling"
         ~underline:0
         ~command:(fun () -> _culling <- (Textvariable.get cu = "on");
            Estado.mostrar (nombre ^ if _culling then
               ": eliminación de caras ocultas (culling) activa" else
               ": eliminación de caras ocultas (culling) inactiva");
            Togl.render vista)
         ~onvalue:"on"
         ~offvalue:"off"
         ~variable:cu
         m;
      (* iluminación *)
      let il = Textvariable.create () in
      Textvariable.set il (if _iluminacion then "on" else "off");
      Menu.add_checkbutton ~label:"Iluminación"
         ~underline:0
         ~command:(fun () -> _iluminacion <- (Textvariable.get il = "on");
            Estado.mostrar (nombre ^ if _iluminacion then ": luz ambiente activa" else ": luz ambiente inactiva");
            Togl.render vista)
         ~onvalue:"on"
         ~offvalue:"off"
         ~variable:il
         m;
      (* muestra el popup *)
      Menu.popup ~x ~y m;

   (** Implementación de la interfaz [serializable] *)
   method clase = "camara"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self#clase then begin
         (* establece los datos desde los atributos *)
         List.iter (function (n, v) ->
                match (String.lowercase n, v) with
                   ("nombre", n) -> self # set_nombre n
                 | ("proyeccion" , "ortogonal"  ) -> self # set_visualizacion ~proyeccion:Ortogonal   ()
                 | ("proyeccion" , "perspectiva") -> self # set_visualizacion ~proyeccion:Perspectiva ()
                 | ("sentido"    , "horario"    ) -> self # set_visualizacion ~sentido:Horario        ()
                 | ("sentido"    , "antihorario") -> self # set_visualizacion ~sentido:Antihorario    ()
                 | ("modo"       , "solido"     ) -> self # set_visualizacion ~modo:Solido            ()
                 | ("modo"       , "alambrico"  ) -> self # set_visualizacion ~modo:Alambrico         ()
                 | ("modo"       , "puntos"     ) -> self # set_visualizacion ~modo:Puntos            ()
                 | ("z-buffer"   , "on"         ) -> self # set_visualizacion ~z_buffer:true          ()
                 | ("z-buffer"   , "off"        ) -> self # set_visualizacion ~z_buffer:false         ()
                 | ("culling"    , "on"         ) -> self # set_visualizacion ~culling:true           ()
                 | ("culling"    , "off"        ) -> self # set_visualizacion ~culling:false          ()
                 | ("sombreado"  , "plano"      ) -> self # set_visualizacion ~sombreado:Plano        ()
                 | ("sombreado"  , "suavizado"  ) -> self # set_visualizacion ~sombreado:Suavizado    ()
                 | ("normales"   , "poligono"   ) -> self # set_visualizacion ~normales:Poligono      ()
                 | ("normales"   , "vertice"    ) -> self # set_visualizacion ~normales:Vertice       ()
                 | ("iluminacion", "on"         ) -> self # set_visualizacion ~iluminacion:true       ()
                 | ("iluminacion", "off"        ) -> self # set_visualizacion ~iluminacion:false      ()
                 | _ -> raise (Invalid_argument (self#clase ^ ":from_xml: el atributo XML no se reconoce")))
             (Xml.attribs xml);
         (* establece los datos desde los hijos *)
         let hijos = Xml.children xml in
         let s = List.nth hijos 0 (* situación *)
         and g = List.nth hijos 1 (* giro      *)
         and z = List.nth hijos 2 (* zoom      *)
         and get_float nodo atrib = float_of_string (Xml.attrib nodo atrib)
         in
         let sx, sy, sz  = (get_float s "x"), (get_float s "y"), (get_float s "z")
         and gx, gy, gz  = (get_float g "x"), (get_float g "y"), (get_float g "z")
         and zoom_factor = float_of_string (Xml.attrib z "factor")
         in
         self # set_situacion ~x:sx ~y:sy ~z:sz ();
         self # set_giro      ~x:gx ~y:gy ~z:gz ();
         self # set_zoom      zoom_factor
      end else
         raise (Invalid_argument (self#clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      (* establece los datos para los atributos *)
      let atributos =
         [
            ("nombre", self # get_nombre);
            ("proyeccion",
               match _proyeccion with
                  Ortogonal   -> "ortogonal"
                | Perspectiva -> "perspectiva");
            ("sentido",
               match _sentido with
                  Horario     -> "horario"
                | Antihorario -> "antihorario");
            ("modo",
               match _modo with
                  Solido      -> "solido"
                | Alambrico   -> "alambrico"
                | Puntos      -> "puntos");
            ("z-buffer",
               if _z_buffer
               then "on"
               else "off");
            ("culling",
               if _culling
               then "on"
               else "off");
            ("sombreado",
               match _sombreado with
                  Plano       -> "plano"
                | Suavizado   -> "suavizado");
            ("normales",
               match _normales with
                  Poligono    -> "poligono"
                | Vertice     -> "vertice");
            ("iluminacion",
               if _iluminacion
               then "on"
               else "off")
         ]
      in
      (* establece los datos para los hijos *)
      let gx', gy', gz' = giro # euler in
      let gx v = Defs.float2xml (self # get_x v)
      and gy v = Defs.float2xml (self # get_y v)
      and gz v = Defs.float2xml (self # get_z v)
      in
      let s = Xml.Element ("situacion",
         [("x", gx situacion); ("y", gy situacion); ("z", gz situacion)],
         [])
      and g = Xml.Element ("giro",
         [("x", Defs.float2xml gx'); ("y", Defs.float2xml gy'); ("z", Defs.float2xml gz')],
         [])
      and z = Xml.Element ("zoom", [("factor", Defs.float2xml zoom)], [])
      in
      Xml.Element (self#clase, atributos, [s; g; z])

   (** Obtiene la coordenada [x] de un vector *)
   method private get_x (x, _, _) = x

   (** Obtiene la coordenada [y] de un vector *)
   method private get_y (_, y, _) = y

   (** Obtiene la coordenada [z] de un vector *)
   method private get_z (_, _, z) = z

   (** Constructor del objeto *)
   initializer
      self # from_xml xml
end

(** Obtiene una cámara por defecto para la vista superior *)
let get_camara_superior () : camara =
  (* establece los datos para los atributos *)
  let atributos =
     [
        ("nombre",      "Superior"   );
        ("proyeccion",  "ortogonal"  );
        ("sentido",     "antihorario");
        ("modo",        "alambrico"  );
        ("z-buffer",    "on"         );
        ("culling",     "off"        );
        ("sombreado",   "plano"      );
        ("normales",    "poligono"   );
        ("iluminacion", "off"        )
      ]
   (* establece los datos para los hijos *)
   and s = Xml.Element ("situacion",
      [("x", "0.0"); ("y", "1.0"); ("z", "0.0")],
      [])
   and g = Xml.Element ("giro",
      [("x", "-90.0"); ("y", "0.0"); ("z", "0.0")],
      [])
   and z = Xml.Element ("zoom", [("factor", "1.0")], [])
   in
   new camara (Xml.Element ("camara", atributos, [s; g; z]))

(** Obtiene una cámara por defecto para la vista anterior *)
let get_camara_anterior () : camara =
  (* establece los datos para los atributos *)
  let atributos =
     [
        ("nombre",      "Anterior"   );
        ("proyeccion",  "ortogonal"  );
        ("sentido",     "antihorario");
        ("modo",        "alambrico"  );
        ("z-buffer",    "on"         );
        ("culling",     "off"        );
        ("sombreado",   "plano"      );
        ("normales",    "poligono"   );
        ("iluminacion", "off"        )
      ]
   (* establece los datos para los hijos *)
   and s = Xml.Element ("situacion",
      [("x", "0.0"); ("y", "0.0"); ("z", "1.0")],
      [])
   and g = Xml.Element ("giro",
      [("x", "0.0"); ("y", "0.0"); ("z", "0.0")],
      [])
   and z = Xml.Element ("zoom", [("factor", "1.0")], [])
   in
   new camara (Xml.Element ("camara", atributos, [s; g; z]))

(** Obtiene una cámara por defecto para la vista izquierda *)
let get_camara_izquierda () : camara =
  (* establece los datos para los atributos *)
  let atributos =
     [
        ("nombre",      "Izquierda"  );
        ("proyeccion",  "ortogonal"  );
        ("sentido",     "antihorario");
        ("modo",        "alambrico"  );
        ("z-buffer",    "on"         );
        ("culling",     "off"        );
        ("sombreado",   "plano"      );
        ("normales",    "poligono"   );
        ("iluminacion", "off"        )
      ]
   (* establece los datos para los hijos *)
   and s = Xml.Element ("situacion",
      [("x", "-1.0"); ("y", "0.0"); ("z", "0.0")],
      [])
   and g = Xml.Element ("giro",
      [("x", "0.0"); ("y", "-90.0"); ("z", "0.0")],
      [])
   and z = Xml.Element ("zoom", [("factor", "1.0")], [])
   in
   new camara (Xml.Element ("camara", atributos, [s; g; z]))

(** Obtiene una cámara por defecto para la vista perspectiva *)
let get_camara_perspectiva () : camara =
  (* establece los datos para los atributos *)
  let atributos =
     [
        ("nombre",      "Perspectiva");
        ("proyeccion",  "perspectiva");
        ("sentido",     "antihorario");
        ("modo",        "solido"     );
        ("z-buffer",    "on"         );
        ("culling",     "on"         );
        ("sombreado",   "suavizado"  );
        ("normales",    "vertice"    );
        ("iluminacion", "on"         )
      ]
   (* establece los datos para los hijos *)
   and s = Xml.Element ("situacion",
      [("x", "2.5"); ("y", "3.0"); ("z", "2.5")],
      [])
   and g = Xml.Element ("giro",
      [("x", "-57.05"); ("y", "27.35"); ("z", "34.50")],
      [])
   and z = Xml.Element ("zoom", [("factor", "1.0")], [])
   in
   new camara (Xml.Element ("camara", atributos, [s; g; z]))
