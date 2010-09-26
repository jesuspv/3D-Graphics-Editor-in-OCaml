(** Mini-editor de polilíneas (líneas con efecto elástico). *)

(** Mini-editor de polilíneas para la extrusión *)
class extrusion (padre : 'a Widget.widget) = object (self)
   (** Barra de estado *)
   val mutable mensajes = None

   (** Indica si se debe dibujar el elástico *)
   val mutable elastico = false

   (** Indica si se ha terminado de dibujar *)
   val mutable terminado = false

   (** Coordenada del punto actual *)
   val mutable x = 0.0
   val mutable y = 0.0

   (** Lista de puntos ya fijados *)
   val mutable seccion : Gl.point2 list = []

   (** Muestra un mensaje por la barra de estado *)
   method private mostrar msj =
      match mensajes with
         None   -> ()
       | Some l -> Label.configure ~text:("  " ^ msj) l

   (** Dibuja el editor de extrusión *)
   method private dibujar_extrusion editor () =
      let w, h = Winfo.width editor, Winfo.height editor in
      GlClear.color Defs.escena_fondo_color;
      GlClear.clear [`color];
      Gl.enable `line_smooth;
      Gl.disable `lighting;
      Gl.disable `depth_test;
      GlDraw.shade_model `smooth;
      GlDraw.viewport ~x:0 ~y:0 ~w ~h;

      GlMat.mode `modelview;
      GlMat.push ();
      GlMat.load_identity ();
      GlMat.mode `projection;
      GlMat.push ();
      GlMat.load_identity ();
      let aspecto = (float w) /. (float h) in
      GlMat.ortho ~x:(-1.0 *. aspecto, 1.0 *. aspecto) ~y:(-1.0, 1.0) ~z:(-1.0, 1.0);

      (* dibuja el grid *)
      GlDraw.color Defs.escena_grid_color;
      GlDraw.line_width 1.0;
      GlDraw.begins `lines;
         (* horizontales *)
         GlDraw.vertex3 (-1.0 *. aspecto, -0.5, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto, -0.5, 0.0);
         GlDraw.vertex3 (-1.0 *. aspecto,  0.5, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto,  0.5, 0.0);
         (* verticales *)
         let ct  = ref 0.0
         and xd = 1.0 *. aspecto
         in
         while !ct <= xd do
            GlDraw.vertex3 (-.(!ct), -1.0, 0.0);
            GlDraw.vertex3 (-.(!ct),  1.0, 0.0);
            GlDraw.vertex3 (   !ct, -1.0, 0.0);
            GlDraw.vertex3 (   !ct,  1.0, 0.0);
            ct := !ct +. 0.5
         done;
      GlDraw.ends ();
      GlDraw.line_width 2.0;
      GlDraw.begins `lines;
         (* eje OX *)
         GlDraw.vertex3 (-1.0 *. aspecto,  0.0, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto,  0.0, 0.0);
         (* eje OY *)
         GlDraw.vertex3 ( 0.0, -1.0, 0.0);
         GlDraw.vertex3 ( 0.0,  1.0, 0.0);
      GlDraw.ends ();

      (* dibuja las líneas *)
      GlDraw.line_width 1.0;
      GlDraw.color Defs.polilineas_color;
      if not terminado then begin
         GlDraw.begins `line_strip;
         List.iter (fun (x, y) -> GlDraw.vertex3 (x, y, 0.0)) seccion;
         (* dibuja la línea elástica *)
         if elastico then begin
               GlDraw.color Defs.camara_nombre_color;
               GlDraw.vertex3 (x, y, 0.0)
         end;
         GlDraw.ends ()
      end else begin
         GlDraw.begins `line_loop;
         List.iter (fun (x, y) -> GlDraw.vertex3 (x, y, 0.0)) seccion;
         GlDraw.ends ()
      end;
      (* dibuja la numeración de los puntos *)
      GlDraw.color Defs.camara_nombre_color;
      let base = Togl.load_bitmap_font editor ~font:`Helvetica_18 in
      for i = 0 to List.length seccion - 1 do
         let u, v = List.nth seccion i in
         GlPix.raster_pos ~x:(u) ~y:v ();
         GlList.call_lists ~base (`byte (string_of_int (i + 1)));
      done;
      Togl.unload_bitmap_font editor ~base:base;

      Gl.flush ();
      Togl.swap_buffers editor

   (** Función para establecer el registro como oyente del entorno *)
   method private escuchar_entorno mat_l
         (objetos, materiales, luces : string list * string list * string list) =
      (* establece los materiales *)
      Listbox.delete mat_l ~first:(`Num 0) ~last:`End;
      Listbox.insert mat_l ~index:`End ~texts:["Imprimación"];
      Listbox.insert mat_l
         ~index:`End
         ~texts:(List.map
            (fun s -> let w = Defs.herramientas_listas_columnas in
               (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
            materiales);
      Listbox.activate mat_l ~index:(`Num 0)

   (** Muestra la ventana del editor de extrusión *)
   initializer
      (* ventana del editor *)
      let ventana = Toplevel.create padre in
      (* título de la ventana *)
      Wm.title_set ventana "[Extrusión sbre Z] Mini-editor de polilíneas::Gráficos por Computador";

      (* controles *)
      let profundidad_t = Textvariable.create () in Textvariable.set profundidad_t "1.0";
      let datos    = Frame.create ventana in
      let material = Frame.create datos in
      let profundidad_l = Label.create ~text:"  Profundidad:" datos
      and profundidad_e = Entry.create ~justify:`Right ~width:5 ~textvariable:profundidad_t datos
      and material_l    = Label.create ~text:"Material:" ~relief:`Flat material
      and material_lst  = Listbox.create ~height:1 ~width:Defs.herramientas_listas_columnas material
      in
      let aceptar  = Button.create
         ~text:"Aceptar"
         ~underline:0
         ~command:(fun () ->
            if terminado or (List.length seccion >= 3) then begin
               let seccion = if not terminado then seccion else List.append seccion [List.hd seccion] in
               try
                  let profundidad = float_of_string (Textvariable.get profundidad_t) in
                  match (Listbox.index material_lst ~index:`Active) with
                     `Num i -> let xml = 
                           if i = 0 then
                              Extrusion.get_xml seccion profundidad
                           else
                              let m = Entorno.get_material (i - 1) in
                              Extrusion.get_xml ~m seccion profundidad
                        in
                        try
                           Escena.add_obj (new Extrusion.extrusion xml :> Dibujable.dibujable);
                           Tk.destroy ventana
                        with
                           Invalid_argument _ -> self # mostrar "La profundidad debe ser positiva y distinta de cero"
               with
                  Failure _ -> self # mostrar "La profundidad debería ser un número ;-)"
            end else
               self # mostrar "Debe haber al menos 3 vértices para completar la edición")
         datos
      and cancelar = Button.create
         ~text:"Cancelar"
         ~underline:0
         ~command:(fun () -> Tk.destroy ventana)
         datos
      in

      (* establece la lista de materiales *)
      Listbox.insert material_lst ~index:`End ~texts:("Imprimación" ::
         (List.map (fun s -> let w = Defs.herramientas_listas_columnas in
            (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
            (Entorno.get_materiales ())));
      Listbox.activate material_lst ~index:(`Num 0);
      (* registra el editor como oyente del entorno *)
      Entorno.registrar_oyente (self # escuchar_entorno material_lst);
      (* evento *)
      Tk.bind material_lst ~events:[`Motion] ~action:(fun _ -> Focus.set material_lst);

      (* vista del editor *)
      let editor_panel = Frame.create
         ~relief:`Groove
         ~borderwidth:5
         ventana
      in
      let editor = Togl.create editor_panel
         ~width: ((fst Defs.vista_dims) * 2)
         ~height:((snd Defs.vista_dims) * 2)
         ~rgba:true
         ~depth:true
         ~double:true
      in
      (* habilita el elástico *)
      Tk.bind editor ~events:[`Enter]
      ~action:(fun _ -> elastico <- true;
      Togl.render editor);
      (* deshabilita el elástico *)
      Tk.bind editor ~events:[`Leave]
         ~action:(fun _ -> elastico <- false;
         Togl.render editor);
      (* añade un nuevo vértice *)
      Tk.bind editor ~events:[`ButtonPressDetail 1]
         ~fields:[`MouseX; `MouseY]
         ~action:(fun ev ->
            if terminado = false then begin
               let m_x, m_y = ev.Tk.ev_MouseX ,(Winfo.height editor) - ev.Tk.ev_MouseY in
               let x', y', _ = GluMat.unproject (float m_x, float m_y, 1.0) in
               x <- x'; y <- y';
               (match seccion with
                  [] -> let p = Printf.sprintf "(%.2f, %.2f)" x' y' in
                  self # mostrar ("Primer punto establecido en (u,v) = " ^ p)
                | a :: [] -> let u, v = a in let p = Printf.sprintf "(%.2f, %.2f)" u v in
                  self # mostrar ("Punto establecido en (u,v) = " ^ p)
                | l -> let u, v = List.hd (List.rev l) in let p = Printf.sprintf "(%.2f, %.2f)" u v in
                  self # mostrar ("Punto establecido en (u,v) = " ^ p));
               seccion <- List.append seccion [(x', y')];
               Togl.render editor
            end else
               self # mostrar "No se puede editar, ya se había terminado de introducir los puntos");
      (* elimina el vértice anterior *)
      Tk.bind editor ~events:[`ButtonPressDetail 2]
         ~action:(fun _ ->
            if not terminado then
               try
                  seccion <- List.rev (List.tl (List.rev seccion));
                  self # mostrar "Último punto eliminado";
               with
                  Failure _ -> self # mostrar "No hay puntos para eliminar"
            else
               self # mostrar "No se puede editar, ya se había terminado de introducir los puntos";
            Togl.render editor);
      (* termina la edición *)
      Tk.bind editor ~events:[`ButtonPressDetail 3]
         ~action:(fun _ ->
            if List.length seccion < 3 then
               self # mostrar "Debe haber al menos 3 vértices para completar la edición"
            else begin
               terminado <- true;
               self # mostrar "Edición completada. Seleccionar profundidad y material y pulsar el botón Aceptar";
               Togl.render editor
            end);
      (* actualiza la posición del ratón *)
      Tk.bind editor ~events:[`Motion]
      ~fields:[`MouseX; `MouseY]
      ~action:(fun ev -> let m_x, m_y = ev.Tk.ev_MouseX ,(Winfo.height editor) - ev.Tk.ev_MouseY in
         let x', y', _ = GluMat.unproject (float m_x, float m_y, 1.0) in
         x <- x'; y <- y';
         if not terminado then
            let p = Printf.sprintf "(%.2f, %.2f)" x' y' in
            if seccion <> [] then
               self # mostrar ("<Mouse1> para posicionar el siguiente punto en P(u,v) = " ^ p)
            else
               self # mostrar ("<Mouse1> para posicionar el punto de origen en P(u,v) = " ^ p);
         Togl.render editor);
      (* dibuja el editor de extrusión *)
      Togl.display_func editor ~cb:(self # dibujar_extrusion editor);

      (* barra de estado *)
      let mensajes_l = Label.create ~text:"  <Mouse1> para posicionar el punto de origen" ~anchor:`W ~relief:`Ridge ventana in
      mensajes <- Some mensajes_l;

      (* empaqueta todos los widgets *)
      Tk.pack ~side:`Top      ~fill:`X              [datos];
      Tk.pack ~side:`Top   ~fill:`Both ~expand:true [editor_panel];
      Tk.pack              ~fill:`Both ~expand:true [editor];
      Tk.pack ~side:`Top   ~fill:`X                 [mensajes_l];
      Tk.pack ~side:`Left                           [Tk.coe profundidad_l; Tk.coe profundidad_e];
      Tk.pack ~side:`Left ~padx:15                  [material];
      Tk.pack ~side:`Left                           [Tk.coe material_l; Tk.coe material_lst];
      Tk.pack ~side:`Right                          [cancelar; aceptar];

      (** Establece el tamaño mínimo de la ventana *)
      Tkwait.visibility ventana;
      Wm.minsize_set ventana ~width:(Winfo.width  ventana) ~height:(Winfo.height ventana)
end

(** Mini-editor de polilíneas para la revolución *)
class revolucion (padre : 'a Widget.widget) = object (self)
   (** Barra de estado *)
   val mutable mensajes = None

   (** Indica si se debe dibujar el elástico *)
   val mutable elastico = false

   (** Indica si se ha terminado de dibujar *)
   val mutable terminado = false

   (** Coordenada del punto actual *)
   val mutable x = 0.0
   val mutable y = 0.0

   (** Lista de puntos ya fijados *)
   val mutable seccion : Gl.point2 list = []

   (** Muestra un mensaje por la barra de estado *)
   method private mostrar msj =
      match mensajes with
         None   -> ()
       | Some l -> Label.configure ~text:("  " ^ msj) l

   (** Dibuja el editor de revolución *)
   method private dibujar_revolucion editor () =
      let w, h = Winfo.width editor, Winfo.height editor in
      GlClear.color Defs.escena_fondo_color;
      GlClear.clear [`color];
      Gl.enable `line_smooth;
      Gl.disable `lighting;
      Gl.disable `depth_test;
      GlDraw.shade_model `smooth;
      GlDraw.viewport ~x:0 ~y:0 ~w ~h;

      GlMat.mode `modelview;
      GlMat.push ();
      GlMat.load_identity ();
      GlMat.mode `projection;
      GlMat.push ();
      GlMat.load_identity ();
      let aspecto = (float w) /. (float h) in
      GlMat.ortho ~x:(-1.0 *. aspecto, 1.0 *. aspecto) ~y:(-1.0, 1.0) ~z:(-1.0, 1.0);

      (* dibuja el grid *)
      GlDraw.color Defs.escena_grid_color;
      GlDraw.line_width 1.0;
      GlDraw.begins `lines;
         (* horizontales *)
         GlDraw.vertex3 (-1.0 *. aspecto, -0.5, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto, -0.5, 0.0);
         GlDraw.vertex3 (-1.0 *. aspecto,  0.5, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto,  0.5, 0.0);
         (* verticales *)
         let ct  = ref 0.0
         and xd = 1.0 *. aspecto
         in
         while !ct <= xd do
            GlDraw.vertex3 (-.(!ct), -1.0, 0.0);
            GlDraw.vertex3 (-.(!ct),  1.0, 0.0);
            GlDraw.vertex3 (   !ct, -1.0, 0.0);
            GlDraw.vertex3 (   !ct,  1.0, 0.0);
            ct := !ct +. 0.5
         done;
      GlDraw.ends ();
      GlDraw.line_width 2.0;
      GlDraw.begins `lines;
         (* eje OX *)
         GlDraw.vertex3 (-1.0 *. aspecto,  0.0, 0.0);
         GlDraw.vertex3 ( 1.0 *. aspecto,  0.0, 0.0);
         (* eje OY *)
         GlDraw.vertex3 ( 0.0, -1.0, 0.0);
         GlDraw.vertex3 ( 0.0,  1.0, 0.0);
      GlDraw.ends ();

      (* dibuja las líneas *)
      GlDraw.line_width 1.0;
      GlDraw.color (1.0, 1.0, 0.0);
      if not terminado then begin
         GlDraw.begins `line_strip;
         List.iter (fun (x, y) -> GlDraw.vertex3 (x, y, 0.0)) seccion;
         (* dibuja la línea elástica *)
         if elastico then begin
               GlDraw.color (1.0, 1.0, 1.0);
               GlDraw.vertex3 (x, y, 0.0)
         end;
         GlDraw.ends ()
      end else begin
         GlDraw.begins `line_loop;
         List.iter (fun (x, y) -> GlDraw.vertex3 (x, y, 0.0)) seccion;
         GlDraw.ends ()
      end;
      (* dibuja la numeración de los puntos *)
      GlDraw.color Defs.camara_nombre_color;
      let base = Togl.load_bitmap_font editor ~font:`Helvetica_18 in
      for i = 0 to List.length seccion - 1 do
         let u, v = List.nth seccion i in
         GlPix.raster_pos ~x:(u) ~y:v ();
         GlList.call_lists ~base (`byte (string_of_int (i + 1)));
      done;
      Togl.unload_bitmap_font editor ~base:base;

      Gl.flush ();
      Togl.swap_buffers editor

   (** Función para establecer el registro como oyente del entorno *)
   method private escuchar_entorno mat_l
         (objetos, materiales, luces : string list * string list * string list) =
      (* establece los materiales *)
      Listbox.delete mat_l ~first:(`Num 0) ~last:`End;
      Listbox.insert mat_l ~index:`End ~texts:["Imprimación"];
      Listbox.insert mat_l
         ~index:`End
         ~texts:(List.map
            (fun s -> let w = Defs.herramientas_listas_columnas in
               (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
            materiales);
      Listbox.activate mat_l ~index:(`Num 0)

   (** Muestra la ventana del editor de revolución *)
   initializer
      (* ventana del editor *)
      let ventana = Toplevel.create padre in
      (* título de la ventana *)
      Wm.title_set ventana "[Revolución sobre Y] Mini-editor de polilíneas::Gráficos por Computador";

      (* controles *)
      let radio_t  = Textvariable.create () in Textvariable.set radio_t "1.0";
      let grado_t  = Textvariable.create () in Textvariable.set grado_t "360.0";
      let merid_t  = Textvariable.create () in Textvariable.set merid_t "32";
      let datos    = Frame.create ventana in
      let grado_f  = Frame.create datos in
      let merid_f  = Frame.create datos in
      let material = Frame.create datos in
      let radio_l  = Label.create ~text:"  Radio:" datos
      and radio_e  = Entry.create ~justify:`Right ~width:5 ~textvariable:radio_t datos
      and grado_l  = Label.create ~text:"  Grados:" grado_f
      and grado_e  = Entry.create ~justify:`Right ~width:5 ~textvariable:grado_t grado_f
      and merid_l  = Label.create ~text:"  Meridianos:" merid_f
      and merid_e  = Entry.create ~justify:`Right ~width:5 ~textvariable:merid_t merid_f
      and material_l    = Label.create ~text:"Material:" ~relief:`Flat material
      and material_lst  = Listbox.create ~height:1 ~width:Defs.herramientas_listas_columnas material
      in
      let aceptar  = Button.create
         ~text:"Aceptar"
         ~underline:0
         ~command:(fun () ->
            if terminado or (List.length seccion >= 3) then begin
               let seccion = if not terminado then seccion else List.append seccion [List.hd seccion] in
               try
                  let radio = float_of_string (Textvariable.get radio_t)
                  and merid = int_of_string   (Textvariable.get merid_t)
                  and grado = float_of_string (Textvariable.get grado_t)
                  in
                  match (Listbox.index material_lst ~index:`Active) with
                     `Num i -> let xml = 
                           if i = 0 then
                              Revolucion.get_xml seccion radio grado merid
                           else
                              let m = Entorno.get_material (i - 1) in
                              Revolucion.get_xml ~m seccion radio grado merid
                        in
                        try
                           Escena.add_obj (new Revolucion.revolucion xml :> Dibujable.dibujable);
                           Tk.destroy ventana
                        with
                           Invalid_argument _ -> self # mostrar
                              "El radio, los grados y el número de meridianos deben ser positivos y distintos de cero"
               with
                  Failure "float_of_string" -> self # mostrar "El radio y los grados deberían ser números ;-)"
                | Failure "int_of_string"   -> self # mostrar "El número de meridianos debería ser un número ;-)"
            end else
               self # mostrar "Debe haber al menos 3 vértices para completar la edición")
         datos
      and cancelar = Button.create
         ~text:"Cancelar"
         ~underline:0
         ~command:(fun () -> Tk.destroy ventana)
         datos
      in

      (* establece la lista de materiales *)
      Listbox.insert material_lst ~index:`End ~texts:("Imprimación" ::
         (List.map (fun s -> let w = Defs.herramientas_listas_columnas in
            (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
            (Entorno.get_materiales ())));
      Listbox.activate material_lst ~index:(`Num 0);
      (* registra el editor como oyente del entorno *)
      Entorno.registrar_oyente (self # escuchar_entorno material_lst);
      (* evento *)
      Tk.bind material_lst ~events:[`Motion] ~action:(fun _ -> Focus.set material_lst);

      (* vista del editor *)
      let editor_panel = Frame.create
         ~relief:`Groove
         ~borderwidth:5
         ventana
      in
      let editor = Togl.create editor_panel
         ~width: ((fst Defs.vista_dims) * 2)
         ~height:((snd Defs.vista_dims) * 2)
         ~rgba:true
         ~depth:true
         ~double:true
      in
      (* habilita el elástico *)
      Tk.bind editor ~events:[`Enter]
      ~action:(fun _ -> elastico <- true;
      Togl.render editor);
      (* deshabilita el elástico *)
      Tk.bind editor ~events:[`Leave]
         ~action:(fun _ -> elastico <- false;
         Togl.render editor);
      (* añade un nuevo vértice *)
      Tk.bind editor ~events:[`ButtonPressDetail 1]
         ~fields:[`MouseX; `MouseY]
         ~action:(fun ev ->
            if terminado = false then begin
               let m_x, m_y = ev.Tk.ev_MouseX ,(Winfo.height editor) - ev.Tk.ev_MouseY in
               let x', y', _ = GluMat.unproject (float m_x, float m_y, 1.0) in
               x <- x'; y <- y';
               (match seccion with
                  [] -> let p = Printf.sprintf "(%.2f, %.2f)" x' y' in
                  self # mostrar ("Primer punto establecido en (u,v) = " ^ p)
                | a :: [] -> let u, v = a in let p = Printf.sprintf "(%.2f, %.2f)" u v in
                  self # mostrar ("Punto establecido en (u,v) = " ^ p)
                | l -> let u, v = List.hd (List.rev l) in let p = Printf.sprintf "(%.2f, %.2f)" u v in
                  self # mostrar ("Punto establecido en (u,v) = " ^ p));
               seccion <- List.append seccion [(x', y')];
               Togl.render editor
            end else
               self # mostrar "No se puede editar, ya se había terminado de introducir los puntos");
      (* elimina el vértice anterior *)
      Tk.bind editor ~events:[`ButtonPressDetail 2]
         ~action:(fun _ ->
            if not terminado then
               try
                  seccion <- List.rev (List.tl (List.rev seccion));
                  self # mostrar "Último punto eliminado";
               with
                  Failure _ -> self # mostrar "No hay puntos para eliminar"
            else
               self # mostrar "No se puede editar, ya se había terminado de introducir los puntos";
            Togl.render editor);
      (* termina la edición *)
      Tk.bind editor ~events:[`ButtonPressDetail 3]
         ~action:(fun _ ->
            if List.length seccion < 2 then
               self # mostrar "Debe haber al menos 2 vértices para completar la edición"
            else begin
               terminado <- true;
               self # mostrar "Edición completada. Seleccionar las propiedades y el material y pulsar el botón Aceptar";
               Togl.render editor
            end);
      (* actualiza la posición del ratón *)
      Tk.bind editor ~events:[`Motion]
      ~fields:[`MouseX; `MouseY]
      ~action:(fun ev -> let m_x, m_y = ev.Tk.ev_MouseX ,(Winfo.height editor) - ev.Tk.ev_MouseY in
         let x', y', _ = GluMat.unproject (float m_x, float m_y, 1.0) in
         x <- x'; y <- y';
         if not terminado then
            let p = Printf.sprintf "(%.2f, %.2f)" x' y' in
            if seccion <> [] then
               self # mostrar ("<Mouse1> para posicionar el siguiente punto en P(u,v) = " ^ p)
            else
               self # mostrar ("<Mouse1> para posicionar el punto de origen en P(u,v) = " ^ p);
         Togl.render editor);
      (* dibuja el editor de revolución *)
      Togl.display_func editor ~cb:(self # dibujar_revolucion editor);

      (* barra de estado *)
      let mensajes_l = Label.create ~text:"  <Mouse1> para posicionar el punto de origen" ~anchor:`W ~relief:`Ridge ventana in
      mensajes <- Some mensajes_l;

      (* empaqueta todos los widgets *)
      Tk.pack ~side:`Top      ~fill:`X              [datos];
      Tk.pack ~side:`Top   ~fill:`Both ~expand:true [editor_panel];
      Tk.pack              ~fill:`Both ~expand:true [editor];
      Tk.pack ~side:`Top   ~fill:`X                 [mensajes_l];
      Tk.pack ~side:`Left                           [Tk.coe radio_l; Tk.coe radio_e];
      Tk.pack ~side:`Left                           [grado_f];
      Tk.pack ~side:`Left                           [Tk.coe grado_l; Tk.coe grado_e];      
      Tk.pack ~side:`Left                           [merid_f];
      Tk.pack ~side:`Left                           [Tk.coe merid_l; Tk.coe merid_e];
      Tk.pack ~side:`Left ~padx:15                  [material];
      Tk.pack ~side:`Left                           [Tk.coe material_l; Tk.coe material_lst];
      Tk.pack ~side:`Right                          [cancelar; aceptar];

      (** Establece el tamaño mínimo de la ventana *)
      Tkwait.visibility ventana;
      Wm.minsize_set ventana ~width:(Winfo.width  ventana) ~height:(Winfo.height ventana)
end
