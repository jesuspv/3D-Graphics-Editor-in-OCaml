(* Interfaz [menus.mli] *)
type w = Widget.any
type widget = w Widget.widget

(* Abre una ventana para cargar archivos de escena *)
let cargar_archivo (padre : 'a Widget.widget) () : unit =
   let archivo = Tk.getOpenFile ~defaultextension:".xml"
      ~filetypes:
         [{Tk.typename = "Escenas"; Tk.extensions = [".xml"]; Tk.mactypes = []};
          {Tk.typename = "Configuraciones"; Tk.extensions = [".config.xml"]; Tk.mactypes = []};
          {Tk.typename = "Todos los archivos"; Tk.extensions = [".*"]; Tk.mactypes = []}]
      ~initialdir:"test"
      ~initialfile:(Filename.basename !Defs.escena_archivo)
      ~parent:padre
      ~title:"Abrir archivo de escena" () in
   if archivo <> "" then
      try
         ignore (Filename.chop_suffix archivo Defs.config_archivo_ext);
         Entorno.cargar archivo
      with
         Invalid_argument _ -> Escena.cargar archivo

(* Abre una ventana para guardar archivos de escena *)
let guardar_archivo ?(preguntar = true) (padre : 'a Widget.widget) () : unit =
   let archivo =
      if preguntar or (!Defs.escena_archivo = "") then
         Tk.getSaveFile ~defaultextension:".xml"
            ~filetypes:
               [{Tk.typename = "Escenas"; Tk.extensions = [".xml"]; Tk.mactypes = []};
                {Tk.typename = "Configuraciones"; Tk.extensions = [".config.xml"]; Tk.mactypes = []};
                {Tk.typename = "Todos los archivos"; Tk.extensions = [".*"]; Tk.mactypes = []}]
            ~initialdir:"test"
            ~initialfile:(Filename.basename !Defs.escena_archivo)
            ~parent:padre
            ~title:"Guardar archivo de escena" ()
      else
         !Defs.escena_archivo
   in
   if archivo <> "" then
      Estado.mostrar (Escena.guardar archivo)

(* Crea el menú {b Archivo}. *)
let crear_menu_archivo (barra_menus : 'a Widget.widget) : unit =
   let boton_archivo = Menubutton.create ~text:"Archivo" ~underline:0 barra_menus in
   let menu_archivo  = Menu.create boton_archivo in

   Menu.add_command ~label:"Nuevo"
      ~underline:0
      ~command:(fun () -> Estado.mostrar (Escena.nueva ()))
      menu_archivo;
   Menu.add_command ~label:"Abrir..."
      ~underline:0
      ~command:(cargar_archivo barra_menus)
      menu_archivo;
   Menu.add_separator menu_archivo;
   Menu.add_command ~label:"Guardar"
      ~underline:0
      ~command:(guardar_archivo ~preguntar:false barra_menus)
      menu_archivo;
   Menu.add_command ~label:"Guardar como..."
      ~underline:8
      ~command:(guardar_archivo barra_menus)
      menu_archivo;
   Menu.add_separator menu_archivo;
   Menu.add_command ~label:"Salir"
      ~underline:0
      ~command:(fun () -> Tk.closeTk (); exit 0)
      menu_archivo;
   Menu.add_command ~label:"Guardar y salir"
      ~underline:1
      ~command:(fun () -> guardar_archivo ~preguntar:false barra_menus ();
         Tk.closeTk (); exit 0)
      menu_archivo;

   Menubutton.configure ~menu:menu_archivo boton_archivo;

   Tk.pack ~side:`Left [boton_archivo]

(* Crea el menú {b Editar}. *)
let crear_menu_editar (barra_menus : 'a Widget.widget) : unit =
   let boton_editar = Menubutton.create ~text:"Editar" ~underline:1 barra_menus in
   let menu_editar  = Menu.create boton_editar in

   Menu.add_command ~label:"Extrusión ..."
      ~underline:0
      ~command:(fun () -> let editor = new Editor.extrusion barra_menus in ignore editor)
      menu_editar;
   Menu.add_command ~label:"Revolución ..."
      ~underline:2
      ~command:(fun () -> let editor = new Editor.revolucion barra_menus in ignore editor)
      menu_editar;

   Menu.add_separator menu_editar;

   let efectos = Textvariable.create () in
   Textvariable.set efectos
      (if Escena.reflejos_activos () then "reflejos"
      else if Escena.sombras_activas () then "sombras"
      else "off");

   Menu.add_checkbutton ~label:"Reflejos sobre XZ"
      ~underline:0
      ~command:(fun () ->
         if Textvariable.get efectos = "reflejos" then begin
            Escena.reflejos_activar ();
            Escena.sombras_desactivar ();
         end else
            Escena.reflejos_desactivar ())
      ~onvalue:"reflejos"
      ~offvalue:"off"
      ~variable:efectos
      menu_editar;

   Menu.add_checkbutton ~label:"Sombras sobre XZ"
      ~underline:0
      ~command:(fun () ->
         if Textvariable.get efectos = "sombras" then begin
            Escena.sombras_activar ();
            Escena.reflejos_desactivar ();
         end else
            Escena.sombras_desactivar ())
      ~onvalue:"sombras"
      ~offvalue:"off"
      ~variable:efectos
      menu_editar;

   Menubutton.configure ~menu:menu_editar boton_editar;

   Tk.pack ~side:`Left [boton_editar]

(* Crea el menú {b Ayuda}. *)
let crear_menu_ayuda (barra_menus : 'a Widget.widget) : unit =
   let boton_ayuda = Menubutton.create ~text:"Ayuda" ~underline:1 barra_menus in
   let menu_ayuda  = Menu.create boton_ayuda in

   let msg0 =
      "La traslación y la rotación se han implementado bajo el sistema de referencia global.\n\n" ^
      "El escalado se efectúa en el sistema de referencia local.\n\n" ^
      "Adicionalmente en las transformaciones puede pulsar <Mouse1>+<Shift> al seleccionar el objeto o luz para duplicarlo."
   in
   Menu.add_command ~label:"Transformaciones"
      ~underline:0
      ~command:(fun () ->
         ignore (Tk.messageBox
            ~icon:`Info
            ~message:msg0
            ~title:"Transformaciones"
            ~typ:`Ok
            ()))
      menu_ayuda;

   let msg1 =
      "Sobre cualquier vista se puede pulsar <Mouse2> para ampliarla y hacer que ocupe todo el espacio de visualización.\n\n" ^
      "Una vez ampliada, la vista podrá ser reducida a su tamaño original volviendo a pulsar <Mouse2> sobre el espacio de visualización.\n\n" ^
      "En cada vista, al mantener pulsado <Mouse3> sobre ella aparecerá el menú correspondiente de configuración de las opciones de visualización."
   in
   Menu.add_command ~label:"Visualización"
      ~underline:0
      ~command:(fun () ->
         ignore (Tk.messageBox
            ~icon:`Info
            ~message:msg1
            ~title:"Visualización"
            ~typ:`Ok
            ()))
      menu_ayuda;

   let msg2 =
      "Los vértices de las secciones deberán darse en sentido antihorario.\n\n" ^
      "Los vértices se definen con <Mouse1>.\n\n" ^
      "Para eliminar el último vértice se pulsará <Mouse2>.\n\n" ^
      "Las secciones pueden ser abiertas o cerradas. Pulsando <Mouse3> se cerrará automáticamente la sección.\n\n" ^
      "Las secciones deberán estar centradas en el origen y ser convexas para la extrusión.\n\n" ^
      "Al revolucionar, un radio de R deja el eje de revolución en u=-R"
   in
   Menu.add_command ~label:"Mini-editor"
      ~underline:0
      ~command:(fun () ->
         ignore (Tk.messageBox
            ~icon:`Info
            ~message:msg2
            ~title:"Mini-editor"
            ~typ:`Ok
            ()))
      menu_ayuda;

   Menu.add_separator menu_ayuda;

   let msg3 =
      "La configuración (qué objetos se pueden crear, con qué materiales y qué luces) se define a partir del archivo de configuración '" ^ Defs.config_archivo ^ "'.\n\n" ^
      "Este archivo se busca en las siguientes localizaciones:\n\n" ^
      " 1) '$" ^ Defs.variable_entorno  ^ "/etc'\n" ^
      " 2) './etc'\n" ^
      " 3) '.'\n" ^
      " 4) Directorio del ejecutable\n" ^
      " 5) Directorio '../etc' desde 4)\n"
   in
   Menu.add_command ~label:"Configuración"
      ~underline:0
      ~command:(fun () ->
         ignore (Tk.messageBox
            ~icon:`Info
            ~message:msg3
            ~title:"Configuración"
            ~typ:`Ok
            ()))
      menu_ayuda;

   Menu.add_separator menu_ayuda;

   let msg4 =
      "Gráficos por Computador\n" ^
      "     CURSO 2003-04\n\n" ^
      "Autores:\n" ^
      "Jesús Pardillo Vela\n" ^
      "Arturo Bernal Mayordomo"
   in
   Menu.add_command ~label:"Acerca de..."
      ~underline:0
      ~command:(fun () ->
         ignore (Tk.messageBox
            ~icon:`Info
            ~message:msg4
            ~title:"Acerca de..."
            ~typ:`Ok
            ()))
      menu_ayuda;

   Menubutton.configure ~menu:menu_ayuda   boton_ayuda;

   Tk.pack ~side:`Right [boton_ayuda]

(* Crea el menu con las herramientas *)
let crear_herramientas (barra_herramientas : 'a Widget.widget) : unit =
   let relief = `Ridge in
   (* Panel para las transformaciones sobre las cámaras *)
   let camaras = Frame.create ~borderwidth:1 ~relief barra_herramientas
   (* Panel para las transformaciones sobre los objetos *)
   and objetos = Frame.create ~borderwidth:1 ~relief barra_herramientas
   (* Panel para los ejes de las transformaciones *)
   and ejes    = Frame.create ~borderwidth:1 ~relief barra_herramientas
   in
   let padx, pady = 1, 1 in
   (* Botones para las transformaciones sobre las cámaras *)
   let cams = Label.create ~pady
      ~text:" cámaras: "
      ~relief:`Flat
      barra_herramientas
   and situar = Button.create ~padx ~pady
      ~text:"situar"
      ~relief:`Sunken (* activado por defecto *)
      camaras
   and girar = Button.create ~padx ~pady
      ~text:"girar"
      ~relief:`Raised
      camaras
   and zoom = Button.create ~padx ~pady
      ~text:"zoom"
      ~relief:`Raised
      camaras
   (* Botones para las transformaciones sobre los objetos *)
   and objs = Label.create ~pady
      ~text:" objetos: "
      ~relief:`Flat
      barra_herramientas
   and mover = Button.create ~padx ~pady
      ~text:"mover"
      ~relief:`Raised
      objetos
   and rotar = Button.create ~padx ~pady
      ~text:"rotar"
      ~relief:`Raised
      objetos
   and escalar = Button.create ~padx ~pady
      ~text:"escalar"
      ~relief:`Raised
      objetos
   and borrar = Button.create ~padx ~pady
      ~text:"borrar"
      ~relief:`Raised
      objetos
   (* Botones para los ejes de las transformaciones *)
   and ejesxyz = Label.create ~pady
      ~text:" ejes: "
      ~relief:`Flat
      barra_herramientas
   and ejex = Button.create ~padx ~pady
      ~text:" x "
      ~relief:`Raised
      ejes
   and ejey = Button.create ~padx ~pady
      ~text:" y "
      ~relief:`Raised
      ejes
   and ejez = Button.create ~padx ~pady
      ~text:" z "
      ~relief:`Raised
      ejes
   and ejexy = Button.create ~padx ~pady
      ~text:"xy"
      ~relief:`Raised
      ejes
   and ejeyz = Button.create ~padx ~pady
      ~text:"yz"
      ~relief:`Raised
      ejes
   and ejexz = Button.create ~padx ~pady
      ~text:"xz"
      ~relief:`Sunken
      ejes
   in

   (* establece los eventos para los tipos de transformación *)
   let botones = [
      situar,  `Situar;
      girar,   `Girar;
      zoom,    `Zoom;
      mover,   `Mover;
      rotar,   `Rotar;
      escalar, `Escalar;
      borrar,  `Borrar]
   in
   let set_estado boton_on =
      Button.configure ~relief:`Sunken boton_on;
      List.iter
         (Button.configure ~relief:`Raised)
         (List.filter (fun b -> boton_on <> b) (List.map fst botones))
   in
   List.iter
      (fun (b, t) -> Button.configure
         ~command:(fun () ->
            set_estado b;
            (match t with
               `Situar  -> Estado.mostrar
                "Situar la cámara en el eje seleccionado (coordenadas globales)"
             | `Girar   -> Estado.mostrar
                "Girar la cámara en el eje seleccionado (coordenadas globales)"
             | `Zoom    -> Estado.mostrar
                "Pulsar <Mouse1> y mover el ratón arriba/abajo para ampliar/reducir el zoom"
             | `Mover   -> Estado.mostrar
                "Pincha sobre el objeto y mover en el eje seleccionado (coordenadas globales)"
             | `Rotar   -> Estado.mostrar
                "Pincha sobre el objeto y rotar en el eje seleccionado (coordenadas globales)"
             | `Escalar -> Estado.mostrar
                "Pincha sobre el objeto y escalar en el eje seleccionado (coordenadas relativas)"
             | `Borrar  -> Estado.mostrar
                "Pincha sobre el objeto para eliminar");
            Escena.transformacion := t)
         b)
      botones;
   (* establece los eventos para los ejes de transformación *)
   let botones = [ejex, `X; ejey, `Y; ejez, `Z; ejexy, `XY; ejeyz, `YZ; ejexz, `XZ] in
   let set_estado boton_on =
      Button.configure ~relief:`Sunken boton_on;
      List.iter
         (Button.configure ~relief:`Raised)
         (List.filter (fun b -> boton_on <> b) (List.map fst botones))
   in
   List.iter
      (fun (b, e) -> Button.configure
         ~command:(fun () ->
            set_estado b;
            (match e with
               `X -> Estado.mostrar
                ("Pulsa <Mouse1> mientras mueves el ratón arriba/abajo para +/- X")
             | `Y -> Estado.mostrar
                ("Pulsa <Mouse1> mientras mueves el ratón arriba/abajo para +/- Y")
             | `Z -> Estado.mostrar
                ("Pulsa <Mouse1> mientras mueves el ratón arriba/abajo para +/- Z")
             | `XY -> Estado.mostrar
                ("Pulsa <Mouse1> y desplaza horizontal/verticalmente para X/Y");
             | `YZ -> Estado.mostrar
                ("Pulsa <Mouse1> y desplaza horizontal/verticalmente para Z/Y")
             | `XZ -> Estado.mostrar
                ("Pulsa <Mouse1> y desplaza horizontal/verticalmente para X/Z"));
            Escena.eje  := e)
         b)
      botones;

   (* Coloca los paneles *)
   Tk.pack ~side:`Left [situar; girar; zoom];
   Tk.pack ~side:`Left [mover;  rotar; escalar; borrar];
   Tk.pack ~side:`Left [ejex; ejey; ejez; ejexy; ejeyz; ejexz];
   Tk.pack ~fill:`X ~padx:2 ~side:`Left [Tk.coe cams; Tk.coe camaras];
   Tk.pack ~fill:`X ~padx:2 ~side:`Left [Tk.coe objs; Tk.coe objetos];
   Tk.pack ~fill:`X ~padx:2 ~side:`Right [Tk.coe ejes; Tk.coe ejesxyz]

(* Interfaz [menus.mli] *)
let crear (padre : 'a Widget.widget) : widget =
   let menus = Frame.create ~relief:`Flat padre in
   (* barra de menus *)
   let barra_menus = Frame.create ~borderwidth:1 ~relief:`Raised menus
   (* barra de herramientas *)
   and barra_herramientas = Frame.create ~borderwidth:1 ~relief:`Raised menus
   in

   (* menus *)
   crear_menu_archivo barra_menus;
   crear_menu_editar  barra_menus;
   crear_menu_ayuda   barra_menus;
   crear_herramientas barra_herramientas;

   Tk.pack ~fill:`X [barra_menus];
   Tk.pack ~fill:`X [barra_herramientas];

   Widget.coe menus

(* Interfaz [menus.mli] *)
let cargar_escena () : unit =
   if Array.length Sys.argv > 1 then
      Escena.cargar Sys.argv.(1)
