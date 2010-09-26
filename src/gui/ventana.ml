(** Ventana de la aplicación. *)

(** Centra la ventana en la pantalla. *)
let centrar (ventana : Widget.toplevel Widget.widget) : unit =
   Tkwait.visibility ventana;
   let p_ancho = Winfo.screenwidth  ventana
   and p_largo = Winfo.screenheight ventana
   and v_ancho = Winfo.width        ventana
   and v_largo = Winfo.height       ventana
   in
   let posicion_x = string_of_int ((p_ancho / 2) - (v_ancho / 2))
   and posicion_y = string_of_int ((p_largo / 2) - (v_largo / 2))
   in
   Wm.geometry_set ventana ("+" ^ posicion_x ^ "+" ^ posicion_y)

(** Crea y muestra la ventana principal *)
let crear () =
   (* ventana principal *)
   let ventana = Tk.openTk () in
   (* título de la ventana *)
   Wm.title_set ventana "OpenGL::Gráficos por Computador";
   (* crea el panel con los menús *)
   let menus = Menus.crear ventana in
   Tk.pack ~fill:`X [menus];

   (* marco para las vistas y el panel de comandos *)
   let marco_central = Frame.create ventana in
   Tk.pack ~side:`Top ~fill:`Both ~expand:true [marco_central];

   (* crea el panel con las vistas *)
   let vistas = Vistas.crear marco_central in
   Tk.pack ~side:`Left ~fill:`Both ~expand:true [vistas];

   (* crea el panel con las herramientas *)
   let herramientas = Herramientas.crear marco_central in
   Tk.pack ~side:`Top [herramientas];

   (* barra de estado *)
   let estado = Estado.crear ventana in
   Tk.pack ~fill:`X [estado];

   (* carga la configuración *)
   Entorno.cargar_configuracion ();

   (* carga la escena *)
   if Array.length Sys.argv <> 1 then
      Menus.cargar_escena ();

   (** Establece el tamaño mínimo de la ventana *)
   Tkwait.visibility ventana;
   Wm.minsize_set ventana ~width:(Winfo.width  ventana) ~height:(Winfo.height ventana);

   (* comienza el bucle de eventos *)
   Tk.mainLoop ()
