type w = Widget.any
type widget = w Widget.widget

(* Etiqueta para el objeto seleccionado *)
let obj_actual = ref None

(* Lista de objetos *)
let lista_objetos = ref None

(* Etiqueta para el material seleccionado *)
let mat_actual = ref None

(* Panel con la lista de los materiales *)
let lista_materiales = ref None

(* Etiqueta para la luz seleccionada *)
let luz_actual = ref None

(* Panel con la lista de las luces *)
let lista_luces = ref None

(* Función para establecer el registro como oyente del entorno *)
let escuchar_entorno (objetos, materiales, luces : string list * string list * string list) : unit =
   let w = Defs.herramientas_listas_columnas in
   (* establece los objetos *)
   (match !lista_objetos, !obj_actual with
      Some l, Some s -> Listbox.delete l ~first:(`Num 0) ~last:`End;
       Listbox.insert l
          ~index:`End
          ~texts:(List.map
             (fun s -> (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
             objetos);
       Listbox.activate l ~index:(`Num 0);
       let o = (Listbox.get l ~index:`Active) in
       let a = (try String.sub o 0 12 with _ -> o) ^ (if String.length o > 12 then "..." else "")
       in
       Label.configure ~text:a s
    | _, _ -> ());

   (* establece los materiales *)
   (match !lista_materiales, !mat_actual with
      Some l, Some s -> Listbox.delete l ~first:(`Num 0) ~last:`End;
       Listbox.insert l ~index:`End ~texts:["Imprimación"];
       Listbox.insert l
          ~index:`End
          ~texts:(List.map
             (fun s -> (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
             materiales);
       Listbox.activate l ~index:(`Num 0);
       Listbox.activate l ~index:(`Num 0);
       let o = (Listbox.get l ~index:`Active) in
       let a = (try String.sub o 0 8 with _ -> o) ^ (if String.length o > 8 then "..." else "")
       in
       Label.configure ~text:a s
    | _, _ -> ());

   (* establece las luces *)
   (match !lista_luces, !luz_actual with
      Some l, Some s -> Listbox.delete l ~first:(`Num 0) ~last:`End;
       Listbox.insert l
          ~index:`End
          ~texts:(List.map
             (fun s -> (try String.sub s 0 w with _ -> s) ^ (if String.length s > w then "..." else ""))
             luces);
       Listbox.activate l ~index:(`Num 0);
       Listbox.activate l ~index:(`Num 0);
       let o = (Listbox.get l ~index:`Active) in
       let a = (try String.sub o 0 12 with _ -> o) ^ (if String.length o > 12 then "..." else "")
       in
       Label.configure ~text:a s
    | _, _ -> ())

(* Interfaz [herramientas.mli] *)
let crear (padre : 'a Widget.widget) : widget =
   (* marco para los comandos *)
   let panel = Frame.create padre in

   (* enlaza barra y panel *)
   let enlazar barra lista =
      Listbox  .configure ~yscrollcommand:(Scrollbar.set barra) lista;
      Scrollbar.configure ~command:(Listbox.yview lista) barra
   and ancho = Defs.herramientas_listas_columnas
   and alto  = Defs.herramientas_listas_filas
   in

   (* objetos *)
   let o_etiqueta  = Label.create ~text:"OBJETOS" ~anchor:`Center ~relief:`Groove panel in
   let o_seleccion = Frame.create panel in
   let o_tipo      = Label.create ~text:" Tipo:" ~anchor:`W ~relief:`Flat o_seleccion in
   let o_actual    = Label.create ~text:"" ~anchor:`W ~relief:`Flat o_seleccion in
   let o_panel     = Frame.create panel in
   let o_barra     = Scrollbar.create o_panel in
   let o_lista     = Listbox  .create ~height:alto ~width:ancho o_panel in
   Tk.bind
      ~events:[`Motion]
      ~action:(fun _ -> Focus.set o_lista;
         let o = (Listbox.get o_lista ~index:`Active) in
         let s = (try String.sub o 0 12 with _ -> o) ^ (if String.length o > 12 then "..." else "")
         in
         Label.configure ~text:s o_actual)
      o_lista;
   enlazar o_barra o_lista;
   lista_objetos := Some o_lista;
   obj_actual    := Some o_actual;

   (* materiales *)
   let m_seleccion = Frame.create panel in
   let m_material  = Label.create ~text:" Material:" ~anchor:`W ~relief:`Flat m_seleccion in
   let m_actual    = Label.create ~text:"Imprimación" ~anchor:`W ~relief:`Flat m_seleccion in
   let m_panel     = Frame.create panel in
   let m_barra     = Scrollbar.create m_panel in
   let m_lista     = Listbox  .create ~height:alto ~width:ancho m_panel in
   Listbox.insert m_lista ~index:`End ~texts:["Imprimación"];
   Tk.bind
      ~events:[`Motion]
      ~action:(fun _ -> Focus.set m_lista;
         let m = (Listbox.get m_lista ~index:`Active) in
         let s = (try String.sub m 0 8 with _ -> m) ^ (if String.length m > 8 then "..." else "")
         in
         Label.configure ~text:s m_actual)
      m_lista;
   enlazar m_barra m_lista;
   lista_materiales := Some m_lista;
   mat_actual       := Some m_actual;

   let o_crear = Button.create
      ~text:"crear"
      ~relief:`Flat
      ~command:(fun () ->
         try
            match
               (Listbox.index o_lista ~index:`Active),
               (Listbox.index m_lista ~index:`Active)
            with
               `Num obj, `Num mat ->
                  try
                     if mat = 0 then
                        Escena.add_obj (Entorno.crear_obj ~obj ())
                     else
                        Escena.add_obj (Entorno.crear_obj ~obj ~mat ())
                  with
                     Invalid_argument msg -> Estado.mostrar ("Se pasó un argumento no válido: " ^ msg)
         with
            Failure "nth" -> Estado.mostrar "Estaría bien si hubiera objetos donde elegir ;-)")
      panel
   in

   Tk.pack ~side:`Top  ~fill:`X    [o_etiqueta];
   Tk.pack ~side:`Top  ~fill:`X    [o_seleccion];
   Tk.pack ~side:`Left ~fill:`X    [o_tipo; o_actual];
   Tk.pack ~side:`Left ~fill:`Both [Tk.coe o_lista; Tk.coe o_barra];
   Tk.pack ~side:`Top  ~fill:`X    [o_panel];
   Tk.pack ~side:`Top  ~fill:`X    [m_seleccion];
   Tk.pack ~side:`Left ~fill:`X    [m_material; m_actual];
   Tk.pack ~side:`Left ~fill:`Both [Tk.coe m_lista; Tk.coe m_barra];
   Tk.pack ~side:`Top  ~fill:`X    [m_panel];
   Tk.pack ~side:`Top  ~fill:`X    [o_crear];

   (* fuentes de luz *)
   let l_etiqueta  = Label.create ~text:"LUCES" ~anchor:`Center ~relief:`Groove panel in
   let l_seleccion = Frame.create panel in
   let l_tipo      = Label.create ~text:" Tipo:" ~anchor:`W ~relief:`Flat l_seleccion in
   let l_actual    = Label.create ~text:"" ~anchor:`W ~relief:`Flat l_seleccion in
   let l_panel     = Frame.create panel in
   let l_barra     = Scrollbar.create l_panel in
   let l_lista     = Listbox  .create ~height:alto ~width:ancho l_panel in
   Tk.bind
      ~events:[`Motion]
      ~action:(fun _ -> Focus.set l_lista;
         let l = (Listbox.get l_lista ~index:`Active) in
         let s = (try String.sub l 0 12 with _ -> l) ^ (if String.length l > 12 then "..." else "")
         in
         Label.configure ~text:s l_actual)
      l_lista;
   lista_luces := Some l_lista;
   luz_actual  := Some l_actual;
   enlazar l_barra l_lista;

   let l_crear = Button.create
      ~text:"crear"
      ~relief:`Flat
      ~command:(fun () ->
         try
            match (Listbox.index l_lista ~index:`Active) with
               `Num i -> Escena.add_luz (Entorno.crear_luz i)
         with
            Failure "nth"        -> Estado.mostrar "Estaría bien si hubiera luces donde elegir ;-)"
          | Failure msg          -> Estado.mostrar ("No se pueden crear más luces: "   ^ msg)
          | Invalid_argument msg -> Estado.mostrar ("Se pasó un argumento no válido: " ^ msg))
      panel
   in

   Tk.pack ~side:`Top  ~fill:`X    [l_etiqueta];
   Tk.pack ~side:`Top  ~fill:`X    [l_seleccion];
   Tk.pack ~side:`Left ~fill:`X    [l_tipo; l_actual];
   Tk.pack ~side:`Left ~fill:`Both [Tk.coe l_lista; Tk.coe l_barra];
   Tk.pack ~side:`Top  ~fill:`X    [l_panel];
   Tk.pack ~side:`Top  ~fill:`X    [l_crear];

   (* registra las herramientas como oyente del entorno *)
   Entorno.registrar_oyente escuchar_entorno;

   Widget.coe panel
