(** Configuración de la escena *)

(* Definiciones XML de las geometrias predefinidos *)
let geometria = ref []

(* Definiciones XML de los materiales predefinidos *)
let materiales = ref []

(* Definiciones XML de las luces predefinidas *)
let iluminacion = ref []

(* Oyentes para los cambios/actualizaciones en el entorno *)
let oyentes = ref []

(* Carga la geometria desde una definición XML *)
let cargar_geometria (xml: Xml.xml) : unit =
   if Xml.tag xml = "geometria" then
      geometria := Xml.children xml
   else
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <geometria>")

(* Guarda la geometria en formato XML *)
let guardar_geometria () : Xml.xml =
   (* tontería para pasar a minúsculas los nombres de los atributos *)
   let min_atribs  atribs = List.map (fun (n, v) -> (String.lowercase n, v)) atribs in
   let rec min_xml xml    =
      match xml with
         Xml.Element (n, a, h) -> Xml.Element (n, min_atribs a, List.map min_xml h)
       | Xml.PCData  t         -> Xml.PCData t
   in
   Xml.Element ("geometria", [], List.map min_xml !geometria)

(* Carga los materiales desde una definición XML *)
let cargar_materiales (xml: Xml.xml) : unit =
   if Xml.tag xml = "materiales" then
      materiales := Xml.children xml
   else
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <materiales>")

(* Guarda los materiales en formato XML *)
let guardar_materiales () : Xml.xml =
   (* parafernalia para pasar a minúsculas los nombres de los atributos *)
   let min_atribs  atribs = List.map (fun (n, v) -> (String.lowercase n, v)) atribs in
   let rec min_xml xml    =
      match xml with
         Xml.Element (n, a, h) -> Xml.Element (n, min_atribs a, List.map min_xml h)
       | Xml.PCData  t         -> Xml.PCData t
   in
   Xml.Element ("materiales", [], List.map min_xml !materiales)

(* Carga las luces desde una definición XML *)
let cargar_iluminacion (xml: Xml.xml) : unit =
   if Xml.tag xml = "iluminacion" then
      iluminacion := Xml.children xml
   else
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <iluminacion>")

(* Guarda las luces en formato XML *)
let guardar_iluminacion () : Xml.xml =
   (* parafernalia para pasar a minúsculas los nombres de los atributos *)
   let min_atribs  atribs = List.map (fun (n, v) -> (String.lowercase n, v)) atribs in
   let rec min_xml xml    =
      match xml with
         Xml.Element (n, a, h) -> Xml.Element (n, min_atribs a, List.map min_xml h)
       | Xml.PCData  t         -> Xml.PCData t
   in
   Xml.Element ("iluminacion", [], List.map min_xml !iluminacion)

(* Actualiza los oyentes con los nuevos datos del entorno *)
let actualizar_oyentes (entorno : string list * string list * string list) : unit =
   (* actualiza los oyentes *)
   let pochos = ref [] in (* lista de oyentes que ya no oyen *)
   for i = 0 to List.length !oyentes - 1 do
      try
         (List.nth !oyentes i) entorno
      with
         (* el oyente ya no está con nosotros *)
         Protocol.TkError _ -> pochos := i :: !pochos
   done;
   (* nos deshacemos de los oyentes que pasaron a mejor vida *)
   let new_oyentes = ref [] in
   for j = 0 to List.length !oyentes - 1 do
      if not (List.mem j !pochos) then new_oyentes := List.append !new_oyentes [List.nth !oyentes j]
   done;
   oyentes := !new_oyentes

(* Interfaz [entorno.mli] *)
let reiniciar () : unit =
   geometria   := [];
   materiales  := [];
   iluminacion := [];
   actualizar_oyentes ([], [], [])

(* Interfaz [entorno.mli] *)
let cargar (archivo : string) : unit =
   let xml = Xml.parse_file archivo in
   if Xml.tag xml = "entorno" then begin
      let geo = List.nth (Xml.children xml) 0 (* geometria   *)
      and mat = List.nth (Xml.children xml) 1 (* materiales  *)
      and luc = List.nth (Xml.children xml) 2 (* iluminacion *)
      in
      cargar_geometria   geo;
      cargar_materiales  mat;
      cargar_iluminacion luc;
      (* actualiza a los oyentes *)
      let o = List.map (fun o -> Xml.attrib o "nombre") !geometria
      and m = List.map (fun m -> Xml.attrib m "nombre") !materiales
      and l = List.map (fun l -> Xml.attrib l "nombre") !iluminacion
      in
      actualizar_oyentes (o, m, l)
   end else
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <entorno>");
   Estado.mostrar ("Entorno cargado (Configuración: \"" ^ archivo ^ "\")")

(* Interfaz [entorno.mli] *)
let guardar () : Xml.xml =
   Xml.Element ("entorno", [], [guardar_geometria (); guardar_materiales (); guardar_iluminacion ()])

(* Interfaz [entorno.mli] *)
let registrar_oyente (oyente : string list * string list * string list -> unit) : unit =
   oyentes := List.append !oyentes [oyente]

(* Interfaz [entorno.mli] *)
let get_materiales () : string list =
   List.map (fun m -> Xml.attrib m "nombre") !materiales

(* Interfaz [entorno.mli] *)
let get_material (mat : int) : Xml.xml =
   List.nth !materiales mat

(* Interfaz [entorno.mli] *)
let crear_obj ~(obj : int) ?(mat = -1) () : Dibujable.dibujable =
   let xml_obj = List.nth !geometria obj
   and def_float = 1.0 and def_int = 1
   in
   let objeto = XmlMarshal.make_geo xml_obj
   in
   if mat <> -1 then begin
      let xml_mat = List.nth !materiales (mat - 1) in
      objeto # set_material (new Material.material ~xml:xml_mat ())
   end;
   objeto

(* Interfaz [entorno.mli] *)
let crear_luz (indice : int) : Luz.luz =
   let xml = List.nth !iluminacion indice in
   XmlMarshal.make_luz xml

(** Interfaz [entorno.mli] *)
let cargar_configuracion () =
   let config_ruta = ref None in

   let gc_home = try Sys.getenv Defs.variable_entorno with Not_found -> "" in
   let cwd     = Sys.getcwd () in
   let exepath = Filename.dirname Sys.executable_name in
   let relpath = Filename.concat (Filename.dirname (Filename.dirname Sys.executable_name)) "etc" in

   if Sys.file_exists (Filename.concat gc_home (Filename.concat "etc" Defs.config_archivo)) then
      config_ruta := Some (Filename.concat gc_home "etc")
   else if Sys.file_exists (Filename.concat cwd     Defs.config_archivo) then config_ruta := Some cwd
   else if Sys.file_exists (Filename.concat exepath Defs.config_archivo) then config_ruta := Some exepath
   else if Sys.file_exists (Filename.concat relpath Defs.config_archivo) then config_ruta := Some relpath
   else begin config_ruta := None;
      let msg = "Archivo de configuración '" ^ Defs.config_archivo ^ "' no encontrado; " ^
        "se ha buscado en los directorios:\n" ^
        "'" ^ gc_home ^ "'\n" ^
        "'" ^ cwd     ^ "'\n" ^
        "'" ^ exepath ^ "'\n" ^
        "'" ^ relpath ^ "'\n"
      in
      ignore (Tk.messageBox
         ~icon:`Warning
         ~message:msg
         ~title:"Archivo de configuración"
         ~typ:`Ok
         ());
      print_string (Filename.basename Sys.argv.(0) ^ ": " ^ msg);
      flush stdout
   end;

   match !config_ruta with
      None      -> ()
    | Some ruta -> cargar (Filename.concat ruta Defs.config_archivo)
