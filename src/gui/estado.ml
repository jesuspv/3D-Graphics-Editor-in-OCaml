
(* vista para los mensajes de estado *)
let estado = ref None

(* Interfaz [estado.mli] *)
let crear (padre : 'a Widget.widget) : Widget.any Widget.widget =
   let e = Label.create ~text:"  Entorno cargado" ~anchor:`W ~relief:`Ridge padre in
   estado := Some (e);
   Tk.coe e

(* Interfaz [estado.mli] *)
let mostrar (msj : string) : unit =
   match
      !estado
   with
      None -> print_string "estado:mostrar: El panel de estado no ha sido creado\n"
    | Some widget -> Label.configure ~text:("  " ^ msj) widget
