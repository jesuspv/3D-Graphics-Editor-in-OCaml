(** Inicia la aplicaci�n *)

(** [Constructor.main] punto de entrada a la aplicaci�n *)
let main () =
   if !Sys.interactive then
      ()
   else begin
      Ventana.crear ();
      exit 0
   end

let _ = main ()
