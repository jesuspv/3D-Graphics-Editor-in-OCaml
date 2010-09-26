(** Inicia la aplicación *)

(** [Constructor.main] punto de entrada a la aplicación *)
let main () =
   if !Sys.interactive then
      ()
   else begin
      Ventana.crear ();
      exit 0
   end

let _ = main ()
