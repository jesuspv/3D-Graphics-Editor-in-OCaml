(** Suelo de la escena en el plano XZ *)

val set_display_list : int -> unit
(** [Suelo.set_display_list num] establece la display list para la vista de número [num]. *)

val call_display_list_alambrico : int -> unit
(** [Suelo.call_display_list_alambrico num] llama a la display list de
    la vista de número [num] para dibujar el suelo en modo alámbrico. *)

val call_display_list_solido : int -> unit
(** [Suelo.call_display_list_solido num] llama a la display list de
    la vista de número [num] para dibujar el suelo en modo sólido. *)
