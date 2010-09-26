(** Panel de men�s de la aplicaci�n *)

type w
type widget = w Widget.widget
(** El tipo de los {i widgets} de men�s. *)

val crear : 'a Widget.widget -> widget
(** [Menus.crear padre] retorna un {i widget} para los men�s
    cuyo padre es [padre]. *)

val cargar_escena : unit -> unit
(** [Menus.cargar_escena] carga la escena que se le haya pasado
    a la aplicaci�n desde l�nea de comandos (si se le ha pasado
    alguna). *)
