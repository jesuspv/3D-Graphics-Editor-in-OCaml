(** Panel de menús de la aplicación *)

type w
type widget = w Widget.widget
(** El tipo de los {i widgets} de menús. *)

val crear : 'a Widget.widget -> widget
(** [Menus.crear padre] retorna un {i widget} para los menús
    cuyo padre es [padre]. *)

val cargar_escena : unit -> unit
(** [Menus.cargar_escena] carga la escena que se le haya pasado
    a la aplicación desde línea de comandos (si se le ha pasado
    alguna). *)
