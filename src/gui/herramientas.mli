(** Panel de herramientas de la aplicación *)

type w
type widget = w Widget.widget
(** El tipo de los {i widgets} de herramientas. *)

val crear : 'a Widget.widget -> widget
(** [Herramientas.crear padre] retorna un {i widget} para las herramientas
    cuyo padre es [padre]. *)
