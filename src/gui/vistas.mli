(** Panel de vistas de la aplicación *)

type w
type widget = w Widget.widget
(** El tipo de los {i widgets} de vistas. *)

type vista = ArrIzq | ArrDer | AbaIzq | AbaDer
(** Los tipos de vistas soportados. *)

val crear : 'a Widget.widget -> widget
(** [Vistas.crear padre] retorna un {i widget} para las vistas
    cuyo padre es [padre]. *)
