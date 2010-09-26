(** Panel de mensajes de la aplicación *)

val crear : 'a Widget.widget -> Widget.any Widget.widget
(** [Mensajes.crear padre] retorna el {i widget} para los mensajes
    cuyo padre es [padre]. *)

val mostrar : string -> unit
(** [Mensajes.mostrar msj] muestra el mensaje [msj]. *)
