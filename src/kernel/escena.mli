(** Funcionalidades solicitables a la escena *)

val transformacion : [`Situar | `Girar | `Zoom | `Mover | `Rotar | `Escalar | `Borrar] ref
(** [Escena.transformacion] variable de estado indicando que tipo de
   transformación se llevará a cabo en la escena. *)

val eje : [`X | `Y | `Z | `XY | `YZ | `XZ] ref
(** [Escena.eje] variable de estado indicando el eje sobre el que se llevará
   a cabo la transformación. *)

val reflejos : bool ref
(** [Escena.reflejos] variable de estado indicando si los reflejos se deben de representar o no. *)

val nueva : unit -> string
(** [Escena.nueva ()] reinicia las estructuras de datos de la escena.
    @return Estado de la operación *)

val cargar : string -> unit
(** [Escena.cargar archivo] carga los datos de la escena a partir
    de la definición de escena en un archivo XML.
    @return Estado de la operación *)

val guardar : string -> string
(** [Escena.guardar archivo] guarda los datos de la escena en
    un archivo XML de definición de la escena.
    @return Estado de la operación *)

val registrar_vista : Togl.widget -> unit
(** [Escena.registrar_vista vista] registra la vista [vista] como vista
    que representa la escena. *)

val add_luz : Luz.luz -> unit
(** [Escena.add_luz luz] añade la luz [luz] a la lista de luces de la escena. *)

val add_obj : Dibujable.dibujable -> unit
(** [Escena.add_obj obj] añade el objeto [obj] a la lista de objetos de la escena. *)

val reflejos_activos : unit -> bool
(** [Escena.reflejos_activos] indica si los reflejos están siendo representados. *)

val reflejos_activar : unit -> unit
(** [Escena.reflejos_activar] activa la representación de los reflejos. *)

val reflejos_desactivar : unit -> unit
(** [Escena.reflejos_desactivar] desactiva la representación de los reflejos. *)

val sombras_activas : unit -> bool
(** [Escena.sombras_activas] indica si las sombras están siendo representadas. *)

val sombras_activar : unit -> unit
(** [Escena.sombras_activar] activa la representación de las sombras. *)

val sombras_desactivar : unit -> unit
(** [Escena.sombras_desactivar] desactiva la representación de las sombras. *)
