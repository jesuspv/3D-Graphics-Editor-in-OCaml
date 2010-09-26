(** Configuraci�n de la escena *)

val reiniciar : unit -> unit
(** [Entorno.reiniciar] reinicia los datos del entorno (no los oyentes). *)

val cargar : string -> unit
(** [Entorno.cargar archivo] Carga el entorno desde un archivo de configuraci�n. *)

val guardar : unit -> Xml.xml
(** [Entorno.guardar ()] guarda el entorno en formato XML. *)

val registrar_oyente : (string list * string list * string list -> unit) -> unit
(** [Entorno.registrar_oyente oyente] registra un oyente para los cambios
    que se produzcan actualizaciones del entorno. *)

val get_materiales : unit -> string list
(** [Entorno.get_materiales] obtiene la lista de nombres de los materiales. *)

val get_material : int -> Xml.xml
(** [Entorno.get_material mat] obtiene la definici�n XML del material de �ndice [mat]. *)

val crear_obj : obj:int -> ?mat:int -> unit -> Dibujable.dibujable
(** [Entorno.crear_obj_mat obj mat] genera el objeto correspondiente al �ndice [obj]
    con el material correspondiente al �ndice [mat] si [mat] es distinto de -1,
    en cuyo caso se selecciona el materia por defecto del objeto. *)

val crear_luz : int -> Luz.luz
(** [Entorno.crear_luz indice] genera la luz correspondiente al �ndice [indice]. *)

val cargar_configuracion : unit -> unit
(** [Entorno.cargar_configuracion] carga la configuraci�n desde un archivo de configuraci�n.

    El archivo se buscar� por orden en:
+ El subdirectorio {i etc} del directorio indicado por la variable de entorno [Defs.variable_entorno]
+ El subdirectorio {i etc} del directorio actual de trabajo
+ El directorio actual de trabajo
+ El directorio donde se encuentra el ejecutable
+ El directorio {i ../etc} desde el directorio donde se encuentra el ejecutable *)
