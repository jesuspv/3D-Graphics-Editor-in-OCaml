(** Interfaz para los objetos serializables en XML *)
class virtual serializable = object
   (** Devuelve el nombre de la clase *)
   method virtual clase : string

   (** Establece los parámetros del objeto a partir de su definición en XML *)
   method virtual from_xml : Xml.xml -> unit

   (** Devuelve la definición del objeto en formato XML *)
   method virtual to_xml : Xml.xml
end
