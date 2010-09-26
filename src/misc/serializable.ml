(** Interfaz para los objetos serializables en XML *)
class virtual serializable = object
   (** Devuelve el nombre de la clase *)
   method virtual clase : string

   (** Establece los par�metros del objeto a partir de su definici�n en XML *)
   method virtual from_xml : Xml.xml -> unit

   (** Devuelve la definici�n del objeto en formato XML *)
   method virtual to_xml : Xml.xml
end
