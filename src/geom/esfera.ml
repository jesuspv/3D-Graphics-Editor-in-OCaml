open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para las esferas *)
class esfera
      ~radio:     (init_radio      : float)
      ~meridianos:(init_meridianos : int  )
      ~paralelos: (init_paralelos  : int  ) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Radio *)
   val mutable radio = 1.0

   (** N�mero de meridianos *)
   val mutable meridianos = 1

   (** N�mero de paralelos *)
   val mutable paralelos  = 1

   (** Selector del radio *)
   method get_radio = radio
   (** Modificador del radio *)
   method set_radio radio' =
      if radio' > 0.0 then
         radio <- radio'
      else
         raise (Invalid_argument (self#clase ^ ":set_radio: radio no positivo"))

   (** Selector del n�mero de meridianos *)
   method get_meridianos = meridianos
   (** Modificador del n�mero de meridianos *)
   method set_meridianos meridianos' =
      if meridianos' > 0 then
         meridianos <- meridianos'
      else
         raise (Invalid_argument (self#clase ^ ":set_meridianos: n�mero de meridianos no positivo"))

   (** Selector del n�mero de paralelos *)
   method get_paralelos = paralelos
   (** Modificador del n�mero de paralelos *)
   method set_paralelos paralelos' =
      if paralelos' > 0 then
         paralelos <- paralelos'
      else
         raise (Invalid_argument (self#clase ^ ":set_paralelos: n�mero de paralelos no positivo"))

   (** Implementaci�n de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      if not sombra then material # pintar ~cara;
      let quad = GluQuadric.create () in
      GluQuadric.normals quad (match normales with `Poligono -> `flat | `Vertice -> `smooth);
      GluQuadric.sphere ~radius:radio ~slices:meridianos ~stacks:paralelos ~quad ();

   (** Implementaci�n de la interfaz [dibujable] *)
   method get_extremo = (max sx (max sy sz)) *. radio

   (** Implementaci�n de la interfaz [dibujable] *)
   method get_limites = sx *. radio, sy *. radio, sz *. radio

   (** Implementaci�n de la interfaz [serializable] *)
   method clase = "esfera"

   (** Implementaci�n de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self#clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml  "nombre");
         self  # set_radio      (float_of_string (Xml.attrib xml "radio"     ));
         self  # set_meridianos (int_of_string   (Xml.attrib xml "meridianos"));
         self  # set_paralelos  (int_of_string   (Xml.attrib xml "paralelos" ))
      end else
         raise (Invalid_argument (self#clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementaci�n de la interfaz [serializable] *)
   method to_xml = Xml.Element (self#clase,
      [
         "nombre",     nombre;
         "radio",      (Defs.float2xml radio     );
         "meridianos", (string_of_int  meridianos);
         "paralelos",  (string_of_int  paralelos )
      ],
      [material # to_xml; super # to_xml])

   (** Constructor del objeto *)
   initializer
      self # set_radio      init_radio;
      self # set_meridianos init_meridianos;
      self # set_paralelos  init_paralelos
end
