open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los discos *)
class disco
      ~interior:  (init_interior   : float)
      ~exterior:  (init_exterior   : float)
      ~meridianos:(init_meridianos : int  )
      ~paralelos: (init_paralelos    : int  ) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Radio interior *)
   val mutable interior = 1.0

   (** Radio exterior *)
   val mutable exterior = 1.0

   (** Número de meridianos *)
   val mutable meridianos = 1

   (** Número de paralelos *)
   val mutable paralelos  = 1

   (** Selector del radio interior *)
   method get_interior = interior
   (** Modificador del radio interior *)
   method set_interior interior' =
      if interior' > 0.0 then
         interior <- interior'
      else
         raise (Invalid_argument (self#clase ^ ":set_interior: radio interior no positivo"))

   (** Selector del radio exterior *)
   method get_interior = exterior
   (** Modificador del radio exterior *)
   method set_exterior exterior' =
      if exterior' > 0.0 then
         exterior <- exterior'
      else
         raise (Invalid_argument (self#clase ^ ":set_exterior: radio exterior no positivo"))

   (** Selector del número de meridianos *)
   method get_meridianos = meridianos
   (** Modificador del número de meridianos *)
   method set_meridianos meridianos' =
      if meridianos' > 0 then
         meridianos <- meridianos'
      else
         raise (Invalid_argument (self#clase ^ ":set_meridianos: número de meridianos no positivo"))

   (** Selector del número de paralelos *)
   method get_paralelos = paralelos
   (** Modificador del número de paralelos *)
   method set_paralelos paralelos' =
      if paralelos' > 0 then
         paralelos <- paralelos'
      else
         raise (Invalid_argument (self#clase ^ ":set_paralelos: número de paralelos no positivo"))

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      if not sombra then material # pintar ~cara;
      GlMat.push ();
      GlMat.rotate ~angle:(-.90.0) ~x:1.0 ();
      let quad = GluQuadric.create () in
      GluQuadric.normals quad (match normales with `Poligono -> `flat | `Vertice -> `smooth);
      GluQuadric.disk ~inner:interior ~outer:exterior ~slices:meridianos ~loops:paralelos ~quad ();
      GlMat.pop ()

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo = (max sx sz) *. exterior

   (** Implementación de la interfaz [dibujable] *)
   method get_limites = sx *. exterior, sy *. 0.0, sz *. exterior

   (** Implementación de la interfaz [serializable] *)
   method clase = "disco"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self#clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml  "nombre");
         self  # set_interior   (float_of_string (Xml.attrib xml "interior"  ));
         self  # set_exterior   (float_of_string (Xml.attrib xml "exterior"  ));
         self  # set_meridianos (int_of_string   (Xml.attrib xml "meridianos"));
         self  # set_paralelos  (int_of_string   (Xml.attrib xml "paralelos" ))
      end else
         raise (Invalid_argument (self#clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml = Xml.Element (self#clase,
      [
         "nombre",     nombre;
         "interior",   (Defs.float2xml interior  );
         "exterior",   (Defs.float2xml exterior  );
         "meridianos", (string_of_int  meridianos);
         "paralelos",  (string_of_int  paralelos )
      ],
      [material # to_xml; super # to_xml])

   (** Constructor del objeto *)
   initializer
      self # set_interior   init_interior;
      self # set_exterior   init_exterior;
      self # set_meridianos init_meridianos;
      self # set_paralelos  init_paralelos
end
