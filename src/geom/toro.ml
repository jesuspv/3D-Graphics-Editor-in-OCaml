open Dibujable    (** Clase [dibujable]    *)
open Revolucion   (** Clase [revolucion]   *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los toros *)
class toro
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

   (** Grados para revolucionar *)
   val mutable grados = 360.0

   (** Número de meridianos *)
   val mutable meridianos = 1

   (** Número de paralelos *)
   val mutable paralelos  = 1

   (** Objeto de revolución *)
   val mutable revolucion = new revolucion (Revolucion.get_xml
      [(0., 0.); (0., 0.); (0., 0.)] 1.0 360.0 3)

   (** Selector del radio interior *)
   method get_interior = interior
   (** Modificador del radio interior *)
   method set_interior interior' =
      if interior' > 0.0 then
         interior <- interior'
      else
         raise (Invalid_argument (self # clase ^ ":set_interior: radio interior no positivo"))

   (** Selector del radio exterior *)
   method get_interior = exterior
   (** Modificador del radio exterior *)
   method set_exterior exterior' =
      if exterior' > 0.0 then
         exterior <- exterior'
      else
         raise (Invalid_argument (self # clase ^ ":set_exterior: radio exterior no positivo"))

   (** Selector de los grados de giro *)
   method get_grados = grados

   (** Modificador de los grados de giro *)
   method set_grados grados' =
      grados <- mod_float grados' 360.0;
      if grados < 0.0 then
         grados <- grados +. 360.0
      else if grados = 0.0 then
         grados <- 360.0

   (** Selector del número de meridianos *)
   method get_meridianos = meridianos
   (** Modificador del número de meridianos *)
   method set_meridianos meridianos' =
      if meridianos' > 0 then
         meridianos <- meridianos'
      else
         raise (Invalid_argument (self # clase ^ ":set_meridianos: número de meridianos no positivo"))

   (** Selector del número de paralelos *)
   method get_paralelos = paralelos
   (** Modificador del número de paralelos *)
   method set_paralelos paralelos' =
      if paralelos' > 0 then
         paralelos <- paralelos'
      else
         raise (Invalid_argument (self # clase ^ ":set_paralelos: número de paralelos no positivo"))

   (** Modificador del objeto de revolución *)
   method private set_revolucion =
      revolucion <- new revolucion
         (Revolucion.get_xml ~m:material#to_xml self#get_seccion exterior grados meridianos)

   (** Modificador del material *)
   method set_material m =
      super # set_material m;
      revolucion <- new revolucion
         (Revolucion.get_xml ~m:material#to_xml self#get_seccion exterior grados meridianos)

   (** Obtiene la circunferencia *)
   method private get_seccion =
      let seccion = ref [] in
      for i = 0 to paralelos do
         let delta = Func.grd2rad (float i *. 360.0 /. (float paralelos)) in
         seccion := List.append !seccion [(cos delta *. interior, sin delta *. interior)]
      done;
      !seccion

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      revolucion # dibujar ~normales ~cara ~sombra ();

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo = (max sx sz) *. (exterior +. interior)

   (** Implementación de la interfaz [dibujable] *)
   method get_limites = sx *. (exterior +. interior), sy *. interior, sz *. (exterior +. interior)

   (** Implementación de la interfaz [serializable] *)
   method clase = "toro"

   (** Implementación de la intierfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self # clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml  "nombre");
         self  # set_interior   (float_of_string (Xml.attrib xml "interior"  ));
         self  # set_exterior   (float_of_string (Xml.attrib xml "exterior"  ));
         self  # set_grados     (float_of_string (Xml.attrib xml "grados"    ));
         self  # set_meridianos (int_of_string   (Xml.attrib xml "meridianos"));
         self  # set_paralelos  (int_of_string   (Xml.attrib xml "paralelos" ));
         self  # set_revolucion
      end else
         raise (Invalid_argument (self # clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml = Xml.Element (self # clase,
      [
         "nombre",     nombre;
         "interior",   (Defs.float2xml interior  );
         "exterior",   (Defs.float2xml exterior  );
         "grados",     (Defs.float2xml grados    );
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
