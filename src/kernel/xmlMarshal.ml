(** Construcción de objetos a partir de su definición XML *)

(** [XmlMarshal.make_geo def] constructor geometrias *)
let rec make_geo (def : Xml.xml) : Dibujable.dibujable =
   let def_float = 1.0
   and def_int   = 1
   in
   match
      Xml.tag def
   with
      "cilindro" -> let obj = (new Cilindro.cilindro
            ~base:      def_float
            ~altura:    def_float
            ~meridianos:def_int
            ~paralelos: def_int :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "cono" -> let obj = (new Cono.cono
            ~base:      def_float
            ~altura:    def_float
            ~meridianos:def_int
            ~paralelos: def_int :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "cubo" -> let obj = (new Cubo.cubo ~lado:def_float :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "disco" -> let obj = (new Disco.disco
            ~interior:  def_float
            ~exterior:  def_float
            ~meridianos:def_int
            ~paralelos: def_int :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "esfera" -> let obj = (new Esfera.esfera
            ~radio:     def_float
            ~meridianos:def_int
            ~paralelos: def_int :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "extrusion" -> let obj = (new Extrusion.extrusion def :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "malla" -> let obj = (new Malla.malla def :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "revolucion" -> let obj = (new Revolucion.revolucion def :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | "toro" -> let obj = (new Toro.toro
            ~interior:  def_float
            ~exterior:  def_float
            ~meridianos:def_int
            ~paralelos: def_int :> Dibujable.dibujable)
         in
         obj # from_xml def; obj
    | geom ->
         raise (Invalid_argument ("Geometría <" ^ geom ^ "> no reconocida"))

(** [XmlMarshal.make_luz def] Constructor de luces *)
let make_luz (def : Xml.xml) : Luz.luz =
   match Xml.tag def with
      "ambiental"   -> (new Luz.ambiental   def :> Luz.luz)
    | "puntual"     -> (new Luz.puntual     def :> Luz.luz)
    | "direccional" -> (new Luz.direccional def :> Luz.luz)
    | "focal"       -> (new Luz.focal       def :> Luz.luz)
    | ilu           -> raise (Invalid_argument ("Iluminación <" ^ ilu ^ "> no reconocida"))
