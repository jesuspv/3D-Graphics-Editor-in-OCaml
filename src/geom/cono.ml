open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los conos *)
class cono
      ~base:      (init_base       : float)
      ~altura:    (init_altura     : float)
      ~meridianos:(init_meridianos : int  )
      ~paralelos: (init_paralelos  : int  ) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Base *)
   val mutable base = 1.0

   (** Altura *)
   val mutable altura = 1.0

   (** Número de meridianos *)
   val mutable meridianos = 1

   (** Número de paralelos *)
   val mutable paralelos  = 1

   (** Selector de la base *)
   method get_base = base
   (** Modificador de la base *)
   method set_base base' =
      if base' > 0.0 then
         base <- base'
      else
         raise (Invalid_argument (self#clase ^ ":set_base: base no positiva"))

   (** Selector de la altura *)
   method get_altura = altura
   (** Modificador de la altura *)
   method set_altura altura' =
      if altura' > 0.0 then
         altura <- altura'
      else
         raise (Invalid_argument (self#clase ^ ":set_altura: altura no positiva"))

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
      let sqrt2  = sqrt 2.0 in
      let normal = if normales = `Poligono then (fun _ _ -> ())
         else (fun rad y ->
               let x, y, z = sin rad /. sqrt2, y /. sqrt2, cos rad /. sqrt2 in
               GlDraw.normal3 (x, y, z)
            )
      in
      if not sombra then material # pintar ~cara;
      GlMat.push ();
      GlMat.translate ~y:(-.altura /. 2.0) ();
      GlMat.rotate    ~angle:(-.90.0) ~x:1.0 ();
      (* tubo *)
      let quad = GluQuadric.create () in
      GluQuadric.normals quad (match normales with `Poligono -> `flat | `Vertice -> `smooth);
      GluQuadric.cylinder ~base:base ~top:0.0 ~height:altura ~slices:meridianos ~stacks:paralelos ~quad ();
      GlMat.pop ();
      (* tapa inferior *)
      GlDraw.begins `triangle_fan;
         GlDraw.normal3 (0.0, -1.0, 0.0);
         GlDraw.vertex3 (0.0, -.altura /. 2.0, 0.0);
         for i = meridianos downto 0 do
            let delta = Func.grd2rad (float i *. 360.0 /. (float meridianos)) in
            normal delta (-1.0); GlDraw.vertex3 (sin delta *. base, -.altura /. 2.0, cos delta *. base)
         done;
      GlDraw.ends ();

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo = sqrt (((max sx sz) *. base) ** 2. +. (sy *. altura /. 2.) ** 2.)

   (** Implementación de la interfaz [dibujable] *)
   method get_limites = sx *. base, sy *. altura /. 2., sz *. base

   (** Implementación de la interfaz [serializable] *)
   method clase = "cono"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self#clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml  "nombre");
         self  # set_base       (float_of_string (Xml.attrib xml "base"      ));
         self  # set_altura     (float_of_string (Xml.attrib xml "altura"    ));
         self  # set_meridianos (int_of_string   (Xml.attrib xml "meridianos"));
         self  # set_paralelos  (int_of_string   (Xml.attrib xml "paralelos" ))
      end else
         raise (Invalid_argument (self#clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml = Xml.Element (self#clase,
      [
         "nombre",     nombre;
         "base",       (Defs.float2xml base      );
         "altura",     (Defs.float2xml altura    );
         "meridianos", (string_of_int  meridianos);
         "paralelos",  (string_of_int  paralelos )
      ],
      [material # to_xml; super # to_xml])

   (** Constructor del objeto *)
   initializer
      self # set_base       init_base;
      self # set_altura     init_altura;
      self # set_meridianos init_meridianos;
      self # set_paralelos  init_paralelos
end
