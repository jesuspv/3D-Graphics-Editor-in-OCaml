open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para las mallas de polígonos *)
class malla (xml : Xml.xml) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Lista de polígonos *)
   val mutable caras = Array.make 0 ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0))

   (** Lista de las normales al polígono *)
   val mutable normales_poligono = Array.make 0 (0.0, 0.0, 0.0)

   (** Lista de las normales al vértice *)
   val mutable normales_vertice = Array.make 0 (0.0, 0.0, 0.0)

   (** Lista de coordenadas de textura *)
   val mutable coord2 = Array.make 0 ((0.0, 0.0), (0.0, 0.0), (0.0, 0.0))

   (** Textura *)
   val mutable textura = Textura.por_defecto

   (** Nombre del archivo de textura *)
   val mutable archivo_textura = ""

   (** Calcula las normales *)
   method private calcular_normales =
      normales_poligono <- Array.make (Array.length caras * 3) (0.0, 0.0, 0.0);
      normales_vertice  <- Array.make (Array.length caras * 3) (0.0, 0.0, 0.0);

      (* calcula las normales al polígono *)
      for i = 0 to Array.length caras - 1 do
         let p0, p1, p2 = caras.(i) in
         let n_pol = Func.vectorial_p p0 p1 p2 in
         normales_poligono.((3 * i)    ) <- n_pol;
         normales_poligono.((3 * i) + 1) <- n_pol;
         normales_poligono.((3 * i) + 2) <- n_pol
      done;

      (* obtiene la lista de normales concurrentes en un único vértice dado *)
      let get_normales punto =
         let normales = ref [] in
         for i = 0 to Array.length caras - 1 do
            let p0, p1, p2 = caras.(i) in
            if punto = p0 then
               normales := normales_poligono.((3 * i)) :: !normales
            else if punto = p1 then
               normales := normales_poligono.((3 * i)) :: !normales
            else if punto = p2 then
               normales := normales_poligono.((3 * i)) :: !normales
         done;
         Array.of_list !normales
      in

      (* calcula las normales al vértice *)
      for i = 0 to Array.length caras - 1 do
         let p0, p1, p2 = caras.(i)
         and get_normal punto =
            let normales = get_normales punto in
            (* calcula la media de las normales *)
            let x, y, z = ref 0.0, ref 0.0, ref 0.0 in
            for j = 0 to Array.length normales - 1 do
               let xj, yj, zj = normales.(j) in
               x := !x +. xj;
               y := !y +. yj;
               z := !z +. zj
            done;
            let ctn = float (Array.length normales) in
            let nor = !x /. ctn, !y /. ctn, !z /. ctn in
            Func.normalizar3 nor
         in
         let ns = List.map get_normal [p0; p1; p2] in
         normales_vertice.((3 * i)    ) <- List.nth ns 0;
         normales_vertice.((3 * i) + 1) <- List.nth ns 1;
         normales_vertice.((3 * i) + 2) <- List.nth ns 2
      done

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      GlMisc.push_attrib [`lighting]; (* IMPORTANTE, SI NO LAS TEXTURAS SALEN MAL *)

      if not sombra then begin
         material # pintar ~cara;
         (* activa las texturas *)
         if archivo_textura <> "" then begin
            Gl.enable `texture_2d;
            Textura.pintar textura
         end
      end;

      let normales  = if normales = `Poligono then normales_poligono else normales_vertice
      and ct_normal = ref 0
      in
      GlDraw.begins `triangles;
      for i = 0 to Array.length caras - 1 do
         let p0, p1, p2 = caras.(i)
         and c0, c1, c2 = coord2.(i)
         in
         GlTex.coord2   c0;
         GlDraw.normal3 normales.(!ct_normal);
         GlDraw.vertex3 p0;
         GlTex.coord2   c1;
         GlDraw.normal3 normales.(!ct_normal + 1);
         GlDraw.vertex3 p1;
         GlTex.coord2   c2;
         GlDraw.normal3 normales.(!ct_normal + 2);
         GlDraw.vertex3 p2;
         ct_normal := !ct_normal + 3
      done;
      GlDraw.ends ();

      (* desactiva las texturas (importante) *)
      Gl.disable `texture_2d;

      GlMisc.pop_attrib ()

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo =
      let (xi, yi, zi), (_,_,_), (_,_,_) = caras.(0) in
      Array.fold_right
         (fun ((x0,y0,z0), (x1,y1,z1), (x2,y2,z2)) maximo ->
            let sqrt0 = sqrt ((x0 *. sx) ** 2. +. (y0 *. sy) ** 2. +. (z0 *. sz) ** 2.)
            and sqrt1 = sqrt ((x1 *. sx) ** 2. +. (y1 *. sy) ** 2. +. (z1 *. sz) ** 2.)
            and sqrt2 = sqrt ((x2 *. sx) ** 2. +. (y2 *. sy) ** 2. +. (z2 *. sz) ** 2.)
            in
            max maximo (max sqrt0 (max sqrt1 sqrt2)))
         caras
         (sqrt ((xi *. sx) ** 2. +. (yi *. sy) ** 2. +. (zi *. sz) ** 2.))

   (** Implementación de la interfaz [dibujable] *)
   method get_limites =
      let (xi, yi, zi), (_,_,_), (_,_,_) = caras.(0) in
      sx *. (Array.fold_right
         (fun ((x0,_,_), (x1,_,_), (x2,_,_)) maximo ->
            let m_x = max (abs_float x0) (max (abs_float x1) (abs_float x2)) in
            max m_x maximo)
         caras
         (abs_float xi)),
      sy *. (Array.fold_right
         (fun ((_,y0,_), (_,y1,_), (_,y2,_)) maximo ->
            let m_y = max (abs_float y0) (max (abs_float y1) (abs_float y2)) in
            max m_y maximo)
         caras
         (abs_float yi)),
      sz *. (Array.fold_right
         (fun ((_,_,z0), (_,_,z1), (_,_,z2)) maximo ->
            let m_z = max (abs_float z0) (max (abs_float z1) (abs_float z2)) in
            max m_z maximo)
         caras
         (abs_float zi))

   (** Establece las caras desde la definición XML *)
   method private superficie_from_xml (xml : Xml.xml) =
      if Xml.tag xml = "superficie" then begin
         (* establece las caras *)
         caras <- Array.of_list (Xml.map
            (fun c ->
               if Xml.tag c = "cara" then begin
                  let p0 = List.nth (Xml.children c) 0
                  and p1 = List.nth (Xml.children c) 1
                  and p2 = List.nth (Xml.children c) 2
                  in
                  let get_p p =
                     if Xml.tag p = "punto3" then
                        float_of_string (Xml.attrib p "x"),
                        float_of_string (Xml.attrib p "y"),
                        float_of_string (Xml.attrib p "z")
                     else
                       raise (Invalid_argument (self # clase ^ ":superficie_from_xml: la etiqueta XML es incorrecta"))
                  in
                  get_p p0, get_p p1, get_p p2
               end else
                  raise (Invalid_argument (self # clase ^ ":superficie_from_xml: la etiqueta XML es incorrecta")))
            xml);
         (* establece las coordenadas de textura *)
         coord2 <- Array.of_list (Xml.map
            (fun c ->
               if Xml.tag c = "cara" then begin
                  let p0 = List.nth (Xml.children c) 0
                  and p1 = List.nth (Xml.children c) 1
                  and p2 = List.nth (Xml.children c) 2
                  in
                  let get_c p =
                     if Xml.tag p = "punto3" then
                        float_of_string (Xml.attrib p "u"),
                        float_of_string (Xml.attrib p "v")
                     else
                       raise (Invalid_argument (self # clase ^ ":superficie_from_xml: la etiqueta XML es incorrecta"))
                  in
                  get_c p0, get_c p1, get_c p2
               end else
                  raise (Invalid_argument (self # clase ^ ":superficie_from_xml: la etiqueta XML es incorrecta")))
            xml);
         self # calcular_normales;
         try
            self # cargar_textura (Filename.dirname !Defs.escena_archivo) (Xml.attrib xml "textura")
         with
            Xml.No_attribute _ -> ()
      end else
         raise (Invalid_argument (self # clase ^ ":superficie_from_xml: la etiqueta XML es incorrecta"))

   (** Obtiene la definición XML de las caras *)
   method private superficie_to_xml =
      let puntos = List.combine (Array.to_list coord2) (Array.to_list caras) in
      Xml.Element ("superficie",
         (if archivo_textura <> "" then ["textura", archivo_textura] else []),
         List.map
            (fun (((u0, v0), (u1, v1), (u2, v2)), ((x0,y0,z0), (x1,y1,z1), (x2,y2,z2))) ->
               let p0 = Xml.Element ("punto3",
                  ["u", Defs.float2xml u0; "v", Defs.float2xml v0; "x", Defs.float2xml x0; "y", Defs.float2xml y0; "z", Defs.float2xml z0],
                  [])
               and p1 = Xml.Element ("punto3",
                  ["u", Defs.float2xml u1; "v", Defs.float2xml v1; "x", Defs.float2xml x1; "y", Defs.float2xml y1; "z", Defs.float2xml z1],
                  [])
               and p2 = Xml.Element ("punto3",
                  ["u", Defs.float2xml u2; "v", Defs.float2xml v2; "x", Defs.float2xml x2; "y", Defs.float2xml y2; "z", Defs.float2xml z2],
                  [])
               in
               Xml.Element ("cara", [], [p0; p1; p2]))
            puntos)

   (** Carga la textura desde un archivo XML de definición *)
   method private cargar_textura (ruta : string) (archivo : string) =
      let xml = Xml.parse_file
         (if Filename.is_relative archivo then
            Filename.concat ruta archivo
         else
            archivo)
      in
      textura         <- Textura.from_xml xml;
      archivo_textura <- archivo

   (** Implementación de la interfaz [serializable] *)
   method clase = "malla"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self # clase then begin
         material # from_xml         (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml            (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre          (Xml.attrib xml "nombre");
         self  # superficie_from_xml (List.nth (Xml.children xml) 2)  (* superficie     *)
      end else
         raise (Invalid_argument (self # clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      Xml.Element (self # clase,
      [
         "nombre",  nombre;
      ],
      [material # to_xml; super # to_xml; self # superficie_to_xml])

   (** Constructor del objeto *)
   initializer
      self # from_xml xml
end
