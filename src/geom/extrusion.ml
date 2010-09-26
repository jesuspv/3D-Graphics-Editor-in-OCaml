open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los objetos generados por extrusión *)
class extrusion (xml : Xml.xml) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Profundidad *)
   val mutable profundidad = 1.0

   (** Lista de vértices *)
   val mutable seccion = Array.make 0 (0.0, 0.0)

   (** Lista de las normales al polígono *)
   val mutable normales_poligono = Array.make 0 (0.0, 0.0, 0.0)

   (** Lista de las normales al vértice *)
   val mutable normales_vertice = Array.make 0 (0.0, 0.0, 0.0)

   (** Selector de la profundidad *)
   method get_profundidad = profundidad

   (** Modificador de la profundidad *)
   method set_profundidad profundidad' =
      if profundidad' > 0.0 then
         profundidad <- profundidad'
      else
         raise (Invalid_argument (self # clase ^ ":set_profundidad: profundidad no positiva"))

   (** Calcula las normales *)
   method private calcular_normales =
      normales_poligono <- Array.make (Array.length seccion * 6) (0.0, 0.0, 0.0);
      normales_vertice  <- Array.make (Array.length seccion * 6) (0.0, 0.0, 0.0);
      let ct_n = ref 0 in
      (* normales de la tapa posterior *)
      let seccion' = Array.make (Array.length seccion) (0.0, 0.0) in
      for i = 0 to Array.length seccion' - 1 do
         seccion'.(i) <- seccion.(Array.length seccion - i - 1)
      done;
      for i = 0 to Array.length seccion' - 1 do
         let u, v   = seccion'.(i) in
         let modulo = sqrt (u ** 2. +. v ** 2. +. (sqrt (u ** 2. +. v ** 2.))) in
         let nu, nv, nz = u /. modulo, v /. modulo, (sqrt (u ** 2. +. v ** 2.)) /. modulo in
         normales_poligono.(!ct_n) <- (0.0, 0.0, -1.0);
         normales_vertice .(!ct_n) <- (nu, nv, -.nz);
         incr ct_n
      done;
      (* normales de la extrusión *)
      for i = 0 to Array.length seccion - 2 do
         let u0, v0 = seccion.(i)
         and u1, v1 = seccion.(i + 1)
         in
         let nu0, nv0 = Func.normalizar2 (u0, v0)
         and nu1, nv1 = Func.normalizar2 (u1, v1)
         in
         let n_pol = Func.vectorial_p (u0, v0, 0.0) (u0, v0, -1.0) (u1, v1, 0.0) in
         normales_poligono.(!ct_n)     <- n_pol;
         normales_poligono.(!ct_n + 1) <- n_pol;
         normales_poligono.(!ct_n + 2) <- n_pol;
         normales_poligono.(!ct_n + 3) <- n_pol;
         normales_vertice .(!ct_n)     <- Func.normalizar3 (nu0, nv0, 0.0);
         normales_vertice .(!ct_n + 1) <- Func.normalizar3 (nu0, nv0, 0.0);
         normales_vertice .(!ct_n + 2) <- Func.normalizar3 (nu1, nv1, 0.0);
         normales_vertice .(!ct_n + 3) <- Func.normalizar3 (nu1, nv1, 0.0);
         ct_n := !ct_n + 4
      done;
      (* normales de la tapa anterior *)
      for i = 0 to Array.length seccion - 1 do
         let u, v   = seccion.(i) in
         let modulo = sqrt (u ** 2. +. v ** 2. +. (sqrt (u ** 2. +. v ** 2.))) in
         let nu, nv, nz = u /. modulo, v /. modulo, (sqrt (u ** 2. +. v ** 2.)) /. modulo in
         normales_poligono.(!ct_n) <- (0.0, 0.0, 1.0);
         normales_vertice .(!ct_n) <- (nu, nv, nz);
         incr ct_n
      done

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      if not sombra then material # pintar ~cara;
      let normales  = if normales = `Poligono then normales_poligono else normales_vertice
      and ct_normal = ref 0
      and z = profundidad /. 2.
      in
      (* sección posterior *)
      let seccion' = Array.make (Array.length seccion) (0.0, 0.0) in
      for i = 0 to Array.length seccion' - 1 do
         seccion'.(i) <- seccion.(Array.length seccion - i - 1)
      done;
      GlDraw.begins `triangle_fan;
         GlDraw.normal3 (0.0, 0.0, -1.0);
         GlDraw.vertex3 (0.0, 0.0, -.z);
         for i = 0 to Array.length seccion' - 1 do
            let u, v = seccion'.(i) in
            GlDraw.normal3 normales.(!ct_normal);
            GlDraw.vertex3 (u, v, -.z);
            incr ct_normal
         done;
      GlDraw.ends ();
      (* extrusión *)
      GlDraw.begins `quads;
         for i = 0 to Array.length seccion - 2 do
            let u0, v0 = seccion.(i)
            and u1, v1 = seccion.(i + 1)
            in
            GlDraw.normal3 normales.(!ct_normal);
            GlDraw.vertex3 (u0, v0,   z);
            GlDraw.normal3 normales.(!ct_normal + 1);
            GlDraw.vertex3 (u0, v0, -.z);
            GlDraw.normal3 normales.(!ct_normal + 2);
            GlDraw.vertex3 (u1, v1, -.z);
            GlDraw.normal3 normales.(!ct_normal + 3);
            GlDraw.vertex3 (u1, v1,   z);
            ct_normal := !ct_normal + 4
         done;
      GlDraw.ends ();
      (* sección anterior *)
      GlDraw.begins `triangle_fan;
         GlDraw.normal3 (0.0, 0.0, 1.0);
         GlDraw.vertex3 (0.0, 0.0, z);
         for i = 0 to Array.length seccion - 1 do
            let u, v = seccion.(i) in
            GlDraw.normal3 normales.(!ct_normal);
            GlDraw.vertex3 (u, v, z);
            incr ct_normal
         done;
      GlDraw.ends ()

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo =
      let max_seccion = Array.fold_right
         (fun (u, v) maximo -> max (sqrt (((sx *. u) ** 2.) +. ((sy *. v) ** 2.))) maximo)
         seccion
         (sqrt ((fst seccion.(0) ** 2.) +. (snd seccion.(0) ** 2.)))
      in
      sqrt ((sz *. profundidad /. 2.) ** 2. +. max_seccion ** 2.)

   (** Implementación de la interfaz [dibujable] *)
   method get_limites =
      sx *. (Array.fold_right (fun (u, v) maximo -> max (abs_float u) maximo) seccion (abs_float (fst (seccion.(0))))),
      sy *. (Array.fold_right (fun (u, v) maximo -> max (abs_float v) maximo) seccion (abs_float (snd (seccion.(0))))),
      sz *. profundidad /. 2.

   (** Implementación de la interfaz [serializable] *)
   method clase = "extrusion"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self # clase then begin
         material # from_xml     (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml        (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre      (Xml.attrib xml  "nombre");
         self  # set_profundidad (float_of_string (Xml.attrib xml "profundidad"));
         let s = List.nth (Xml.children xml) 2 in                 (* seccion        *)
         seccion <- Array.of_list (Xml.map
            (fun v -> if Xml.tag v = "punto2" then
                  float_of_string (Xml.attrib v "u"),
                  float_of_string (Xml.attrib v "v")
               else
                  raise (Invalid_argument (self # clase ^ ":from_xml: la etiqueta XML es incorrecta")))
            s);
         self # calcular_normales
      end else
         raise (Invalid_argument (self # clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let s = Array.to_list (Array.map
         (fun (u, v) -> Xml.Element ("punto2",
            [("u", Defs.float2xml u); ("v", Defs.float2xml v)],
            []))
         seccion)
      in
      Xml.Element (self # clase,
      [
         "nombre",      nombre;
         "profundidad", (Defs.float2xml profundidad)
      ],
      [material # to_xml; super # to_xml; Xml.Element ("seccion", [], s)])

   (** Constructor del objeto *)
   initializer
      self # from_xml xml
end

(** Obtiene la definición XML del objeto extruido *)
let get_xml ?(m = Material.material_defecto) ?(t = Transformacion.transformacion_defecto)
      (seccion     : Gl.point2 list)
      (profundidad : float) : Xml.xml =
   let s = Xml.Element ("seccion",
      [],
      List.map
         (fun (u, v) -> Xml.Element ("punto2", ["u", (string_of_float u); "v", (string_of_float v)], []))
         seccion)
   in
   Xml.Element ("extrusion",
      ["nombre", "Extrusión"; "profundidad", (string_of_float profundidad)],
      [m; t; s])
