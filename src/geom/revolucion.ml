open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los objetos generados por revolución *)
class revolucion (xml : Xml.xml) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Radio exterior (de revolución) *)
   val mutable radio = 1.0

   (** Grados para revolucionar *)
   val mutable grados = 360.0

   (** Meridianos *)
   val mutable meridianos = 32

   (** Lista de vértices *)
   val mutable seccion = Array.make 0 (0.0, 0.0)

   (** Lista de las normales al polígono *)
   val mutable normales_poligono = Array.make 0 (0.0, 0.0, 0.0)

   (** Lista de las normales al vértice *)
   val mutable normales_vertice = Array.make 0 (0.0, 0.0, 0.0)

   (** Selector del radio *)
   method get_radio = radio

   (** Modificador del radio *)
   method set_radio radio' =
      if radio' >= 0.0 then
         radio <- radio'
      else
         raise (Invalid_argument (self # clase ^ ":set_radio: radio negativo"))

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

   (** Calcula las normales *)
   method private calcular_normales =
      normales_poligono <- Array.make (Array.length seccion * 2 * (meridianos * 2)) (0.0, 0.0, 0.0);
      normales_vertice  <- Array.make (Array.length seccion * 2 * (meridianos * 2)) (0.0, 0.0, 0.0);
      let ct_n = ref 0 in
      (* normales de la revolución *)
      for ct_p = 0 to Array.length seccion - 2 do
         let u0, v0 = seccion.(ct_p)
         and u1, v1 = seccion.(ct_p + 1)
         in
         let giro = grados /. 360.0 in
         for ct_m = 0 to meridianos - 1 do
            let a0 = (float ct_m)       *. giro *. 2. *. Defs.pi /. (float meridianos)
            and a1 = (float (ct_m + 1)) *. giro *. 2. *. Defs.pi /. (float meridianos)
            in
            let p0 = (u0 +. radio) *. (cos a0), v0, (u0 +. radio) *. (sin a0)
            and p1 = (u1 +. radio) *. (cos a0), v1, (u1 +. radio) *. (sin a0)
            and p2 = (u0 +. radio) *. (cos a1), v0, (u0 +. radio) *. (sin a1)
            in
            let n_pol = Func.vectorial_p p0 p1 p2 in
            normales_poligono.(!ct_n)     <- n_pol;
            normales_poligono.(!ct_n + 1) <- n_pol;
            normales_poligono.(!ct_n + 2) <- n_pol;
            normales_poligono.(!ct_n + 3) <- n_pol;
            normales_vertice .(!ct_n)     <- Func.normalizar3 (u0 *. (cos a0), v0, u0 *. (sin a0));
            normales_vertice .(!ct_n + 1) <- Func.normalizar3 (u1 *. (cos a0), v1, u1 *. (sin a0));
            normales_vertice .(!ct_n + 2) <- Func.normalizar3 (u1 *. (cos a1), v1, u1 *. (sin a1));
            normales_vertice .(!ct_n + 3) <- Func.normalizar3 (u0 *. (cos a1), v0, u0 *. (sin a1));
            ct_n := !ct_n + 4
         done
      done

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      if not sombra then material # pintar ~cara;
      let normales  = if normales = `Poligono then normales_poligono else normales_vertice
      and ct_normal = ref 0
      in
      let giro = grados /. 360.0 in
      for ct_p = 0 to Array.length seccion - 2 do
         let u0, v0 = seccion.(ct_p)
         and u1, v1 = seccion.(ct_p + 1)
         in
         for ct_m = 0 to meridianos - 1 do
            let a0 = (float ct_m)       *. giro *. 2. *. Defs.pi /. (float meridianos)
            and a1 = (float (ct_m + 1)) *. giro *. 2. *. Defs.pi /. (float meridianos)
            in
            GlDraw.begins `quads;
               GlDraw.normal3 normales.(!ct_normal);
               GlDraw.vertex3 ((u0 +. radio) *. (cos a0), v0, (u0 +. radio) *. (sin a0));
               GlDraw.normal3 normales.(!ct_normal + 1);
               GlDraw.vertex3 ((u1 +. radio) *. (cos a0), v1, (u1 +. radio) *. (sin a0));
               GlDraw.normal3 normales.(!ct_normal + 2);
               GlDraw.vertex3 ((u1 +. radio) *. (cos a1), v1, (u1 +. radio) *. (sin a1));
               GlDraw.normal3 normales.(!ct_normal + 3);
               GlDraw.vertex3 ((u0 +. radio) *. (cos a1), v0, (u0 +. radio) *. (sin a1));
            GlDraw.ends ();
            ct_normal := !ct_normal + 4
         done
      done

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo =
      let x = (Array.fold_right (fun (u, v) maximo -> max u             maximo) seccion            (fst (seccion.(0))))
      and y = (Array.fold_right (fun (u, v) maximo -> max (abs_float v) maximo) seccion (abs_float (snd (seccion.(0)))))
      in
      sqrt (((x +. radio) *. (max sx sz)) ** 2. +. (y *. sy) ** 2.)
 
   (** Implementación de la interfaz [dibujable] *)
   method get_limites =
      sx *. (radio +. (Array.fold_right (fun (u, v) maximo -> max (abs_float u) maximo) seccion (abs_float (fst (seccion.(0)))))),
      sy *. (Array.fold_right (fun (u, v) maximo -> max (abs_float v) maximo) seccion (abs_float (snd (seccion.(0))))),
      sz *. (radio +. (Array.fold_right (fun (u, v) maximo -> max (abs_float u) maximo) seccion (abs_float (fst (seccion.(0))))))

   (** Implementación de la interfaz [serializable] *)
   method clase = "revolucion"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self # clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml  "nombre");
         self  # set_radio      (float_of_string (Xml.attrib xml "radio"));
         self  # set_grados     (float_of_string (Xml.attrib xml "grados"));
         self  # set_meridianos (int_of_string   (Xml.attrib xml "meridianos"));
         let s = List.nth (Xml.children xml) 2 in                (* seccion        *)
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
         "nombre",     nombre;
         "radio",      (Defs.float2xml radio);
         "grados",     (Defs.float2xml grados);
         "meridianos", (string_of_int  meridianos)
      ],
      [material # to_xml; super # to_xml; Xml.Element ("seccion", [], s)])

   (** Constructor del objeto *)
   initializer
      self # from_xml xml
end

(** Obtiene la definición XML del objeto revolucionado *)
let get_xml ?(m = Material.material_defecto) ?(t = Transformacion.transformacion_defecto)
      (seccion    : Gl.point2 list)
      (radio      : float)
      (grados     : float)
      (meridianos : int) : Xml.xml =
   let s = Xml.Element ("seccion",
      [],
      List.map
         (fun (u, v) -> Xml.Element ("punto2", ["u", (string_of_float u); "v", (string_of_float v)], []))
         seccion)
   in
   Xml.Element ("revolucion",
      [
         "nombre", "Revolución";
         "radio", (string_of_float radio);
         "grados", (string_of_float grados);
         "meridianos", (string_of_int meridianos)],
      [m; t; s])
