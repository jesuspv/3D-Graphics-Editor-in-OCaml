open Serializable (** Clase [serializable] *)

(** Definición XML del material por defecto *)
let material_defecto : Xml.xml =
   let bri = Xml.Element ("brillo",   [("factor", "15.0")], [])
   and esp = Xml.Element ("especular",[("r", "1.0"); ("g", "1.0"); ("b", "1.0"); ("a", "1.0")], [])
   and dif = Xml.Element ("difuso",   [("r", "0.9"); ("g", "0.9"); ("b", "0.9"); ("a", "1.0")], [])
   and amb = Xml.Element ("ambiente", [("r", "0.5"); ("g", "0.5"); ("b", "0.5"); ("a", "1.0")], [])
   and emi = Xml.Element ("emision",  [("r", "0.0"); ("g", "0.0"); ("b", "0.0"); ("a", "1.0")], [])
   in
   Xml.Element ("material", [("nombre", "Imprimación")], [bri; esp; dif; amb; emi])

(** Material para definir la apariencia de los objetos *)
class material ?(xml = material_defecto) () = object (yo)

   inherit serializable

   (** Nombre del material *)
   val mutable nombre = "defecto"

   (** Intensidad del brillo especular *)
   val mutable brillo = 1.0

   (** Color para la luz especular *)
   val mutable color_especular = (1.0, 1.0, 1.0, 1.0)

   (** Color para la luz difusa *)
   val mutable color_difuso = (0.5, 0.5, 0.5, 1.0)

   (** Color para la luz ambiente *)
   val mutable color_ambiente = (0.0, 0.0, 0.0, 1.0)

   (** Color de emisión *)
   val mutable color_emision = (0.0, 0.0, 0.0, 1.0)

   (** Selector del nombre *)
   method get_nombre = nombre

   (** Modificador del nombre *)
   method set_nombre n = nombre <- n

   (** Selector del brillo *)
   method get_brillo = brillo

   (** Modificador del brillo *)
   method set_brillo b = brillo <- b

   (** Selector del color especular *)
   method get_especular = color_especular

   (** Modificador del color especular *)
   method set_especular (r, g, b, a) =
      color_especular <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector del color difuso *)
   method get_difuso = color_difuso

   (** Modificador del color difuso *)
   method set_difuso (r, g, b, a) =
      color_difuso <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector del color ambiente *)
   method get_ambiente = color_ambiente

   (** Modificador del color ambiente *)
   method set_ambiente (r, g, b, a) =
      color_ambiente <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector del color de emisión *)
   method get_emision = color_emision

   (** Modificador del color de emisión *)
   method set_emision (r, g, b, a) =
      color_emision <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Informa acerca de la total opacidad del material *)
   method es_opaco =
      let _,_,_, a1 = color_especular
      and _,_,_, a2 = color_difuso
      and _,_,_, a3 = color_ambiente
      and _,_,_, a4 = color_emision
      in
      (1.0 = a1) && (a1 = a2) && (a2 = a3) && (a3 = a4)

   (** Pinta el material *)
   method pintar ~cara:face =
      (* Pintado para escenas con iluminación *)
      (* Junto con GlLight.light_model (two_side true), debemos
         usar GlLight.material ~face:`front si no queremos que la
         cosa pete en el dibujado de las caras *)
      GlLight.material ~face (`ambient   color_ambiente);
      GlLight.material ~face (`diffuse   color_difuso);
      GlLight.material ~face (`specular  color_especular);
      GlLight.material ~face (`shininess brillo);
      GlLight.material ~face (`emission  color_emision);
      (* Pintado para escenas sin iluminación *)
      let r, g, b, a = color_difuso in
      GlDraw.color ~alpha:a (r, g, b);

   (** Implementación de la interfaz [serializable] *)
   method clase = "material"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = yo # clase then begin
         let nom = Xml.attrib xml "nombre" in
         let hij = Xml.children xml in
         let bri = List.nth hij 0 (* brillo    *)
         and esp = List.nth hij 1 (* especular *)
         and dif = List.nth hij 2 (* difuso    *)
         and amb = List.nth hij 3 (* ambiente  *)
         and emi = List.nth hij 4 (* emisión   *)
         and ga nodo atrib = float_of_string (Xml.attrib nodo atrib)
         in
         let gc nodo   = (ga nodo "r"), (ga nodo "g"), (ga nodo "b"), (ga nodo "a") in
         let brillo    = ga bri "factor"
         and color_esp = gc esp
         and color_dif = gc dif
         and color_amb = gc amb
         and color_emi = gc emi
         in
         yo # set_nombre    nom;
         yo # set_brillo    brillo;
         yo # set_especular color_esp;
         yo # set_difuso    color_dif;
         yo # set_ambiente  color_amb;
         yo # set_emision   color_emi
      end else
         raise (Invalid_argument (yo # clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let esp_r, esp_g, esp_b, esp_a = yo # get_especular
      and dif_r, dif_g, dif_b, dif_a = yo # get_difuso
      and amb_r, amb_g, amb_b, amb_a = yo # get_ambiente
      and emi_r, emi_g, emi_b, emi_a = yo # get_emision
      in
      let bri = Xml.Element ("brillo", [("factor", Defs.float2xml yo # get_brillo)], [])
      and esp = Xml.Element ("especular",
         [("r", Defs.float2xml esp_r); ("g", Defs.float2xml esp_g); ("b", Defs.float2xml esp_b); ("a", Defs.float2xml esp_a)],
         [])
      and dif = Xml.Element ("difuso",
         [("r", Defs.float2xml dif_r); ("g", Defs.float2xml dif_g); ("b", Defs.float2xml dif_b); ("a", Defs.float2xml dif_a)],
         [])
      and amb = Xml.Element ("ambiente",
         [("r", Defs.float2xml amb_r); ("g", Defs.float2xml amb_g); ("b", Defs.float2xml amb_b); ("a", Defs.float2xml amb_a)],
         [])
      and emi = Xml.Element ("emision",
         [("r", Defs.float2xml emi_r); ("g", Defs.float2xml emi_g); ("b", Defs.float2xml emi_b); ("a", Defs.float2xml emi_a)],
         [])
      in
      Xml.Element (yo # clase, [("nombre", yo # get_nombre)], [bri; esp; dif; amb; emi])

   (** Valor normalizado en [0.0, 1.0] para una componente dada de color *)
   method private normalizar_color color =
      if color <= 0.0 then
         0.0
      else if color >= 1.0 then
         1.0
      else
         color

   (** Constructor del objeto *)
   initializer
      yo # from_xml xml
end
