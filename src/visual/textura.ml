(** Definición de texturas a partir de XML *)

(** Tipo textura *)
type textura =
{
   nombre       : string;
   entorno      : GlTex.env_param;
   filtrado_mag : [`linear | `nearest];
   filtrado_min : GlTex.filter;
   repeticion   : GlTex.wrap;
   ancho        : int;
   alto         : int;
   pixels       : (GlTex.format, Gl.kind) GlPix.t
}

(** Textura por defecto *)
let por_defecto =
{
   nombre       = "defecto";
   entorno      = `mode `decal;
   filtrado_mag = `nearest;
   filtrado_min = `nearest;
   repeticion   = `clamp;
   ancho        = 1;
   alto         = 1;
   pixels       = GlPix.create `ubyte ~format:`rgba ~width:1 ~height:1
}

(** Carga el archivo de imagen (FORMATO PNM CRUDO) *)
let cargar_archivo (archivo : Xml.xml) : int * int * ('b, 'a) GlPix.t =
   let nombre = Xml.pcdata (List.hd (Xml.children archivo)) in
   let flujo  = open_in_bin
      (if Filename.is_relative nombre then
         Filename.concat (Filename.dirname !Defs.escena_archivo) nombre
      else
         nombre)
   in
   let magico = input_line flujo   in
   let dims   =
      let linea = input_line flujo in
      if String.contains linea '#' then (* comentario *)
         input_line flujo
      else
         linea
   in
   if magico = "P6" then ignore (input_line flujo);
   let sp    = String.index dims ' ' in
   let ancho = int_of_string (String.sub dims 0 sp) in
   let alto  = int_of_string (String.sub dims (sp + 1) ((String.length dims) - sp - 1)) in
   (* crea la imagen *)
   let imagen = GlPix.create `ubyte ~format:`rgb ~width:ancho ~height:alto in
   (* establece la imagen *)
   for i = 1 to alto do
      let x = (alto - i) * ancho * 3 in
      for j = 0 to ancho * 3 - 1 do
         Raw.sets (GlPix.to_raw imagen) ~pos:(x + j) [|(int_of_char (input_char flujo))|]
      done
   done;
   close_in flujo;
   (ancho, alto, imagen)

(*
(** Carga el raster de pixels de imagen *)
let cargar_raster (raster : Xml.xml) : int * int * ('b, 'a) GlPix.t =
   let ancho = int_of_string (Xml.attrib raster "ancho")
   and alto  = int_of_string (Xml.attrib raster "alto")
   in
   let imagen = GlPix.create `ubyte ~format:`rgba ~width:ancho ~height:alto in

   let ct = ref 0 in
   Xml.iter
      (fun pix ->
         let r = int_of_float (float_of_string (Xml.attrib pix "r") *. 255.)
         and g = int_of_float (float_of_string (Xml.attrib pix "g") *. 255.)
         and b = int_of_float (float_of_string (Xml.attrib pix "b") *. 255.)
         and a = int_of_float (float_of_string (Xml.attrib pix "a") *. 255.)
         in
         Raw.sets (GlPix.to_raw imagen) ~pos:(4 * !ct) [|r; g; b; a|];
         incr ct)
      raster;
   (ancho, alto, imagen)
*)

(** Carga el raster de pixels de imagen *)
let cargar_raster (raster : Xml.xml) : int * int * ('b, 'a) GlPix.t =
   let ancho = int_of_string (Xml.attrib raster "ancho")
   and alto  = int_of_string (Xml.attrib raster "alto")
   in
   let buffer = Xml.pcdata (List.nth (Xml.children raster) 0) in

   (* obtiene los pixels *)
   let pixels = Array.make (ancho * alto) [|0; 0; 0; 0|]
   and ctb = ref 0
   and ctp = ref 0
   in
   while !ctb < (String.length buffer - 1) do
      let r = int_of_string (String.sub buffer !ctb 3) in ctb := !ctb + 3;
      let g = int_of_string (String.sub buffer !ctb 3) in ctb := !ctb + 3;
      let b = int_of_string (String.sub buffer !ctb 3) in ctb := !ctb + 3;
      let a = int_of_string (String.sub buffer !ctb 3) in ctb := !ctb + 3;

      pixels.(!ctp) <- [|r; g; b; a|];
      incr ctp
   done;

   (* establece la imagen *)
   let imagen = GlPix.create `ubyte ~format:`rgba ~width:ancho ~height:alto in
   for y = 0 to alto - 1 do
      for x = 0 to ancho - 1 do
         let pos  = y * ancho + x in
         let pos' = (alto - 1 - y) * ancho + x in
         Raw.sets (GlPix.to_raw imagen) ~pos:(4 * pos) pixels.(pos')
      done
   done;

   (ancho, alto, imagen)

(** Importa una textura desde su definición XML *)
let from_xml (xml : Xml.xml) : textura =
   if Xml.tag xml = "textura" then begin
      let nombre  = Xml.attrib xml "nombre"
      and mapeado = List.nth (Xml.children xml) 0 (* mapeado *)
      in
      (* analiza el mapeado *)
      if Xml.tag mapeado <> "mapeado" then
         raise (Invalid_argument ("Textura:from_xml: la etiqueta XML " ^ (Xml.tag xml) ^ " es incorrecta"));
      let entorno = match Xml.attrib mapeado "entorno" with
         "remplazar" -> `mode `decal
       | _           -> `mode `modulate (* "combinar" *)
      and filtrado_mag, filtrado_min = match Xml.attrib mapeado "filtrado" with
         "lineal" -> `linear, `linear
       | _        -> `nearest, `nearest (* "mas_cercano" *)
      and repeticion = match Xml.attrib mapeado "repeticion" with
         "on" -> `repeat
       | _    -> `clamp (* "off" *)
      in
      (* analiza la imagen *)
      let imagen = List.nth (Xml.children xml) 1 (* imagen *) in
      let ancho, alto, pixels = match Xml.tag imagen with
         "archivo" -> cargar_archivo imagen
       | "raster"  -> cargar_raster  imagen
       | _ -> raise (Invalid_argument ("Textura:from_xml: la etiqueta XML " ^ (Xml.tag xml) ^ " es incorrecta"))
      in
      {
         nombre       = nombre;
         entorno      = entorno;
         filtrado_mag = filtrado_mag;
         filtrado_min = filtrado_min;
         repeticion   = repeticion;
         ancho        = ancho;
         alto         = alto;
         pixels       = pixels
      }
   end else
      raise (Invalid_argument ("Textura:from_xml: la etiqueta XML " ^ (Xml.tag xml) ^ " es incorrecta"))

(** Pinta la textura. Tras ello se deberá activar las texturas y tras dibujar el
    objeto sería deseable desactivarlas. *)
let pintar (textura : textura) : unit =
   GlPix.store (`unpack_alignment 1);
   GlTex.image2d textura.pixels;
   List.iter (GlTex.parameter ~target:`texture_2d)
      [
         `wrap_s     textura.repeticion;
         `wrap_t     textura.repeticion;
         `mag_filter textura.filtrado_mag;
         `min_filter textura.filtrado_min
      ];
   GlTex.env textura.entorno
