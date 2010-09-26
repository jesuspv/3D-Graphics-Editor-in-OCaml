open MatRot       (** Clase [mat_rot]      *)
open Serializable (** Clase [serializable] *)

(** Contador del número de luces registradas *)
(** Para que se registren las luces adecuadamente es necesario que
    se cree primero y obligatoriamente una luz ambiental *)
let ct_luces = ref (-1)

(** Luz genérica *)
class virtual luz (xml : Xml.xml) = object (yo)

   inherit serializable

   (** Nombre de la luz *)
   val mutable nombre = "defecto"

   (** Número de la luz *)
   val mutable numero = -1

   (** Activación de la luz *)
   val mutable encender = (fun () -> ())

   (** Desactivación de la luz *)
   val mutable apagar = (fun () -> ())

   (** Atenuación *)
   val mutable atenuacion : [`Constante | `Lineal | `Cuadratica] * float * (unit -> unit) =
      (`Constante, 1.0, fun () -> ())

   (** Color especular *)
   val mutable especular = (1.0, 1.0, 1.0, 1.0)

   (** Color difuso *)
   val mutable difusa = (0.5, 0.5, 0.5, 1.0)

   (** Color ambiente *)
   val mutable ambiente = (0.0, 0.0, 0.0, 1.0)

   (** Posición *)
   val mutable posicion = (0.0, 0.0, 0.0)

   (** Selector del nombre *)
   method get_nombre = nombre

   (** Modificador del nombre *)
   method set_nombre n = nombre <- n

   (** Selector de la atenuación *)
   method get_atenuacion = let dist, fact, _ = atenuacion in dist, fact

   (** Modificador de la atenuación *)
   method set_atenuacion ~dist ~fact =
      let f = if fact < 0.0 then 0.0 else fact in
      atenuacion <- dist, f,
      match dist with
         `Constante  -> (fun () -> GlLight.light ~num:numero (`constant_attenuation    f))
       | `Lineal     -> (fun () -> GlLight.light ~num:numero (`linear_attenuation      f))
       | `Cuadratica -> (fun () -> GlLight.light ~num:numero (`quadratic_attenuation   f))

   (** Selector del color especular *)
   method get_especular = especular

   (** Modificador del color especular *)
   method set_especular (r, g, b, a) =
      especular <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector del color difuso *)
   method get_difusa = difusa

   (** Modificador del color difuso *)
   method set_difusa (r, g, b, a) =
      difusa <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector del color ambiente *)
   method get_ambiente = ambiente

   (** Modificador del color ambiente *)
   method set_ambiente (r, g, b, a) =
      ambiente <-
         yo # normalizar_color r,
         yo # normalizar_color g,
         yo # normalizar_color b,
         yo # normalizar_color a

   (** Selector de la posición *)
   method get_posicion = posicion

   (** Modificador de la posición *)
   method set_posicion p = posicion <- p

   (** Selector de la orientación *)
   method virtual get_orientacion : Gl.vect3

   (** Modificador de la orientación *)
   method virtual set_orientacion : Gl.vect3 -> unit

   (** Posiciona en el espacio *)
   method posicionar ?(x = 0) ?(y = 0) ?(z = 0) () =
      let x0, y0, z0 = posicion in
      let x1, y1, z1 =
         x0 +. (Defs.pixel_factor_obj_t *. (float x)),
         y0 +. (Defs.pixel_factor_obj_t *. (float y)),
         z0 +. (Defs.pixel_factor_obj_t *. (float z))
      in
      posicion <- (x1, y1, z1);
      Printf.sprintf "Posición T(x,y,z) = (%.2f, %.2f, %.2f)" x1 y1 z1

   (** Orienta el en espacio *)
   method virtual orientar : ?x:int -> ?y:int -> ?z:int -> unit -> string

   (** Enciende la luz *)
   method encender = encender (); yo # iluminar

   (** Ilumina la escena *)
   method virtual iluminar : unit

   (** Apaga la luz *)
   method apagar = apagar ()

   (** Dibuja un marcador para la luz *)
   method virtual dibujar : unit

   (** Implementación de la interfaz [serializable] *)
   method clase = "luz"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = yo # clase then begin
         let nom = Xml.attrib xml "nombre" in
         let hij = Xml.children xml in
         let ate = List.nth hij 0 (* atenuación *)
         and esp = List.nth hij 1 (* especular  *)
         and dif = List.nth hij 2 (* difuso     *)
         and amb = List.nth hij 3 (* ambiente   *)
         and pos = List.nth hij 4 (* posición   *)
         and ga nodo atrib = float_of_string (Xml.attrib nodo atrib)
         in
         let gc nodo    = (ga nodo "r"), (ga nodo "g"), (ga nodo "b"), (ga nodo "a") in
         let atenuacion = Xml.attrib ate "distancia", ga ate "factor"
         and color_esp  = gc esp
         and color_dif  = gc dif
         and color_amb  = gc amb
         and posicion   = ga pos "x", ga pos "y", ga pos "z"
         in
         yo # set_nombre     nom;
         yo # set_atenuacion ~dist:(match fst atenuacion with
               "constante"  -> `Constante
             | "lineal"     -> `Lineal
             | "cuadratica" -> `Cuadratica
             | _            -> `Constante)
            ~fact:(snd atenuacion);
         yo # set_especular  color_esp;
         yo # set_difusa     color_dif;
         yo # set_ambiente   color_amb;
         yo # set_posicion   posicion
      end else
         raise (Invalid_argument ("luz:from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let distancia, factor = (match fst yo # get_atenuacion with
            `Constante  -> "constante"
          | `Lineal     -> "lineal"
          | `Cuadratica -> "cuadratica"),
         (snd yo # get_atenuacion)
      and esp_r, esp_g, esp_b, esp_a = yo # get_especular
      and dif_r, dif_g, dif_b, dif_a = yo # get_difusa
      and amb_r, amb_g, amb_b, amb_a = yo # get_ambiente
      and pos_x, pos_y, pos_z        = yo # get_posicion
      in
      let ate = Xml.Element ("atenuacion",
         [("distancia", distancia); ("factor", Defs.float2xml factor)],
         [])
      and esp = Xml.Element ("especular",
         [("r", Defs.float2xml esp_r); ("g", Defs.float2xml esp_g); ("b", Defs.float2xml esp_b); ("a", Defs.float2xml esp_a)],
         [])
      and dif = Xml.Element ("difusa",
         [("r", Defs.float2xml dif_r); ("g", Defs.float2xml dif_g); ("b", Defs.float2xml dif_b); ("a", Defs.float2xml dif_a)],
         [])
      and amb = Xml.Element ("ambiente",
         [("r", Defs.float2xml amb_r); ("g", Defs.float2xml amb_g); ("b", Defs.float2xml amb_b); ("a", Defs.float2xml amb_a)],
         [])
      and pos = Xml.Element ("posicion",
         [("x", Defs.float2xml pos_x); ("y", Defs.float2xml pos_y); ("z", Defs.float2xml pos_z)],
         [])
      in
      Xml.Element (yo # clase, [("nombre", yo # get_nombre)], [ate; esp; dif; amb; pos])

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
      yo # from_xml xml;
      if !ct_luces >= 8 then
         raise (Failure "El número de luces ha excedido el máximo permitido de 8")
      else if !ct_luces <> -1 then begin
         numero <- !ct_luces;
         (match numero with
            0 -> encender <- (fun () -> Gl.enable `light0); apagar <- (fun () -> Gl.disable `light0)
          | 1 -> encender <- (fun () -> Gl.enable `light1); apagar <- (fun () -> Gl.disable `light1)
          | 2 -> encender <- (fun () -> Gl.enable `light2); apagar <- (fun () -> Gl.disable `light2)
          | 3 -> encender <- (fun () -> Gl.enable `light3); apagar <- (fun () -> Gl.disable `light3)
          | 4 -> encender <- (fun () -> Gl.enable `light4); apagar <- (fun () -> Gl.disable `light4)
          | 5 -> encender <- (fun () -> Gl.enable `light5); apagar <- (fun () -> Gl.disable `light5)
          | 6 -> encender <- (fun () -> Gl.enable `light6); apagar <- (fun () -> Gl.disable `light6)
          | 7 -> encender <- (fun () -> Gl.enable `light7); apagar <- (fun () -> Gl.disable `light7)
          | _ -> raise (Failure "El número de luces ha excedido el máximo permitido de 8"))
      end;
      incr ct_luces
end

(** Definición XML de la luz ambiental por defecto *)
let ambiental_defecto : Xml.xml =
   Xml.Element ("ambiental", [("r", "0.15"); ("g", "0.15"); ("b", "0.15"); ("a", "1.0")], [])

(** Luz ambiental *)
class ambiental (xml : Xml.xml) = object (yo)

   inherit luz (xml)

   (** Implementación de la interfaz [luz] *)
   method get_orientacion = (0.0, 0.0, 0.0)

   (** Implementación de la interfaz [luz] *)
   method set_orientacion o = ignore o

   (** Orienta el en espacio *)
   method orientar ?(x = 0) ?(y = 0) ?(z = 0) () = ignore (x, y, z); ""

   (** Implementacion de la interfaz [luz] *)
   method iluminar = GlLight.light_model (`ambient ambiente)

   (** Implementacion de la interfaz [luz] *)
   method dibujar = ()

   (** Implementación de la interfaz [serializable] *)
   method clase = "ambiental"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = yo # clase then begin
         let ga nodo atrib = float_of_string (Xml.attrib nodo atrib) in
         let gc nodo atrib = float_of_string (Xml.attrib nodo atrib) in
         let color = (ga xml "r"), (ga xml "g"), (ga xml "b"), (ga xml "a") in
         yo # set_nombre   "ambiental";
         yo # set_ambiente color
      end else
         raise (Invalid_argument ("luz:from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let amb_r, amb_g, amb_b, amb_a = yo # get_ambiente
      in
      Xml.Element (yo # clase, [("r", Defs.float2xml amb_r); ("g", Defs.float2xml amb_g); ("b", Defs.float2xml amb_b); ("a", Defs.float2xml amb_a)], [])
end

(** Luz puntual *)
class puntual (xml : Xml.xml) = object (yo)

   inherit luz (xml)

   (** Implementación de la interfaz [luz] *)
   method get_orientacion = (0.0, 0.0, 0.0)

   (** Implementación de la interfaz [luz] *)
   method set_orientacion o = ignore o

   (** Implementación de la interfaz [luz] *)
   method iluminar =
      (* transforma la luz *)
      GlMat.push ();
      GlMat.translate3 yo # get_posicion;

      (* establece los materiales *)
      (* ilumina *)
      let _, _, f_atenuacion = atenuacion in f_atenuacion ();
      GlLight.light ~num:numero (`specular especular);
      GlLight.light ~num:numero (`diffuse  difusa   );
      GlLight.light ~num:numero (`ambient  ambiente );
      GlLight.light ~num:numero (`position (0.0, 0.0, 0.0, 1.0));

      (* deshace la transformación *)
      GlMat.pop ()

   (** Implementación de la interfaz [luz] *)
   method dibujar =
      (* guarda los atributos anteriores *)
      GlMisc.push_attrib [`lighting];
      GlMisc.push_attrib [`depth_buffer];
      GlMisc.push_attrib [`polygon];
      GlMisc.push_attrib [`color_buffer];
      (* establece los atributos *)
      Gl.disable `lighting;
      Gl.disable `depth_test;
      Gl.disable `cull_face;
      GlDraw.shade_model `smooth;
      Gl.enable `blend;

      (* dibuja *)
      let r, g, b, _ = difusa in
      GlDraw.color ~alpha:0.5 (r, g, b);
      (* pinta la luz *)
      GlFunc.blend_func ~src:`src_alpha ~dst:`one;
      GlDraw.polygon_mode ~face:`both `fill;
      GluQuadric.sphere ~radius:Defs.luz_marcador_tamanyo ~slices:4 ~stacks:2 ();
      (* dibuja el contorno *)
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
      GlDraw.polygon_mode ~face:`both `line;
      GluQuadric.sphere ~radius:Defs.luz_marcador_tamanyo ~slices:4 ~stacks:2 ();

      (* restaura los atributos anteriores *)
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ()

   (** Implementación de la interfaz [luz] *)
   method orientar ?(x = 0) ?(y = 0) ?(z = 0) () = "Las luces puntuales no se pueden orientar"

   (** Implementación de la interfaz [serializable] *)
   method clase = "puntual"

   (** Constructor del objeto *)
   initializer
      yo # from_xml xml
end

(** Luz direccional *)
class direccional (xml : Xml.xml) = object (yo)

   inherit luz (xml) as padre

   (** Orientación *)
   val mutable orientacion = new mat_rot

   (** Implementación de la interfaz [luz] *)
   method get_orientacion = orientacion # euler
   
   (** Implementación de la interfaz [luz] *)
   method set_orientacion (x, y, z) =
      orientacion # reset;
      orientacion # rotar_z (mod_float z 360.0);
      orientacion # rotar_y (mod_float y 360.0);
      orientacion # rotar_x (mod_float x 360.0)

   (** Implementación de la interfaz [luz] *)
   method orientar ?(x = 0) ?(y = 0) ?(z = 0) () =
      if z <> 0 then orientacion # rotar_z (mod_float (float z *. Defs.pixel_factor_obj_r) 360.0);
      if y <> 0 then orientacion # rotar_y (mod_float (float y *. Defs.pixel_factor_obj_r) 360.0);
      if x <> 0 then orientacion # rotar_x (mod_float (float x *. Defs.pixel_factor_obj_r) 360.0);

      let ox, oy, oz = orientacion # euler in
      Printf.sprintf "Orientación R(x,y,z) = (%.2f, %.2f, %.2f) grados" ox oy oz

   (** Implementación de la interfaz [luz] *)
   method iluminar =
      (* transforma la luz *)
      GlMat.push ();
      GlMat.translate3 yo # get_posicion;
      let ox, oy, oz = orientacion # euler in
      GlMat.rotate     ~angle:ox ~x:1.0 (); 
      GlMat.rotate     ~angle:oy ~y:1.0 ();
      GlMat.rotate     ~angle:oz ~z:1.0 ();

      (* ilumina *)
      let _, _, f_atenuacion = atenuacion in f_atenuacion ();
      GlLight.light ~num:numero (`specular especular);
      GlLight.light ~num:numero (`diffuse  difusa   );
      GlLight.light ~num:numero (`ambient  ambiente );
      GlLight.light ~num:numero (`position (0.0, 0.0, 1.0, 0.0));

      (* deshace la transformación *)
      GlMat.pop ()

   (** Implementación de la interfaz [luz] *)
   method dibujar =
      (* guarda los atributos anteriores *)
      GlMisc.push_attrib [`lighting];
      GlMisc.push_attrib [`depth_buffer];
      GlMisc.push_attrib [`polygon];
      GlMisc.push_attrib [`color_buffer];
      (* establece los atributos *)
      Gl.disable `lighting;
      Gl.disable `depth_test;
      Gl.disable `cull_face;
      GlDraw.shade_model `smooth;
      Gl.enable `blend;

      (* dibuja *)
      let r, g, b, _ = difusa
      in
      GlDraw.color ~alpha:0.5 (r, g, b);
      (* pinta la luz *)
      GlFunc.blend_func ~src:`src_alpha ~dst:`one;
      GlDraw.polygon_mode ~face:`both `fill;
      GlMat.push ();
      GlMat.translate ~z:(-.Defs.luz_marcador_tamanyo) ();
      (* tubo *)
      GluQuadric.cylinder ~base:Defs.luz_marcador_tamanyo ~top:Defs.luz_marcador_tamanyo
         ~height:(2. *. Defs.luz_marcador_tamanyo) ~slices:32 ~stacks:1 ();
      GlMat.rotate ~angle:180.0 ~y:1.0 ();
      (* gorro *)
      GluQuadric.cylinder ~base:(2. *. Defs.luz_marcador_tamanyo) ~top:0.0
         ~height:(2. *. Defs.luz_marcador_tamanyo) ~slices:32 ~stacks:1 ();
      GlMat.pop ();
      (* dibuja el contorno *)
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
      GlDraw.polygon_mode ~face:`both `line;
      let pasos = 32 in
      (* tubo *)
      GlDraw.begins `line_loop;
         for i = 0 to pasos - 1 do
            let delta = Func.grd2rad (float i *. 360.0 /. (float pasos)) in
            let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
            GlDraw.vertex3 (sin delta *. Defs.luz_marcador_tamanyo,
               cos delta *. Defs.luz_marcador_tamanyo,
               Defs.luz_marcador_tamanyo)
         done;
      GlDraw.ends ();
      GlDraw.begins `line_loop;
         for i = 0 to pasos - 1 do
            let delta = Func.grd2rad (float i *. 360.0 /. (float pasos)) in
            let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
            GlDraw.vertex3 (sin delta *. 2. *. Defs.luz_marcador_tamanyo,
               cos delta *. 2. *. Defs.luz_marcador_tamanyo,
               -.Defs.luz_marcador_tamanyo)
         done;
      GlDraw.ends ();
      GlDraw.begins `line_loop;
         for i = 0 to pasos - 1 do
            let delta = Func.grd2rad (float i *. 360.0 /. (float pasos)) in
            let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
            GlDraw.vertex3 (sin delta *. Defs.luz_marcador_tamanyo,
               cos delta *. Defs.luz_marcador_tamanyo,
               -.Defs.luz_marcador_tamanyo)
         done;
      GlDraw.ends ();

      (* restaura los atributos anteriores *)
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ()

   (** Implementación de la interfaz [serializable] *)
   method clase = "direccional"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      padre # from_xml xml;
      let ori = List.nth (Xml.children xml) 5 (* orientacion *)
      and ga nodo atrib = float_of_string (Xml.attrib nodo atrib)
      in
      let orientacion = ga ori "x", ga ori "y", ga ori "z" in
      yo # set_orientacion orientacion

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let ori_x, ori_y, ori_z = yo # get_orientacion
      in
      let ori = Xml.Element ("orientacion",
         [("x", Defs.float2xml ori_x); ("y", Defs.float2xml ori_y); ("z", Defs.float2xml ori_z)],
         [])
      in
      let xml = padre # to_xml in
      Xml.Element (Xml.tag xml, Xml.attribs xml, List.append (Xml.children xml) [ori])

   (** Constructor del objeto *)
   initializer
      yo # from_xml xml
end

(** Luz focal *)
class focal (xml : Xml.xml) = object (yo)

   inherit luz (xml) as padre

   (** Intensidad *)
   val mutable intensidad = 0.0

   (** Apertura en grados *)
   val mutable apertura = 180.0

   (** Orientación *)
   val mutable orientacion = new mat_rot

   (** Selector de la intensidad *)
   method get_intensidad = intensidad

   (** Modificador de la intensidad *)
   method set_intensidad i =
      intensidad <- if i < 0.0 then 0.0 else if 128.0 < i then 128.0 else i

   (** Selector de la apertura *)
   method get_apertura = apertura

   (** Modificador de la apertura *)
   method set_apertura a = apertura <-
      if a < 0.0 then
         0.0
      else if a = 180.0 then
         180.0
      else if 90.0 < a then
         90.0
      else
         a

   (** Implementación de la interfaz [luz] *)
   method get_orientacion = orientacion # euler

   (** Implementación de la interfaz [luz] *)
   method set_orientacion (x, y, z) =
      orientacion # reset;
      orientacion # rotar_z (mod_float z 360.0);
      orientacion # rotar_y (mod_float y 360.0);
      orientacion # rotar_x (mod_float x 360.0)

   (** Implementación de la interfaz [luz] *)
   method orientar ?(x = 0) ?(y = 0) ?(z = 0) () =
      if z <> 0 then orientacion # rotar_z (mod_float (float z *. Defs.pixel_factor_obj_r) 360.0);
      if y <> 0 then orientacion # rotar_y (mod_float (float y *. Defs.pixel_factor_obj_r) 360.0);
      if x <> 0 then orientacion # rotar_x (mod_float (float x *. Defs.pixel_factor_obj_r) 360.0);

      let ox, oy, oz = orientacion # euler in
      Printf.sprintf "Orientación R(x,y,z) = (%.2f, %.2f, %.2f) grados" ox oy oz

   (** Implementación de la interfaz [luz] *)
   method iluminar =
      (* transforma la luz *)
      GlMat.push ();
      GlMat.translate3 yo # get_posicion;
      let ox, oy, oz = orientacion # euler in
      GlMat.rotate     ~angle:ox ~x:1.0 (); 
      GlMat.rotate     ~angle:oy ~y:1.0 ();
      GlMat.rotate     ~angle:oz ~z:1.0 ();

      (* ilumina *)
      let _, _, f_atenuacion = atenuacion in f_atenuacion ();
      GlLight.light ~num:numero (`specular       especular);
      GlLight.light ~num:numero (`diffuse        difusa   );
      GlLight.light ~num:numero (`ambient        ambiente );
      GlLight.light ~num:numero (`position       (0.0, 0.0, 0.0, 1.0));
      GlLight.light ~num:numero (`spot_direction (0.0, 0.0, 1.0));
      GlLight.light ~num:numero (`spot_cutoff    apertura);
      GlLight.light ~num:numero (`spot_exponent  intensidad);

      (* deshace la transformación *)
      GlMat.pop ()

   (** Implementación de la interfaz [luz] *)
   method dibujar =
      (* guarda los atributos anteriores *)
      GlMisc.push_attrib [`lighting];
      GlMisc.push_attrib [`depth_buffer];
      GlMisc.push_attrib [`polygon];
      GlMisc.push_attrib [`color_buffer];
      (* establece los atributos *)
      Gl.disable `lighting;
      Gl.disable `depth_test;
      GlDraw.polygon_mode ~face:`both `fill;
      Gl.disable `cull_face;
      GlDraw.shade_model `smooth;
      Gl.enable `blend;

      (* dibuja *)
      let r, g, b, _ = difusa
      in
      GlDraw.color ~alpha:0.5 (r, g, b);
      if apertura > 90.0 then begin (* punto de luz *)
         (* pinta la luz *)
         GlFunc.blend_func ~src:`src_alpha ~dst:`one;
         GlDraw.polygon_mode ~face:`both `fill;
         GluQuadric.sphere ~radius:Defs.luz_marcador_tamanyo ~slices:4 ~stacks:2 ();
         (* dibuja el contorno *)
         GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
         GlDraw.polygon_mode ~face:`both `line;
         GluQuadric.sphere ~radius:Defs.luz_marcador_tamanyo ~slices:4 ~stacks:2 ();
      end else begin (* foco de luz *)
         (* pinta la luz *)
         GlFunc.blend_func ~src:`src_alpha ~dst:`one;
         GlDraw.polygon_mode ~face:`both `fill;
         (* tubo *)
         let ape = if apertura > 90.0 then 22.5 else apertura /. 2.
         and alt = 4. *. Defs.luz_marcador_tamanyo in
         GluQuadric.cylinder ~base:0.0 ~top:(tan (Func.grd2rad ape) *. alt)
            ~height:alt ~slices:32 ~stacks:1 ();
         (* dibuja el contorno *)
         GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
         GlDraw.polygon_mode ~face:`both `line;
         (* tubo *)
         let ape   = if apertura > 90.0 then 22.5 else apertura /. 2.
         and rad   = tan (Func.grd2rad ape) *. alt
         and pasos = 32
         in
         GlDraw.begins `line_loop;
            for i = 0 to pasos - 1 do
               let delta = Func.grd2rad (float i *. 360.0 /. (float pasos)) in
               let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
               GlDraw.vertex3 (sin delta *. rad, cos delta *. rad, alt)
            done;
         GlDraw.ends ()
      end;

      (* restaura los atributos anteriores *)
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ();
      GlMisc.pop_attrib ()

   (** Implementación de la interfaz [serializable] *)
   method clase = "focal"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      padre # from_xml xml;
      let ori = List.nth (Xml.children xml) 5 (* orientacion *)
      and ga nodo atrib = float_of_string (Xml.attrib nodo atrib)
      in
      let intensidad  = ga xml "intensidad"
      and apertura    = ga xml "apertura"
      and orientacion = ga ori "x", ga ori "y", ga ori "z"
      in
      yo # set_intensidad  intensidad;
      yo # set_apertura    apertura;
      yo # set_orientacion orientacion

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let ori_x, ori_y, ori_z = yo # get_orientacion
      in
      let intensidad = "intensidad", Defs.float2xml yo # get_intensidad
      and apertura   = "apertura",   Defs.float2xml yo # get_apertura
      and ori = Xml.Element ("orientacion",
         [("x", Defs.float2xml ori_x); ("y", Defs.float2xml ori_y); ("z", Defs.float2xml ori_z)],
         [])
      in
      let xml = padre # to_xml in
      Xml.Element (Xml.tag xml,
         List.append (Xml.attribs xml) [intensidad; apertura],
         List.append (Xml.children xml) [ori])

   (** Constructor del objeto *)
   initializer
      yo # from_xml xml
end
