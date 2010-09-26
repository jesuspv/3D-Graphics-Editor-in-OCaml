open MatRot       (** Clase [mat_rot]      *)
open Serializable (** Clase [serializable] *)

(** Definición XML de la transformación por defecto *)
let transformacion_defecto : Xml.xml =
   let t = Xml.Element ("traslacion",
      [("x", "0.0"); ("y", "0.0"); ("z", "0.0")],
      [])
   and r = Xml.Element ("rotacion",
      [("x", "0.0"); ("y", "0.0"); ("z", "0.0")],
      [])
   and s = Xml.Element ("escalado",
      [("x", "1.0"); ("y", "1.0"); ("z", "1.0")],
      [])
   in
   Xml.Element ("transformacion", [], [t; r; s])

(** Transformaciones de traslación, rotación y escalado *)
class transformacion = object (self)

   inherit serializable

   (** Traslación sobre el eje *)
   val mutable tx = 0.0
   val mutable ty = 0.0
   val mutable tz = 0.0

   (** Matriz de rotación *)
   val mutable rot = new mat_rot

   (** Escalado del eje *)
   val mutable sx = 0.0
   val mutable sy = 0.0
   val mutable sz = 0.0

   (** Selector de la traslación *)
   method get_tx = tx
   method get_ty = ty
   method get_tz = tz
   method get_t  = (tx, ty, tz)

   (** Modificador de la traslación *)
   method set_t ?(x = tx) ?(y = ty) ?(z = tz) () =
      tx <- x;
      ty <- y;
      tz <- z

   (** Selector de la rotación *)
   method get_rx = let gx, _, _ = rot # euler in gx
   method get_ry = let _, gy, _ = rot # euler in gy
   method get_rz = let _, _, gz = rot # euler in gz
   method get_r  = rot # euler

   (** Modificador de la rotación *)
   method set_r
         ?(x = self # get_rx)
         ?(y = self # get_ry)
         ?(z = self # get_rz) () =
      rot # reset;
      rot # rotar_z (mod_float z 360.0);
      rot # rotar_y (mod_float y 360.0);
      rot # rotar_x (mod_float x 360.0)

   (** Selector del escalado *)
   method get_sx = sx
   method get_sy = sy
   method get_sz = sz
   method get_s  = (sx, sy, sz)

   (** Modificador del escalado *)
   method set_s ?(x = sx) ?(y = sy) ?(z = sz) () =
      sx <- x;
      sy <- y;
      sz <- z

   (** Traslación sobre el eje *)
   method trasladar ?(x = 0) ?(y = 0) ?(z = 0) () =
      tx <- tx +. (Defs.pixel_factor_obj_t *. (float x));
      ty <- ty +. (Defs.pixel_factor_obj_t *. (float y));
      tz <- tz +. (Defs.pixel_factor_obj_t *. (float z));
      Printf.sprintf "Traslación T(x,y,z) = (%.2f, %.2f, %.2f)"
         (self # get_tx)
         (self # get_ty)
         (self # get_tz)

   (** Rotación sobre el eje *)
   method rotar ?(x = 0) ?(y = 0) ?(z = 0) () =
      if z <> 0 then rot # rotar_z (mod_float (float z *. Defs.pixel_factor_obj_r) 360.0);
      if y <> 0 then rot # rotar_y (mod_float (float y *. Defs.pixel_factor_obj_r) 360.0);
      if x <> 0 then rot # rotar_x (mod_float (float x *. Defs.pixel_factor_obj_r) 360.0);

      let rx, ry, rz = rot # euler in
      Printf.sprintf "Rotación R(x,y,z) = (%.2f, %.2f, %.2f) grados" rx ry rz

   (** Escalado sobre el eje *)
   method escalar ?(x = 0) ?(y = 0) ?(z = 0) () =
      sx <- sx +. (Defs.pixel_factor_obj_s *. (float x));
      sy <- sy +. (Defs.pixel_factor_obj_s *. (float y));
      sz <- sz +. (Defs.pixel_factor_obj_s *. (float z));
      Printf.sprintf "Escalado S(x,y,z) = (%.2f, %.2f, %.2f)"
         (self # get_sx)
         (self # get_sy)
         (self # get_sz)

   (** Implementación de la interfaz [serializable] *)
   method clase = "transformacion"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = "transformacion" then begin
         let hijos = Xml.children xml in
         let t     = List.nth hijos 0 (* traslación *)
         and r     = List.nth hijos 1 (* rotación   *)
         and s     = List.nth hijos 2 (* escalado   *)
         and get_float nodo atrib = float_of_string (Xml.attrib nodo atrib)
         in
         let tx, ty, tz = (get_float t "x"), (get_float t "y"), (get_float t "z")
         and rx, ry, rz = (get_float r "x"), (get_float r "y"), (get_float r "z")
         and sx, sy, sz = (get_float s "x"), (get_float s "y"), (get_float s "z")
         in
         self # set_t ~x:tx ~y:ty ~z:tz ();
         self # set_r ~x:rx ~y:ry ~z:rz ();
         self # set_s ~x:sx ~y:sy ~z:sz ()
      end else
         raise (Invalid_argument ("transformacion:from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      let rx, ry, rz = rot # euler in
      let t = Xml.Element ("traslacion",
         [("x", Defs.float2xml tx); ("y", Defs.float2xml ty); ("z", Defs.float2xml tz)],
         [])
      and r = Xml.Element ("rotacion",
         [("x", Defs.float2xml rx); ("y", Defs.float2xml ry); ("z", Defs.float2xml rz)],
         [])
      and s = Xml.Element ("escalado",
         [("x", Defs.float2xml sx); ("y", Defs.float2xml sy); ("z", Defs.float2xml sz)],
         [])
      in
      Xml.Element ("transformacion", [], [t; r; s])
end
