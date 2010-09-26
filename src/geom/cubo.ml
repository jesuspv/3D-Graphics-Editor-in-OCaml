open Dibujable    (** Clase [dibujable]    *)
open Serializable (** Clase [serializable] *)

(** Primitiva para los cubos *)
class cubo ~lado:(init_lado : float) = object (self)

   inherit dibujable as super
   inherit serializable

   (** Lado *)
   val mutable lado = 1.0

   (** Selector del lado *)
   method get_lado = lado
   (** Modificador del lado *)
   method set_lado lado' =
      if lado' > 0.0 then
         lado <- lado'
      else
         raise (Invalid_argument (self#clase ^ ":set_lado: lado no positivo"))

   (** Implementación de la interfaz [dibujable] *)
   method dibujar ~normales ~cara ?(sombra = false) () =
      (* calcula las normales *)
      let sqrt3 = sqrt 3. in
      (* (x,y,z)=(m|p,m|p,m|p) / p: +lado & m: -lado *) (* p=1 & m=0 *)
      let normal = (match normales with
         `Poligono -> (fun _ -> (fun () -> ()))
       | `Vertice  -> (fun spec ->
            let n = (2. *. float (spec / 100)         -. 1.) /. sqrt3,
                    (2. *. float ((spec / 10) mod 10) -. 1.) /. sqrt3,
                    (2. *. float (spec mod 10)        -. 1.) /. sqrt3
            in
            (fun () -> GlDraw.normal3 n)))
      in
      let ppp, ppm, mpm, mpp = normal 111, normal 110, normal 010, normal 011
      and pmp, pmm, mmm, mmp = normal 101, normal 100, normal 000, normal 001
      in

      if not sombra then material # pintar ~cara;
      let lado = lado /. 2.0 in
      GlDraw.begins `quads;
         (* Cara anterior *)
         GlDraw.normal3 (0.0, 0.0, 1.0);
         mpp (); GlDraw.vertex3(-.lado,   lado,   lado);
         mmp (); GlDraw.vertex3(-.lado, -.lado,   lado);
         pmp (); GlDraw.vertex3(  lado, -.lado,   lado);
         ppp (); GlDraw.vertex3(  lado,   lado,   lado);
         (* Cara posterior *)
         GlDraw.normal3 (0.0, 0.0, -1.0);
         mpm (); GlDraw.vertex3(-.lado,   lado, -.lado);
         ppm (); GlDraw.vertex3(  lado,   lado, -.lado);
         pmm (); GlDraw.vertex3(  lado, -.lado, -.lado);
         mmm (); GlDraw.vertex3(-.lado, -.lado, -.lado);
         (* Cara superior *)
         GlDraw.normal3 (0.0, 1.0, 0.0);
         mpp (); GlDraw.vertex3(-.lado,   lado,   lado);
         ppp (); GlDraw.vertex3(  lado,   lado,   lado);
         ppm (); GlDraw.vertex3(  lado,   lado, -.lado);
         mpm (); GlDraw.vertex3(-.lado,   lado, -.lado);
         (* Cara inferior *)
         GlDraw.normal3 (0.0, -1.0, 0.0);
         mmp (); GlDraw.vertex3(-.lado, -.lado,   lado);
         mmm (); GlDraw.vertex3(-.lado, -.lado, -.lado);
         pmm (); GlDraw.vertex3(  lado, -.lado, -.lado);
         pmp (); GlDraw.vertex3(  lado, -.lado,   lado);
         (* Cara derecha *)
         GlDraw.normal3 (1.0, 0.0, 0.0);
         ppp (); GlDraw.vertex3(  lado,   lado,   lado);
         pmp (); GlDraw.vertex3(  lado, -.lado,   lado);
         pmm (); GlDraw.vertex3(  lado, -.lado, -.lado);
         ppm (); GlDraw.vertex3(  lado,   lado, -.lado);
         (* Cara izquierda *)
         GlDraw.normal3 (-1.0, 0.0, 0.0);
         mpp (); GlDraw.vertex3(-.lado,   lado,   lado);
         mpm (); GlDraw.vertex3(-.lado,   lado, -.lado);
         mmm (); GlDraw.vertex3(-.lado, -.lado, -.lado);
         mmp (); GlDraw.vertex3(-.lado, -.lado,   lado);
      GlDraw.ends()

   (** Implementación de la interfaz [dibujable] *)
   method get_extremo =
      let l = lado /. 2. in
      sqrt ((sx *. l) ** 2. +. (sy *. l) ** 2. +. (sz *. l) ** 2.)

   (** Implementación de la interfaz [dibujable] *)
   method get_limites = sx *. lado /. 2., sy *. lado /. 2., sz *. lado /. 2.

   (** Implementación de la interfaz [serializable] *)
   method clase = "cubo"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      if Xml.tag xml = self#clase then begin
         material # from_xml    (List.nth (Xml.children xml) 0); (* material       *)
         super # from_xml       (List.nth (Xml.children xml) 1); (* transformacion *)
         self  # set_nombre     (Xml.attrib xml "nombre");
         self  # set_lado (float_of_string (Xml.attrib xml "lado"))
      end else
         raise (Invalid_argument (self#clase ^ ":from_xml: la etiqueta XML es incorrecta"))

   (** Implementación de la interfaz [serializable] *)
   method to_xml = Xml.Element (self#clase,
      ["nombre", nombre; "lado", (Defs.float2xml lado)],
      [material # to_xml; super # to_xml])

   (** Constructor del objeto *)
   initializer
      self # set_lado init_lado
end
