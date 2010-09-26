(** Funciones de utilidad *)

(** Conversión de grados a radianes *)
let grd2rad (grados : float) : float = grados *. Defs.pi /. 180.0

(** Conversión de radianes a grados *)
let rad2grd (radianes : float) : float = radianes *. 180.0 /. Defs.pi

(** Imprime un vector *)
let print_v (x, y, z : float * float * float) : unit =
   Printf.printf "(%f\t%f\t%f)\n" x y z

(** Imprime una matriz *)
let print_m (m : GlMat.t) : unit =
   let raw   = GlMat.to_raw m in
   let get p = Raw.get_float raw ~pos:p in

   Printf.printf "(%f\t%f\t%f\t%f\n"  (get  0) (get  1) (get  2) (get  3);
   Printf.printf " %f\t%f\t%f\t%f\n"  (get  4) (get  5) (get  6) (get  7);
   Printf.printf " %f\t%f\t%f\t%f\n"  (get  8) (get  9) (get 10) (get 11);
   Printf.printf " %f\t%f\t%f\t%f)\n" (get 12) (get 13) (get 14) (get 15)

(** Normaliza el vector 2D *)
let normalizar2 (x, y : Gl.point2) : Gl.point2 =
   let modulo = sqrt (x ** 2. +. y ** 2.) in
   x /. modulo, y /. modulo

(** Normaliza el vector 3D *)
let normalizar3 (x, y, z : Gl.vect3) : Gl.vect3 =
   let modulo = sqrt (x ** 2. +. y ** 2. +. z ** 2.) in
   x /. modulo, y /. modulo, z /. modulo

(** [Func.vectorial_v v1 v2]Obtiene el vector perpendical a otros dos.
    El sentido de la normal viene dado por la regla de la mano derecha desde
    v1 hacia v2. *)
let vectorial_v (x1, y1, z1 : Gl.vect3) (x2, y2, z2 : Gl.vect3) : Gl.vect3 =
   let x, y, z =
      (z2 *. y1) -. (y2 *. z1),
      (x2 *. z1) -. (z2 *. x1),
      (y2 *. x1) -. (x2 *. y1)
   in
   let modulo = sqrt (x ** 2. +. y ** 2. +. z ** 2.) in
   x /. modulo, y /. modulo, z /. modulo

(** Obtiene el vector perpendical a otros dos dados.
    El sentido de la normal viene dado por la regla de la mano derecha desde
    v0v1 hacia v0v2. *)
let vectorial_p (x0, y0, z0 : Gl.point3) (x1, y1, z1 : Gl.point3) (x2, y2, z2 : Gl.point3) : Gl.vect3 =
   let x, y, z =
      ((z2 -. z0) *. (y1 -. y0)) -. ((y2 -. y0) *. (z1 -. z0)),
      ((x2 -. x0) *. (z1 -. z0)) -. ((z2 -. z0) *. (x1 -. x0)),
      ((y2 -. y0) *. (x1 -. x0)) -. ((x2 -. x0) *. (y1 -. y0))
   in
   let modulo = sqrt (x ** 2. +. y ** 2. +. z ** 2.) in
   x /. modulo, y /. modulo, z /. modulo

(** Obtiene el color de la unión de los ejes dados *)
let get_color_union (ejes : [`X | `Y | `Z] list) : Gl.vect3 =
   if (List.mem `X ejes) && (List.mem `Y ejes) && (List.mem `Z ejes) then
      (1.0, 1.0, 1.0)
   else if (List.mem `X ejes) && (List.mem `Y ejes) then
      (1.0, 1.0, 0.0)
   else if (List.mem `Y ejes) && (List.mem `Z ejes) then
      (0.0, 1.0, 1.0)
   else if (List.mem `X ejes) && (List.mem `Z ejes) then
      (1.0, 0.0, 1.0)
   else if (List.mem `X ejes) then
      (1.0, 0.0, 0.0)
   else if (List.mem `Y ejes) then
      (0.0, 1.0, 0.0)
   else if (List.mem `Z ejes) then
      (0.0, 0.0, 1.0)
   else
      (0.0, 0.0, 0.0)

(** Genera la matriz de rotación correspondiente a los ángulos de euler dados *)
let get_mr3 (x, y, z : Gl.vect3) : GlMat.t =
   let mr3 = Raw.create `double ~len:16
   and a   = cos x
   and b   = sin x
   and c   = cos y
   and d   = sin y
   and e   = cos z
   and f   = sin z
   in
   let ad  = a *. d
   and bd  = b *. d
   in

   Raw.set_float mr3 ~pos:0  (  c *. e);
   Raw.set_float mr3 ~pos:1  (-.c *. f);
   Raw.set_float mr3 ~pos:2  d;
   Raw.set_float mr3 ~pos:3  0.0;
   Raw.set_float mr3 ~pos:4  (  bd *. e +. a *. f);
   Raw.set_float mr3 ~pos:5  (-.bd *. f +. a *. e);
   Raw.set_float mr3 ~pos:6  (-.b *. c);
   Raw.set_float mr3 ~pos:7  0.0;
   Raw.set_float mr3 ~pos:8  (-.ad *. e +. b *. f);
   Raw.set_float mr3 ~pos:9  (  ad *. f +. b *. e);
   Raw.set_float mr3 ~pos:10 (a *. c);
   Raw.set_float mr3 ~pos:11 0.0;
   Raw.set_float mr3 ~pos:12 0.0;
   Raw.set_float mr3 ~pos:13 0.0;
   Raw.set_float mr3 ~pos:14 0.0;
   Raw.set_float mr3 ~pos:15 1.0;

   GlMat.of_raw mr3

(** Genera la matriz de traslación *)
let get_mt3 (x, y, z : Gl.vect3) : GlMat.t =
   let mt3 = Raw.create `double ~len:16 in

   Raw.set_float mt3 ~pos:0  1.0;
   Raw.set_float mt3 ~pos:1  0.0;
   Raw.set_float mt3 ~pos:2  0.0;
   Raw.set_float mt3 ~pos:3  x;
   Raw.set_float mt3 ~pos:4  0.0;
   Raw.set_float mt3 ~pos:5  1.0;
   Raw.set_float mt3 ~pos:6  0.0;
   Raw.set_float mt3 ~pos:7  y;
   Raw.set_float mt3 ~pos:8  0.0;
   Raw.set_float mt3 ~pos:9  0.0;
   Raw.set_float mt3 ~pos:10 1.0;
   Raw.set_float mt3 ~pos:11 z;
   Raw.set_float mt3 ~pos:12 0.0;
   Raw.set_float mt3 ~pos:13 0.0;
   Raw.set_float mt3 ~pos:14 0.0;
   Raw.set_float mt3 ~pos:15 1.0;

   GlMat.of_raw mt3

(** Genera la matriz de escalado *)
let get_ms3 (x, y, z : Gl.vect3) : GlMat.t =
   let ms3 = Raw.create `double ~len:16 in

   Raw.set_float ms3 ~pos:0  x;
   Raw.set_float ms3 ~pos:1  0.0;
   Raw.set_float ms3 ~pos:2  0.0;
   Raw.set_float ms3 ~pos:3  0.0;
   Raw.set_float ms3 ~pos:4  0.0;
   Raw.set_float ms3 ~pos:5  y;
   Raw.set_float ms3 ~pos:6  0.0;
   Raw.set_float ms3 ~pos:7  0.0;
   Raw.set_float ms3 ~pos:8  0.0;
   Raw.set_float ms3 ~pos:9  0.0;
   Raw.set_float ms3 ~pos:10 z;
   Raw.set_float ms3 ~pos:11 0.0;
   Raw.set_float ms3 ~pos:12 0.0;
   Raw.set_float ms3 ~pos:13 0.0;
   Raw.set_float ms3 ~pos:14 0.0;
   Raw.set_float ms3 ~pos:15 1.0;

   GlMat.of_raw ms3

(** Obtiene la matriz traspuesta de una matriz *)
let get_traspuesta (m : GlMat.t) : GlMat.t =
   let mr  = GlMat.to_raw m
   and raw = Raw.create `double ~len:16
   in
   let g p   = Raw.get_float mr  ~pos:p
   and s p f = Raw.set_float raw ~pos:p f
   in

   s  0 (g 0); s  1 (g 4); s  2 (g  8); s  3 (g 12);
   s  4 (g 1); s  5 (g 5); s  6 (g  9); s  7 (g 13);
   s  8 (g 2); s  9 (g 6); s 10 (g 10); s 11 (g 14);
   s 12 (g 3); s 13 (g 7); s 14 (g 11); s 15 (g 15);

   GlMat.of_raw raw

(* Multiplica la matriz dado por el vector (cambio de coordenadas) *)
let mult_mv (m : GlMat.t) (v : Gl.vect3) : Gl.vect3 =
   let raw     = GlMat.to_raw m in
   let get p   = Raw.get_float raw ~pos:p
   and x, y, z = v
   in

   (
      (get 0) *. x +. (get 1) *. y +. (get  2) *. z +. (get  3),
      (get 4) *. x +. (get 5) *. y +. (get  6) *. z +. (get  7),
      (get 8) *. x +. (get 9) *. y +. (get 10) *. z +. (get 11)
   )

(** Dibuja el marcador de la traslación *)
let dibujar_marcador_traslacion (ejes : [`X | `Y | `Z] list) (longitud : float) : unit =
   (* guarda los atributos anteriores *)
   GlMisc.push_attrib [`lighting];
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`polygon];
   GlMisc.push_attrib [`line];
   (* establece los atributos *)
   Gl.disable `lighting;
   GlClear.clear [`depth];
   Gl.enable `depth_test;
   GlDraw.polygon_mode ~face:`both `fill;
   Gl.disable `cull_face;
   GlDraw.shade_model `smooth;
   Gl.enable `line_smooth;

   let lon = longitud and bas = 2.0 in

   (* selecciona el color central *)
   let color_centro = get_color_union ejes in

   (* dibuja los ejes *)
   GlDraw.line_width bas;
   GlDraw.begins `lines;
      (* OX *)
      if List.mem `X ejes then begin
         GlDraw.color   color_centro;
         GlDraw.vertex3 (0.0, 0.0, 0.0);
         GlDraw.color   (1.0, 0.0, 0.0);
         GlDraw.vertex3 (lon, 0.0, 0.0)
      end;
      (* OY *)
      if List.mem `Y ejes then begin
         GlDraw.color   color_centro;
         GlDraw.vertex3 (0.0, 0.0, 0.0);
         GlDraw.color   (0.0, 1.0, 0.0);
         GlDraw.vertex3 (0.0, lon, 0.0)
      end;
      (* OZ *)
      if List.mem `Z ejes then begin
         GlDraw.color   color_centro;
         GlDraw.vertex3 (0.0, 0.0, 0.0);
         GlDraw.color   (0.0, 0.0, 1.0);
         GlDraw.vertex3 (0.0, 0.0, lon)
      end;
   GlDraw.ends ();
   GlDraw.line_width 1.0;

   let fle_lon = 0.2 *. lon and fle_bas = 0.08 *. lon in

   (* flecha para OX *)
   if List.mem `X ejes then begin
      GlMat.push ();
      GlMat.translate3 (lon, 0.0, 0.0);
      GlMat.rotate ~angle:90.0 ~y:1.0 ();
      GlDraw.color (1.0, 0.0, 0.0);
      GluQuadric.cylinder ~base:fle_bas ~top:0.0 ~height:fle_lon ~slices:16 ~stacks:1 ();
      GlMat.pop ()
   end;
   (* flecha para OY *)
   if List.mem `Y ejes then begin
      GlMat.push ();
      GlMat.translate3 (0.0, lon, 0.0);
      GlMat.rotate ~angle:(-.90.0) ~x:1.0 ();
      GlDraw.color (0.0, 1.0, 0.0);
      GluQuadric.cylinder ~base:fle_bas ~top:0.0 ~height:fle_lon ~slices:16 ~stacks:1 ();
      GlMat.pop ()
   end;
   (* flecha para OZ *)
   if List.mem `Z ejes then begin
      GlMat.push ();
      GlMat.translate3 (0.0, 0.0, lon);
      GlDraw.color (0.0, 0.0, 1.0);
      GluQuadric.cylinder ~base:fle_bas ~top:0.0 ~height:fle_lon ~slices:16 ~stacks:1 ();
      GlMat.pop ()
   end;

   (* restaura los atributos anteriores *)
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ()

(** Dibuja el marcador de la rotación *)
let dibujar_marcador_rotacion (ejes : [`X | `Y | `Z] list) (radio : float) : unit =
   (* guarda los atributos anteriores *)
   GlMisc.push_attrib [`lighting];
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`polygon];
   GlMisc.push_attrib [`color_buffer];
   GlMisc.push_attrib [`line];
   (* establece los atributos *)
   Gl.disable `lighting;
   Gl.enable `depth_test;
   GlDraw.polygon_mode ~face:`both `fill;
   Gl.disable `cull_face;
   GlDraw.shade_model `smooth;
   Gl.enable `blend;
   GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;

   (* selecciona el color de las uniones *)
   let color_x, color_y, color_z = get_color_union ejes
   and alpha color = if color = 1.0 then 1.0 else 0.25
   in

   let lon = radio and bas = 2.0 and pasos = 64 in

   GlDraw.line_width bas;
   (* dibuja el plano OX *)
   GlDraw.begins `line_loop;
      for i = 0 to pasos - 1 do
         let delta = grd2rad (float i *. 360.0 /. (float pasos)) in
         let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
         GlDraw.color   ~alpha:(alpha color_x) (1.0, color_y *. color, color_z *. color);
         GlDraw.vertex3 (0.0, sin delta *. lon, cos delta *. lon)
      done;
   GlDraw.ends ();
   (* dibuja el plano OY *)
   GlDraw.begins `line_loop;
      for i = 0 to pasos - 1 do
         let delta = grd2rad (float i *. 360.0 /. (float pasos)) in
         let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
         GlDraw.color   ~alpha:(alpha color_y) (color_x *. color, 1.0, color_z *. color);
         GlDraw.vertex3 (sin delta *. lon, 0.0, cos delta *. lon)
      done;
   GlDraw.ends ();
   (* dibuja el plano OZ *)
   GlDraw.begins `line_loop;
      for i = 0 to pasos - 1 do
         let delta = grd2rad (float i *. 360.0 /. (float pasos)) in
         let color = abs_float (mod_float (4.0 *. delta /. Defs.pi) 2.0 -. 1.0) in
         GlDraw.color   ~alpha:(alpha color_z) (color_x *. color, color_y *. color, 1.0);
         GlDraw.vertex3 (sin delta *. lon, cos delta *. lon, 0.0)
      done;
   GlDraw.ends ();
   GlDraw.line_width 1.0;

   (* restaura los atributos anteriores *)
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ()

(** Dibuja el marcador del escalado *)
let dibujar_marcador_escalado (ejes : [`X | `Y | `Z] list) (longitud : Gl.vect3) : unit =
   (* guarda los atributos anteriores *)
   GlMisc.push_attrib [`lighting];
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`polygon];
   GlMisc.push_attrib [`color_buffer];
   GlMisc.push_attrib [`line];
   (* establece los atributos *)
   Gl.disable `lighting;
   Gl.enable `depth_test;
   GlDraw.polygon_mode ~face:`both `fill;
   Gl.disable `cull_face;
   GlDraw.shade_model `smooth;
   Gl.enable `blend;
   GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
   Gl.enable `line_smooth;

   (* selecciona el color de las uniones *)
   (* selecciona el color de las uniones *)
   let cox, coy, coz = get_color_union ejes
   and alpha color = if color = 1.0 then 1.0 else 0.25
   in

   let lsx, lsy, lsz = longitud and bas = 2.0 in

   (* dibuja el plano OX *)
   GlDraw.line_width bas;
   GlDraw.begins `line_loop;
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   0.0);
      GlDraw.vertex3 (0.0,   lsy,   lsz);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   coz);
      GlDraw.vertex3 (0.0,   lsy,   0.0);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   0.0);
      GlDraw.vertex3 (0.0,   lsy, -.lsz);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   coy,   0.0);
      GlDraw.vertex3 (0.0,   0.0, -.lsz);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   0.0);
      GlDraw.vertex3 (0.0, -.lsy, -.lsz);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   coz);
      GlDraw.vertex3 (0.0, -.lsy,   0.0);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   0.0,   0.0);
      GlDraw.vertex3 (0.0, -.lsy,   lsz);
      GlDraw.color   ~alpha:(alpha cox) (1.0,   coy,   0.0);
      GlDraw.vertex3 (0.0,   0.0,   lsz);
   GlDraw.ends ();
   (* dibuja el plano OY *)
   GlDraw.begins `line_loop;
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   0.0);
      GlDraw.vertex3 (-.lsx, 0.0, -.lsz);
      GlDraw.color   ~alpha:(alpha coy) (  cox, 1.0,   0.0);
      GlDraw.vertex3 (  0.0, 0.0, -.lsz);
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   0.0);
      GlDraw.vertex3 (  lsx, 0.0, -.lsz);
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   coz);
      GlDraw.vertex3 (  lsx, 0.0,   0.0);
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   0.0);
      GlDraw.vertex3 (  lsx, 0.0,   lsz);
      GlDraw.color   ~alpha:(alpha coy) (  cox, 1.0,   0.0);
      GlDraw.vertex3 (  0.0, 0.0,   lsz);
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   0.0);
      GlDraw.vertex3 (-.lsx, 0.0,   lsz);
      GlDraw.color   ~alpha:(alpha coy) (  0.0, 1.0,   coz);
      GlDraw.vertex3 (-.lsx, 0.0,   0.0);
   GlDraw.ends ();
   (* dibuja el plano OZ *)
   GlDraw.begins `line_loop;
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   0.0, 1.0);
      GlDraw.vertex3 (-.lsx,   lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  cox,   0.0, 1.0);
      GlDraw.vertex3 (  0.0,   lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   0.0, 1.0);
      GlDraw.vertex3 (  lsx,   lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   coy, 1.0);
      GlDraw.vertex3 (  lsx,   0.0, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   0.0, 1.0);
      GlDraw.vertex3 (  lsx, -.lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  cox,   0.0, 1.0);
      GlDraw.vertex3 (  0.0, -.lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   0.0, 1.0);
      GlDraw.vertex3 (-.lsx, -.lsy, 0.0);
      GlDraw.color   ~alpha:(alpha coz) (  0.0,   coy, 1.0);
      GlDraw.vertex3 (-.lsx,   0.0, 0.0);
   GlDraw.ends ();
   GlDraw.line_width 1.0;

   (* restaura los atributos anteriores *)
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ()
