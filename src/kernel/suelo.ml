(* Suelo de la escena en el plano XZ *)

(* Display lists para la rejilla alámbrica *)
let alambrico = Array.make 4 None

(* Display lists para el suelo sólido *)
let solido = Array.make 4 None

(* Dibuja el suelo alámbrico del plano XZ *)
let dibujar_alambrico () : unit =
   GlMat.push ();
   GlMisc.push_attrib [`lighting];
   GlMisc.push_attrib [`depth_buffer];
   GlMisc.push_attrib [`line];
   Gl.disable `lighting;
   Gl.enable  `depth_test;
   GlDraw.begins `lines;
   Gl.enable `line_smooth;

   let lon = Defs.escena_grid_tam /. 2. in
   (* dibuja la rejilla *)
   GlDraw.color Defs.escena_grid_color;
   let f i = i *. (Defs.escena_grid_tam /. (float Defs.escena_grid_divs)) -. lon in
   for i = 1 to Defs.escena_grid_divs / 2 - 1 do
      let x = f (float i) in
      GlDraw.vertex ~x ~y:(-.lon) ~z:0.0 ();
      GlDraw.vertex ~x ~y:(  lon) ~z:0.0 ();
   done;
   for i = Defs.escena_grid_divs / 2 + 1 to Defs.escena_grid_divs - 1 do
      let x = f (float i) in
      GlDraw.vertex ~x ~y:(-.lon) ~z:0.0 ();
      GlDraw.vertex ~x ~y:(  lon) ~z:0.0 ();
   done;
   for i = 1 to Defs.escena_grid_divs / 2 - 1 do
      let y = f (float i) in
      GlDraw.vertex ~x:(-.lon) ~y ~z:0.0 ();
      GlDraw.vertex ~x:(  lon) ~y ~z:0.0 ();
   done;
   for i =  Defs.escena_grid_divs / 2 + 1 to Defs.escena_grid_divs - 1 do
      let y = f (float i) in
      GlDraw.vertex ~x:(-.lon) ~y ~z:0.0 ();
      GlDraw.vertex ~x:(  lon) ~y ~z:0.0 ();
   done;   

   (* dibuja los límites *)
   GlDraw.color Defs.escena_grid_limites_color;
   GlDraw.vertex ~x:(-.lon) ~y:(-.lon) ~z:0. ();
   GlDraw.vertex ~x:(-.lon) ~y:(  lon) ~z:0. ();
   GlDraw.vertex ~x:(  lon) ~y:(-.lon) ~z:0. ();
   GlDraw.vertex ~x:(  lon) ~y:(  lon) ~z:0. ();
   GlDraw.vertex ~x:(-.lon) ~y:(-.lon) ~z:0. ();
   GlDraw.vertex ~x:(  lon) ~y:(-.lon) ~z:0. ();
   GlDraw.vertex ~x:(-.lon) ~y:(  lon) ~z:0. ();
   GlDraw.vertex ~x:(  lon) ~y:(  lon) ~z:0. ();
   (* dibuja los ejes *)
   GlDraw.color Defs.escena_grid_ejes_color;
   GlDraw.vertex ~x:(0.) ~y:(-.lon) ~z:0. ();
   GlDraw.vertex ~x:(0.) ~y:(  lon) ~z:0. ();
   GlDraw.vertex ~x:(-.lon) ~y:(0.) ~z:0. ();
   GlDraw.vertex ~x:(  lon) ~y:(0.) ~z:0. ();

   GlDraw.ends ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMisc.pop_attrib ();
   GlMat.pop ()

(** Dibuja el suelo como un tablero de ajedrez *)
let dibujar_solido () : unit =
   (* opacidad *)
   let alpha = Defs.escena_suelo_alpha in
   (* Patrón para el suelo *)
   let ajedrezado =
      let image = GlPix.create `ubyte ~format:`rgb ~width:2 ~height:2 in
      Raw.sets (GlPix.to_raw image) ~pos:0 Defs.escena_suelo_color1;
      Raw.sets (GlPix.to_raw image) ~pos:3 Defs.escena_suelo_color2;
      Raw.sets (GlPix.to_raw image) ~pos:6 Defs.escena_suelo_color2;
      Raw.sets (GlPix.to_raw image) ~pos:9 Defs.escena_suelo_color1;
      image
   in
   (* Dibuja el suelo *)
   let dibujar_suelo () : unit =
      GlMisc.push_attrib [`polygon];
      GlDraw.polygon_mode ~face:`both `fill;
      Gl.disable `cull_face;

      let d = Defs.escena_grid_tam /. 2.0
      and n = float (Defs.escena_grid_divs / 2)
      in
      GlDraw.begins `quads;
         GlDraw.normal3 (0.0, 1.0, 0.0);
         GlDraw.color ~alpha Defs.escena_fondo_color;
         GlTex.coord2 (n, 0.0);
         GlDraw.vertex3 (  d, 0.0,   d);
         GlDraw.color ~alpha Defs.escena_fondo_color;
         GlTex.coord2 (n, n);
         GlDraw.vertex3 (  d, 0.0, -.d);
         GlDraw.color ~alpha Defs.escena_fondo_color;
         GlTex.coord2 (0.0, n);
         GlDraw.vertex3 (-.d, 0.0, -.d);
         GlDraw.color ~alpha Defs.escena_fondo_color;
         GlTex.coord2 (0.0, 0.0);
         GlDraw.vertex3 (-.d, 0.0,   d);
      GlDraw.ends ();
      GlMisc.pop_attrib ()
   in

   GlPix.store (`unpack_alignment 1);
   GlTex.image2d ajedrezado;
   List.iter (GlTex.parameter ~target:`texture_2d)
      [
         `wrap_s `repeat;
         `wrap_t `repeat;
         `mag_filter `nearest;
         `min_filter `nearest
      ];
   GlTex.env (`mode `decal);
   Gl.enable `texture_2d;
   GlDraw.shade_model `flat;
   dibujar_suelo ();
   Gl.disable `texture_2d

(* Interfaz [suelo.mli] *)
let set_display_list (num : int) : unit =
   (match alambrico.(num) with
      None -> ()
    | Some dl -> GlList.delete dl);
   (match solido.(num) with
      None -> ()
    | Some dl -> GlList.delete dl);

   let dl = GlList.create `compile in
   GlList.begins dl ~mode:`compile;
   dibujar_alambrico ();
   GlList.ends ();
   alambrico.(num) <- Some dl;

   let dl' = GlList.create `compile in
   GlList.begins dl' ~mode:`compile;
   dibujar_solido ();
   GlList.ends ();
   solido.(num) <- Some dl'

(* Interfaz [suelo.mli] *)
let call_display_list_alambrico (num : int) : unit =
   match alambrico.(num) with
     None    -> ()
   | Some dl -> GlList.call dl

(* Interfaz [suelo.mli] *)
let call_display_list_solido (num : int) : unit =
   match solido.(num) with
     None    -> ()
   | Some dl -> GlList.call dl
