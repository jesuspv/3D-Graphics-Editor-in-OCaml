(** Matriz de rotación *)
class mat_rot = object (self)

   (** Matriz de rotación *)
   val mutable matriz =
      let rot = Array.make_matrix 4 4 0.0 in
      rot.(0).(0) <- 1.0;
      rot.(1).(1) <- 1.0;
      rot.(2).(2) <- 1.0;
      rot.(3).(3) <- 1.0;
      rot

   (** Rota sobre el eje X *)
   method rotar_x (grados : float) =
      let angulo = Func.grd2rad grados
      and rotx   = Array.make_matrix 4 4 0.0
      in
      rotx.(0).(0) <- 1.0;
      rotx.(1).(1) <- cos angulo;
      rotx.(1).(2) <- -.(sin angulo);
      rotx.(2).(1) <- sin angulo;
      rotx.(2).(2) <- cos angulo;
      rotx.(3).(3) <- 1.0;

      matriz <- self # multiplicar rotx matriz

   (** Rota sobre el eje Y *)
   method rotar_y (grados : float) =
      let angulo = Func.grd2rad grados
      and roty   = Array.make_matrix 4 4 0.0
      in
      roty.(0).(0) <- cos angulo;
      roty.(0).(2) <- sin angulo;
      roty.(1).(1) <- 1.0;
      roty.(2).(0) <- -.(sin angulo);
      roty.(2).(2) <- cos angulo;
      roty.(3).(3) <- 1.0;

      matriz <- self # multiplicar roty matriz

   (** Rota sobre el eje Z *)
   method rotar_z (grados : float) =
      let angulo = Func.grd2rad grados
      and rotz   = Array.make_matrix 4 4 0.0
      in
      rotz.(0).(0) <- cos angulo;
      rotz.(0).(1) <- -.(sin angulo);
      rotz.(1).(0) <- sin angulo;
      rotz.(1).(1) <- cos angulo;
      rotz.(2).(2) <- 1.0;
      rotz.(3).(3) <- 1.0;

      matriz <- self # multiplicar rotz matriz

   (** Inicializa la matriz *)
   method reset =
      let rot = Array.make_matrix 4 4 0.0 in
      rot.(0).(0) <- 1.0;
      rot.(1).(1) <- 1.0;
      rot.(2).(2) <- 1.0;
      rot.(3).(3) <- 1.0;
      matriz <- rot

   (** Obtiene la matriz de rotación *)
   method get = GlMat.of_array matriz

   (** Obtiene los ángulos de Euler *)
   method euler =
      (* Calculate Y-axis angle *)
      let angle_y = Func.rad2grd (asin matriz.(0).(2)) in
      let c       = cos (asin matriz.(0).(2)) in
      (* gimball lock? *)
      let angle_x, angle_z =
         if abs_float c > 0.005 then begin
            let angle_x =
               let tr_x =   matriz.(2).(2) /. c in (* No, so get X-axis angle *)
               let tr_y = -.matriz.(1).(2) /. c in
               Func.rad2grd (atan2 tr_y tr_x)
            in
            let angle_z =
               let tr_x =   matriz.(0).(0) /. c in (* Get Z-axis angle *)
               let tr_y = -.matriz.(0).(1) /. c in
               Func.rad2grd (atan2 tr_y tr_x)
            in
            (angle_x, angle_z)
         end else begin (* Gimball lock has occurred *)
            let angle_x  = 0.0 in                  (* Set X-axis angle to zero *)
            let tr_x     =  matriz.(1).(1) in      (* And calculate Z-axis angle *)
            let tr_y     =  matriz.(1).(0) in
            let angle_z  = Func.rad2grd (atan2 tr_y tr_x) in
            (angle_x, angle_z)
         end
      in
      (angle_x, angle_y, angle_z)

   (** Multiplica dos matrices m x l *)
   method private multiplicar m l =
      let r = Array.make_matrix 4 4 0.0 in
      for i = 0 to 3 do
         for j = 0 to 3 do
            r.(i).(j) <-
               m.(i).(0) *. l.(0).(j) +.
               m.(i).(1) *. l.(1).(j) +.
               m.(i).(2) *. l.(2).(j) +.
               m.(i).(3) *. l.(3).(j)
         done
      done;
      r
end
