open Material       (** Clase [material] *)
open Transformacion (** Clase [transformacion] *)
open Serializable   (** Clase [serializable]   *)

(** Interfaz para los objetos dibujables *)
class virtual dibujable = object (self)

   inherit transformacion as super
   inherit serializable

   (** Nombre del objeto *)
   val mutable nombre = ""

   (** Material para el objeto *)
   val mutable material = new material ()

   (** Array de display lists (una por vista) *)
   val mutable display_lists = Array.make_matrix 4 8 None

   (** Establece la display list del número indicado *)
   method set_display_list (num : int) =
      (* elimina las antiguas display lists *)
      let dls = display_lists.(num) in
      Array.iter (function (Some dl) -> GlList.delete dl | None -> ()) dls;

      (* crea las nuevas display lists *)
      let dls = List.map
         (fun (a, b, c) ->
            let dl = GlList.create `compile in
            GlList.begins dl ~mode:`compile;
            self # dibujar ~normales:a ~cara:b ~sombra:c ();
            GlList.ends ();
            Some dl)
         [
            (`Poligono, `front, false);
            (`Poligono, `front, true);
            (`Poligono, `back,  false);
            (`Poligono, `back,  true);
            (`Vertice,  `front, false);
            (`Vertice,  `front, true);
            (`Vertice,  `back,  false);
            (`Vertice,  `back,  true)
         ]
      in
      display_lists.(num) <- Array.of_list dls

   (** Elimina todas las display lists *)
   method clear_display_lists =
      Array.iter
         (Array.iter
            (function (Some dl) -> GlList.delete dl | None -> ()))
         display_lists

   (** Dibuja la Display List *)
   method call_display_list (num : int)
         ~(normales : [`Poligono | `Vertice])
         ~(cara : [`front | `back])
         ?(sombra = false) () =
      let i =
         if normales = `Poligono then
            if cara = `front then
               if sombra = false then
                  0
               else
                  1
            else
               if sombra = false then
                  2
               else
                  3
         else
            if cara = `front then
               if sombra = false then
                  4
               else
                  5
            else
               if sombra = false then
                  6
               else
                  7
      in
      match display_lists.(num).(i) with
         None    -> ()
       | Some dl -> GlList.call dl

   (** Selector del nombre *)
   method get_nombre = nombre

   (** Modificador del nombre *)
   method set_nombre (n : string) = nombre <- n

   (** Selector del material *)
   method get_material = material

   (** Modificador del material *)
   method set_material m = material <- m

   (** Dibuja la primitiva *)
   method virtual dibujar : normales:[`Poligono | `Vertice] -> cara:Gl.face -> ?sombra:bool -> unit -> unit

   (** Radio en el sistema de coordenadas local del objeto *)
   method virtual get_extremo : float

   (** Límites en el sistema de coordenadas local del objeto *)
   method virtual get_limites : Gl.vect3

   (** Implementación de la interfaz [serializable] *)
   method clase = "dibujable"

   (** Implementación de la interfaz [serializable] *)
   method from_xml xml =
      super # from_xml xml

   (** Implementación de la interfaz [serializable] *)
   method to_xml =
      super # to_xml

end
