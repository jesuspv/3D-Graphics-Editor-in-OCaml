(** Módulo que engloba las funcionalidades solicitables a cualquier escena *)

open Dibujable
open Camara
open Cubo
open Cilindro
open Cono
open Disco
open Esfera
open Extrusion
open Malla
open Revolucion
open Toro

(* Ventana principal *)
let ventana_principal : Widget.toplevel Widget.widget list ref = ref []

(* Camaras desde las que visualizar la escena *)
let camaras : camara list ref = ref []

(* Vistas que representan la escena *)
let vistas : Togl.widget list ref = ref []

(* Geometría para representar *)
let geometria : dibujable list ref = ref []

(* Luces de la escena *)
let iluminacion : Luz.luz list ref = ref [(new Luz.ambiental Luz.ambiental_defecto :> Luz.luz)]

(* Interfaz [escena.mli] *)
let transformacion = ref `Situar

(* Interfaz [escena.mli] *)
let eje = ref `XZ

(* Indica si deben representarse los reflejos *)
let reflejos = ref false

(* Indica si deben representarse las sombras *)
let sombras = ref false

(* Marcador para las transformaciones de los objetos *)
let marcador : ([`Mover | `Rotar | `Escalar | `Vacio] * Gl.vect3 * Gl.vect3 * Gl.vect3) ref =
   ref (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0))

(*
 * tipo: marcador
 *)

(* Actualiza los datos del marcador de la transformación *)
let actualizar_marcador (objetivo, indice : [`Obj | `Luz] * int) (camara: camara) ~(info : bool) : unit =
   let escalar   (x, y, z) = x +. Defs.pixel_factor_obj_s, y +. Defs.pixel_factor_obj_s, z +. Defs.pixel_factor_obj_s
   and escalar_t (x, y, z) = 1.5 *. x, 1.5 *. y, 1.5 *. z
   in
   try
      (match objetivo, !transformacion with
          _, `Situar | _, `Girar | _, `Zoom ->
          marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0));
          if info then Estado.mostrar ("Cámara " ^ (camara # get_nombre) ^ ": pulsar <Mouse2> para ampliar/reducir la vista")
       | `Obj, `Mover -> marcador := (`Mover,
          (List.nth !geometria indice) # get_t,
          escalar_t (List.nth !geometria indice) # get_limites,
          (List.nth !geometria indice) # get_r)
       | `Obj, `Rotar -> marcador := (`Rotar,
          (List.nth !geometria indice) # get_t,
          escalar ((List.nth !geometria indice) # get_extremo,
             (List.nth !geometria indice) # get_extremo,
             (List.nth !geometria indice) # get_extremo),
          (List.nth !geometria indice) # get_r)
       | `Obj, `Escalar -> marcador := (`Escalar,
          (List.nth !geometria indice) # get_t,
          escalar (List.nth !geometria indice) # get_limites,
          (List.nth !geometria indice) # get_r)
       | `Luz, `Escalar -> marcador := (`Vacio,
          (0.0, 0.0, 0.0),
          (1.0, 1.0, 1.0),
          (0.0, 0.0, 0.0))
       | `Luz, `Mover   -> marcador := (`Mover,
          (List.nth !iluminacion indice) # get_posicion,
          (4.*.Defs.luz_marcador_tamanyo, 4.*.Defs.luz_marcador_tamanyo, 4.*.Defs.luz_marcador_tamanyo),
          (List.nth !iluminacion indice) # get_orientacion)
       | `Luz, `Rotar   -> marcador := (`Rotar,
          (List.nth !iluminacion indice) # get_posicion,
          (4.*.Defs.luz_marcador_tamanyo, 4.*.Defs.luz_marcador_tamanyo, 4.*.Defs.luz_marcador_tamanyo),
          (List.nth !iluminacion indice) # get_orientacion)
       | `Obj, `Borrar -> marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0));
          let obj = (List.nth !geometria indice) in
          let geometria' = ref [] in
          for i = 0 to List.length !geometria - 1 do
             if i <> indice then
                geometria' := List.append !geometria' [(List.nth !geometria i)]
             else
                (* elimina las display lists *)
                (List.nth !geometria i) # clear_display_lists;
          done;
          geometria := !geometria';
          Estado.mostrar ("Objeto " ^ obj # get_nombre ^ ": eliminado de la escena")
       | `Luz, `Borrar -> marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0));
          let luz = (List.nth !iluminacion indice) in
          let iluminacion' = ref [] in
          for i = 0 to List.length !iluminacion - 1 do
             if i <> indice then iluminacion' := List.append !iluminacion' [(List.nth !iluminacion i)];
          done;
          iluminacion := !iluminacion';
          decr Luz.ct_luces;
          Estado.mostrar ("Luz " ^ luz # get_nombre ^ ": eliminada de la escena"))
   with
      Invalid_argument _ ->
         marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0));
         Estado.mostrar ("Cámara " ^ (camara # get_nombre) ^ ": pulsar <Mouse2> para ampliar/reducir la vista")
    | Failure _ ->
         marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0));
         Estado.mostrar ("Cámara " ^ (camara # get_nombre) ^ ": pulsar <Mouse2> para ampliar/reducir la vista")

(* Dibuja el marcador de la transformación *)
let dibujar_marcador () : unit =
   let listar ejes = match ejes with
      `X -> [`X]
    | `Y -> [`Y]
    | `Z -> [`Z]
    | `XY -> [`X; `Y]
    | `YZ -> [`Y; `Z]
    | `XZ -> [`X; `Z]
   and t, p, (sx, sy, sz), (rx, ry, rz) = !marcador
   in
   GlMat.push ();
   GlMat.translate3 p;
   if t = `Escalar then begin
      GlMat.rotate ~angle:rx ~x:1.0 ();
      GlMat.rotate ~angle:ry ~y:1.0 ();
      GlMat.rotate ~angle:rz ~z:1.0 ()
   end;
   (match t with
      `Mover   -> Func.dibujar_marcador_traslacion (listar !eje) (max sx (max sy sz))
    | `Rotar   -> Func.dibujar_marcador_rotacion   (listar !eje) (max sx (max sy sz))
    | `Escalar -> Func.dibujar_marcador_escalado   (listar !eje) (sx, sy, sz)
    | `Vacio   -> ());
   GlMat.pop ()

(*
 * tipo: camaras
 *)

(* Establece como cámaras las cámaras por defecto *)
let set_camaras_iniciales () : unit =
   (* 4 cámaras por defecto *)
   camaras :=
      [
         Camara.get_camara_superior    ();
         Camara.get_camara_anterior    ();
         Camara.get_camara_izquierda   ();
         Camara.get_camara_perspectiva ()
      ]

(* Primera llamada para la inicialización de las cámaras *)
;; set_camaras_iniciales ();;

(* Obtiene la cámara correspondiente al índice dado *)
let get_camara (indice : int) : camara =
   if 0 <= indice && indice < (List.length !camaras) then
      List.nth !camaras indice
   else
      raise (Invalid_argument "escena:get_camara: el índice no corresponde a ninguna cámara")

(* Carga la visualización desde una definición XML *)
let cargar_visualizacion (xml : Xml.xml) : unit =
   if Xml.tag xml = "visualizacion" then
      Xml.iter (fun hijo -> camaras := List.append !camaras [new camara hijo]) xml
   else
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <visualizacion>")

(* Guarda la visualización en formato XML *)
let guardar_visualizacion () : Xml.xml =
   Xml.Element ("visualizacion", [], (List.map (fun cam -> cam # to_xml) !camaras))

(*
 * tipo: vistas
 *)

(* Obtiene la vista correspondiente al índice dado *)
let get_vista (indice : int) : Togl.widget =
   if 0 <= indice && indice < (List.length !vistas) then
      List.nth !vistas indice
   else
      raise (Invalid_argument "escena:get_vista: el índice no corresponde a ninguna vista")

(* Obtiene la función de visualización de la escena bajo la vista de la
   cámara de índice [indice] (si existe). *)
let get_visualizacion (indice : int) : unit -> unit =
   let vista  = get_vista indice in
   try
      let camara = get_camara indice in
      let f () =
         (* establece la visualización *)
         camara # representar ~vista ~indice;

         (* habilita la iluminación *)
         List.iter (fun luz -> luz # encender) !iluminacion;

         (* dibuja los reflejos sobre el plano XZ *)
         if !reflejos then Reflejos.dibujar
            ~cam:camara
            ~objs:!geometria
            ~luces:!iluminacion
            ~sentido:camara # get_sentido
            ~vista:vista
            ~indice:indice;

         if !sombras then Sombras.dibujar
            ~cam:camara
            ~objs:!geometria
            ~luces:!iluminacion
            ~vista:vista
            ~indice:indice;

         (* obtiene dos particiones con los objetos opacos y los transparentes *)
         let obj_opacos, obj_transp = List.partition (fun obj -> obj # get_material # es_opaco) !geometria
         in
         (* dibuja los objetos opacos *)
         List.iter
            (fun (obj : Dibujable.dibujable) ->
               GlMat.push ();
               GlMat.translate3 obj # get_t;
               GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
               GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
               GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
               GlMat.scale3     obj # get_s;
               Togl.make_current vista;
               obj # call_display_list indice ~normales:camara # get_normales ~cara:`front ();
               GlMat.pop ())
            obj_opacos;
         (* dibuja los objetos transparentes *)
         GlFunc.depth_mask false; (* Z-buffer sólo lectura *)
         List.iter
            (fun (obj : Dibujable.dibujable) ->
               GlMat.push ();
               GlMat.translate3 obj # get_t;
               GlMat.rotate     ~angle:obj # get_rx ~x:1.0 ();
               GlMat.rotate     ~angle:obj # get_ry ~y:1.0 ();
               GlMat.rotate     ~angle:obj # get_rz ~z:1.0 ();
               GlMat.scale3     obj # get_s;
               Togl.make_current vista;
               obj # call_display_list indice ~normales:camara # get_normales ~cara:`front ();
               GlMat.pop ())
            obj_transp;
         GlFunc.depth_mask true; (* Z-buffer lectura/escritura *)

         (* deshabilita la iluminación *)
         List.iter (fun luz -> luz # apagar) !iluminacion;

         (* dibuja los marcadores de las luces *)
         List.iter
            (fun luz ->
               GlMat.push ();
               GlMat.translate3 luz # get_posicion;
               let rx, ry, rz = luz # get_orientacion in
               GlMat.rotate     ~angle:rx ~x:1.0 (); 
               GlMat.rotate     ~angle:ry ~y:1.0 ();
               GlMat.rotate     ~angle:rz ~z:1.0 ();
               luz # dibujar;
               GlMat.pop ())
            !iluminacion;
         (* dibuja el marcador de la transformación (si lo hubiera *)
         dibujar_marcador ();
         (* dibuja los controles de la cámara *)
         camara # dibujar_controles vista;
         Gl.flush ();
         Togl.swap_buffers vista;
      in
      f
   with
      Invalid_argument _ -> (fun () ->
         GlClear.color Defs.escena_fondo_color;
         GlClear.clear [`color];
         Gl.flush ();
         Togl.swap_buffers vista)

(* Actualiza las funciones de visualización y eventos para las vistas registradas *)
let actualizar_visualizaciones () : unit =
   for i = 0 to (List.length !vistas) - 1 do
      let vis = List.nth !vistas i in
      (* función de visualización *)
      Togl.display_func vis ~cb:(get_visualizacion i);
      if i < List.length !camaras then
         let cam = List.nth !camaras i in
         (* menú popup para los parámetros de visualización *)
         Tk.bind vis ~events:[`ButtonPressDetail 3]
            ~fields:[`MouseX; `MouseY]
            ~action:(fun ev ->
               let dx, dy = Winfo.rootx vis, Winfo.rooty vis in
               cam # mostrar_parametros vis
                  ~x:(dx + ev.Tk.ev_MouseX)
                  ~y:(dy + ev.Tk.ev_MouseY));
          let x0, y0 = ref 0, ref 0 in
          let seleccion = ref (`Obj, -1) in
          (* activación del modo transformación *)
          Tk.bind vis ~events:[`ButtonPressDetail 1]
             ~fields:[`MouseX; `MouseY]
             ~action:(fun ev -> x0 := ev.Tk.ev_MouseX; y0 := ev.Tk.ev_MouseY;
                seleccion := cam # seleccionar
                   ~x:ev.Tk.ev_MouseX
                   ~y:(Winfo.height vis - ev.Tk.ev_MouseY)
                   ~objs:(!geometria)
                   ~luces:(!iluminacion)
                   ~vista:vis;
                actualizar_marcador !seleccion cam ~info:true;
                List.iter Togl.render !vistas);
          (* activación del modo transformación + duplicación *)
          Tk.bind vis ~events:[`Modified([`Shift], `ButtonPressDetail 1)]
             ~fields:[`MouseX; `MouseY]
             ~action:(fun ev -> x0 := ev.Tk.ev_MouseX; y0 := ev.Tk.ev_MouseY;
                seleccion := cam # seleccionar
                   ~x:ev.Tk.ev_MouseX
                   ~y:(Winfo.height vis - ev.Tk.ev_MouseY)
                   ~objs:(!geometria)
                   ~luces:(!iluminacion)
                   ~vista:vis;
                (* duplica el objeto *)
                let t, i = !seleccion in
                (match t with
                   `Obj -> if (!transformacion = `Mover) or
                              (!transformacion = `Rotar) or
                              (!transformacion = `Escalar) then
                      begin
                         let obj = XmlMarshal.make_geo (List.nth !geometria i) # to_xml in
                         (** crea las display lists del objeto *)
                         for i = 0 to (List.length !vistas) - 1 do
                            let vista  = get_vista  i
                            and camara = get_camara i
                            in
                            Togl.make_current vista;
                            obj # set_display_list i
                         done;
                         geometria := List.append !geometria [obj];
                         seleccion := (t, List.length !geometria - 1)
                      end
                 | `Luz -> if (!transformacion = `Mover) or
                              (!transformacion = `Rotar) or
                              (!transformacion = `Escalar) then
                      try
                         iluminacion := List.append !iluminacion
                            [XmlMarshal.make_luz (List.nth !iluminacion i) # to_xml];
                         seleccion := (t, List.length !iluminacion - 1);
                      with
                         Failure _ -> ());
                actualizar_marcador !seleccion cam ~info:true;
                List.iter Togl.render !vistas);
          (* transformación en el plano YZ *)
          Tk.bind vis ~events:[`Modified([`Button1], `Motion)]
             ~fields:[`MouseX; `MouseY]
             ~action:(fun ev -> let x, y = (ev.Tk.ev_MouseX - !x0), (!y0 - ev.Tk.ev_MouseY) in
                 Estado.mostrar (try
                    let objs   = !geometria
                    and luces  = !iluminacion
                    and indice = snd !seleccion
                    in
                    (match (fst !seleccion), !transformacion, !eje with
                          _, `Situar,  `X  -> cam # situar ~x:y ()
                     |    _, `Situar,  `Y  -> cam # situar ~y   ()
                     |    _, `Situar,  `Z  -> cam # situar ~z:y ()
                     |    _, `Situar,  `XY -> cam # situar ~x ~y   ()
                     |    _, `Situar,  `YZ -> cam # situar ~y ~z:x ()
                     |    _, `Situar,  `XZ -> cam # situar ~x ~z:y ()
                     |    _, `Girar,   `X  -> cam # girar  ~x:y ()
                     |    _, `Girar,   `Y  -> cam # girar  ~y   ()
                     |    _, `Girar,   `Z  -> cam # girar  ~z:y ()
                     |    _, `Girar,   `XY -> cam # girar  ~x ~y   ()
                     |    _, `Girar,   `YZ -> cam # girar  ~y ~z:x ()
                     |    _, `Girar,   `XZ -> cam # girar  ~x ~z:y ()
                     |    _, `Zoom,     _  -> cam # zoom    y
                     | `Obj, `Mover,   `X  -> (List.nth objs  indice) # trasladar ~x:y ()
                     | `Obj, `Mover,   `Y  -> (List.nth objs  indice) # trasladar ~y:y ()
                     | `Obj, `Mover,   `Z  -> (List.nth objs  indice) # trasladar ~z:y ()
                     | `Obj, `Mover,   `XY -> (List.nth objs  indice) # trasladar ~x:x ~y:y ()
                     | `Obj, `Mover,   `YZ -> (List.nth objs  indice) # trasladar ~y:y ~z:x ()
                     | `Obj, `Mover,   `XZ -> (List.nth objs  indice) # trasladar ~x:x ~z:y ()
                     | `Obj, `Rotar,   `X  -> (List.nth objs  indice) # rotar     ~x:y  ()
                     | `Obj, `Rotar,   `Y  -> (List.nth objs  indice) # rotar     ~y:y  ()
                     | `Obj, `Rotar,   `Z  -> (List.nth objs  indice) # rotar     ~z:y  ()
                     | `Obj, `Rotar,   `XY -> (List.nth objs  indice) # rotar     ~x:x ~y:y ()
                     | `Obj, `Rotar,   `YZ -> (List.nth objs  indice) # rotar     ~y:y ~z:x ()
                     | `Obj, `Rotar,   `XZ -> (List.nth objs  indice) # rotar     ~x:x ~z:y ()
                     | `Obj, `Escalar, `X  -> (List.nth objs  indice) # escalar   ~x:y ()
                     | `Obj, `Escalar, `Y  -> (List.nth objs  indice) # escalar   ~y:y ()
                     | `Obj, `Escalar, `Z  -> (List.nth objs  indice) # escalar   ~z:y ()
                     | `Obj, `Escalar, `XY -> (List.nth objs  indice) # escalar   ~x:x ~y:y ()
                     | `Obj, `Escalar, `YZ -> (List.nth objs  indice) # escalar   ~y:y ~z:x ()
                     | `Obj, `Escalar, `XZ -> (List.nth objs  indice) # escalar   ~x:x ~z:y ()
                     | `Luz, `Mover,   `X  -> (List.nth luces indice) # posicionar ~x:y ()
                     | `Luz, `Mover,   `Y  -> (List.nth luces indice) # posicionar ~y:y ()
                     | `Luz, `Mover,   `Z  -> (List.nth luces indice) # posicionar ~z:y ()
                     | `Luz, `Mover,   `XY -> (List.nth luces indice) # posicionar ~x:x ~y:y ()
                     | `Luz, `Mover,   `YZ -> (List.nth luces indice) # posicionar ~y:y ~z:x ()
                     | `Luz, `Mover,   `XZ -> (List.nth luces indice) # posicionar ~x:x ~z:y ()
                     | `Luz, `Rotar,   `X  -> (List.nth luces indice) # orientar   ~x:y  ()
                     | `Luz, `Rotar,   `Y  -> (List.nth luces indice) # orientar   ~y:y  ()
                     | `Luz, `Rotar,   `Z  -> (List.nth luces indice) # orientar   ~z:y  ()
                     | `Luz, `Rotar,   `XY -> (List.nth luces indice) # orientar   ~x:x ~y:y ()
                     | `Luz, `Rotar,   `YZ -> (List.nth luces indice) # orientar   ~y:y ~z:x ()
                     | `Luz, `Rotar,   `XZ -> (List.nth luces indice) # orientar   ~x:x ~z:y ()
                     | `Luz, `Escalar,  _  -> "Las luces no se pueden escalar"
                     | _, _, _ -> "")
                 with
                    Invalid_argument _ -> ""
                  | Failure          _ -> "");
                if !transformacion <> `Borrar then actualizar_marcador !seleccion cam ~info:false;
                x0 := ev.Tk.ev_MouseX;
                y0 := ev.Tk.ev_MouseY;
                List.iter Togl.render !vistas);
      else begin
         (* desactiva el menú popup *)
         Tk.bind vis ~events:[`ButtonPressDetail 3] ~action:(fun ev -> ignore ev);
         (* desactiva las transformaciones *)
         Tk.bind vis ~events:[`ButtonPressDetail 1] ~action:(fun ev -> ignore ev);
         Tk.bind vis ~events:[`Modified([`Button1], `Motion)] ~action:(fun ev -> ignore ev);
     end
   done

(* Interfaz [escena.mli] *)
let registrar_vista (vista : Togl.widget) : unit =
   if not (List.mem vista !vistas) then begin
      Togl.make_current vista;
      Suelo.set_display_list (List.length !vistas);
      vistas := List.append !vistas [vista];
      actualizar_visualizaciones ()
   end

(* Actualiza las vistas que representan la escena *)
let actualizar_vistas () : unit =
   List.iter Togl.render !vistas

(*
 * tipo: geometria
 *)

(* Carga la geometría desde una definición XML *)
let cargar_geometria (xml : Xml.xml) : unit =
   if Xml.tag xml = "geometria" then
      let hijos      = Xml.children xml in
      for i = 0 to List.length hijos - 1 do
         let def_float = 1.0
         and def_int   = 1
         and hijo      = List.nth hijos i
         in
         try
            let obj = XmlMarshal.make_geo hijo in
            (** crea las display lists del objeto *)
            for i = 0 to (List.length !vistas) - 1 do
               let vista  = get_vista i
               and camara = get_camara i
               in
               Togl.make_current vista;
               obj # set_display_list i
            done;
            geometria := List.append !geometria [obj]
         with
            Invalid_argument msg -> Estado.mostrar msg
      done
   else
      (* por aquí nunca pasa (validación DTD) *)
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <geometria>")

(* Guarda la geometría en formato XML *)
let guardar_geometria () : Xml.xml =
   Xml.Element ("geometria", [], (List.map (fun obj -> obj # to_xml) !geometria))

(* Interfaz [escena.mli] *)
let add_obj (obj : Dibujable.dibujable) : unit =
   (** crea las display lists del objeto *)
   for i = 0 to (List.length !vistas) - 1 do
      let vista  = get_vista  i
      and camara = get_camara i
      in
      Togl.make_current vista;
      obj # set_display_list i
   done;

   geometria := List.append !geometria [obj];
   actualizar_vistas ();
   Estado.mostrar ("Objeto de tipo '" ^ obj # get_nombre ^ "' creado")

(*
 * tipo: iluminacion
 *)

 (* Carga la iluminación desde una definición XML *)
let cargar_iluminacion (xml : Xml.xml) : unit =
   if Xml.tag xml = "iluminacion" then
      let hijos = Xml.children xml in
      for i = 0 to (List.length hijos) - 1 do
         let hijo = List.nth hijos i in
         try
            iluminacion := List.append !iluminacion [XmlMarshal.make_luz hijo]
         with
            Invalid_argument msg -> Estado.mostrar msg
      done
   else
      (* por aquí nunca pasa (validación DTD) *)
      raise (Invalid_argument "La etiqueta XML no es correcta; se esperaba <iluminacion>")

(* Guarda la iluminación en formato XML *)
let guardar_iluminacion () : Xml.xml =
   Xml.Element ("iluminacion", [], (List.map (fun luz -> luz # to_xml) !iluminacion))

(* Interfaz [escena.mli] *)
let add_luz (luz : Luz.luz) : unit =
   iluminacion := List.append !iluminacion [luz];
   actualizar_vistas ();
   Estado.mostrar ("Luz de tipo '" ^ luz # get_nombre ^ "' creada")

(*
 * tipo: escena
 *)

(* Reinicia las estructuras de datos de la escena *)
let reiniciar () : unit =
    Defs.escena_archivo := "";
    camaras     := [];
    (* elimina las display lists *)
    List.iter (fun obj -> obj # clear_display_lists) !geometria;
    geometria   := [];
    (* crea la luz ambiental *)
    Luz.ct_luces := -1;
    iluminacion  := [];
    marcador := (`Vacio, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), (0.0, 0.0, 0.0))

(* Interfaz [escena.mli] *)
let nueva () : string =
    reiniciar ();
    iluminacion := [(new Luz.ambiental Luz.ambiental_defecto :> Luz.luz)];
    set_camaras_iniciales ();
    actualizar_visualizaciones ();
    actualizar_vistas ();
    "Nueva escena cargada"

(* Interfaz [escena.mli] *)
let cargar (archivo : string) : unit =
   reiniciar ();
   let msg = try
         let xml = Xml.parse_file archivo in
         if Xml.tag xml = "escena" then begin
            Defs.escena_archivo := archivo;
            Estado.mostrar ("Escena \"" ^ !Defs.escena_archivo ^ "\" cargada correctamente");
            let visualizacion = List.nth (Xml.children xml) 0 (* visualización *)
            and iluminacion   = List.nth (Xml.children xml) 1 (* iluminación   *)
            and geometria     = List.nth (Xml.children xml) 2 (* geometría     *)
            in
            cargar_visualizacion visualizacion;
            cargar_iluminacion   iluminacion;
            cargar_geometria     geometria
         end else
            Estado.mostrar "La etiqueta XML no es correcta; se esperaba <escena>"
      with
         | Xml.Error msg ->
            Estado.mostrar ("Error en el XML : " ^ (Xml.error msg))
         | Xml.File_not_found msg ->
            Estado.mostrar ("No se encuentra el archivo \"" ^  msg ^ "\"")
         | Dtd.Parse_error msg ->
            Estado.mostrar ("Error al parsear la DTD : " ^ (Dtd.parse_error msg))
         | Dtd.Check_error msg ->
            Estado.mostrar ("Error al chequear la DTD : " ^ (Dtd.check_error msg))
         | Dtd.Prove_error msg ->
            Estado.mostrar ("Error al validar la DTD : " ^ (Dtd.prove_error msg))
         | Failure "float_of_string" ->
            Estado.mostrar ("Revisar archivo \"" ^ !Defs.escena_archivo ^ "\" en busca de floats sin la pinta de número")
         | Failure "int_of_string" ->
            Estado.mostrar ("Revisar archivo \"" ^ !Defs.escena_archivo ^ "\" en busca de enteros sin la pinta de número")
         | Invalid_argument msg -> Estado.mostrar ("Se pasó un argumento no válido: " ^ msg)
         | Failure msg -> Estado.mostrar ("Se produjo el siguiente fallo: " ^ msg)
   in
   actualizar_visualizaciones ();
   actualizar_vistas ();
   msg

(* Interfaz [escena.mli] *)
let guardar (archivo : string) : string =
   try
      let xml = Xml.Element ("escena",
         [],
         [
            guardar_visualizacion ();
            guardar_iluminacion   ();
            guardar_geometria     ()
         ])
      in
      let str = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\n" ^
         "<!DOCTYPE escena SYSTEM \"../etc/escena.dtd\">\n\n" ^
         (Xml.to_string_fmt xml) ^ "\n"
      and out = open_out archivo
      in
      output_string out str;
      close_out out;
      "Escena guardada"
   with
      Sys_error msg -> msg

(*
 * tipo: reflejos
 *)

(* Interfaz [escena.mli] *)
let reflejos_activos () : bool = !reflejos

(* Interfaz [escena.mli] *)
let reflejos_activar () : unit =
   reflejos := true;
   actualizar_vistas ()

(* Interfaz [escena.mli] *)
let reflejos_desactivar () : unit =
   reflejos := false;
   actualizar_vistas ()

(*
 * tipo: sombras
 *)

(* Interfaz [escena.mli] *)
let sombras_activas () : bool = !sombras

(* Interfaz [escena.mli] *)
let sombras_activar () : unit =
   sombras := true;
   actualizar_vistas ()

(* Interfaz [escena.mli] *)
let sombras_desactivar () : unit =
   sombras := false;
   actualizar_vistas ()
