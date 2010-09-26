type w = Widget.any
type widget = w Widget.widget

type vista = ArrIzq | ArrDer | AbaIzq | AbaDer

let paneles_vistas = ref []

let ampliar (panel : Widget.frame Widget.widget) =
   List.iter Place.forget (List.filter (fun v -> v <> panel) !paneles_vistas);
   Tk.place ~relx:0.0 ~rely:0.0 ~relwidth:1.0 ~relheight:1.0 panel

let reducir () =
   let pv0 = List.nth !paneles_vistas 0
   and pv1 = List.nth !paneles_vistas 1
   and pv2 = List.nth !paneles_vistas 2
   and pv3 = List.nth !paneles_vistas 3
   in
   Tk.place ~relx:0.0 ~rely:0.0 ~relwidth:0.5 ~relheight:0.5 pv0;
   Tk.place ~relx:0.5 ~rely:0.0 ~relwidth:0.5 ~relheight:0.5 pv1;
   Tk.place ~relx:0.0 ~rely:0.5 ~relwidth:0.5 ~relheight:0.5 pv2;
   Tk.place ~relx:0.5 ~rely:0.5 ~relwidth:0.5 ~relheight:0.5 pv3

let crear_vista (padre : 'a Widget.widget) : Togl.widget =
   let vista =
      Togl.create padre
         ~width: (fst Defs.vista_dims)
         ~height:(snd Defs.vista_dims)
         ~rgba:true
         ~depth:true
         ~stencil:true
         ~double:true
   in
   Escena.registrar_vista vista;
   Tk.bind vista ~events:[`Enter]
      ~action:(fun _ -> Frame.configure ~background:Defs.vista_borde_color_on padre);
   Tk.bind vista ~events:[`Leave]
      ~action:(fun _ -> Frame.configure ~background:Defs.vista_borde_color_off padre);
   let ampliado = ref false in
   Tk.bind vista ~events:[`ButtonPressDetail 2]
      ~action:(fun _ -> if !ampliado then reducir () else ampliar padre; ampliado := not !ampliado);
   vista

let crear (padre : 'a Widget.widget) : widget =
   (* vistas para OpenGL *)
   let vistas = Frame.create
      ~relief:`Groove
      ~borderwidth:5
      ~width: (2 * (fst Defs.vista_dims))
      ~height:(2 * (snd Defs.vista_dims))
      padre
   in
   (* Código para el Controlador geométrico PACK
   let sup = Frame.create
      ~background:`White
      ~relief:`Flat
      vistas
   and inf = Frame.create
      ~background:`White
      ~relief:`Flat
      vistas
   in 
   Tk.pack ~expand:true ~fill:`Both [sup; inf];
   (* crea cada una de las vistas y las posiciona *)
   let v0 = crear_vista sup
   and v1 = crear_vista sup
   and v2 = crear_vista inf
   and v3 = crear_vista inf
   in
   Tk.pack ~expand:true ~fill:`Both ~side:`Left ~padx:1 ~pady:1 [v0; v1];
   Tk.pack ~expand:true ~fill:`Both ~side:`Left ~padx:1 ~pady:1 [v2; v3];
   *)

   (* crea los paneles para las vistas *)
   let pv0 = Frame.create ~background:Defs.vista_borde_color_off ~borderwidth:1 vistas
   and pv1 = Frame.create ~background:Defs.vista_borde_color_off ~borderwidth:1 vistas
   and pv2 = Frame.create ~background:Defs.vista_borde_color_off ~borderwidth:1 vistas
   and pv3 = Frame.create ~background:Defs.vista_borde_color_off ~borderwidth:1 vistas
   in

   (* crea cada una de las vistas y las posiciona *)
   let v0 = crear_vista pv0
   and v1 = crear_vista pv1
   and v2 = crear_vista pv2
   and v3 = crear_vista pv3
   in

   (* almacena los paneles de las vistas *)
   paneles_vistas := [pv0; pv1; pv2; pv3];

   (* posiciona las vistas *)
   Tk.place ~relx:0.0 ~rely:0.0 ~relwidth:0.5 ~relheight:0.5 pv0;
   Tk.place ~relx:0.5 ~rely:0.0 ~relwidth:0.5 ~relheight:0.5 pv1;
   Tk.place ~relx:0.0 ~rely:0.5 ~relwidth:0.5 ~relheight:0.5 pv2;
   Tk.place ~relx:0.5 ~rely:0.5 ~relwidth:0.5 ~relheight:0.5 pv3;
   Tk.pack ~expand:true ~fill:`Both [v0; v1; v2; v3];

   Widget.coe vistas
