(** Constantes de la aplicaci�n *)

(** Color del texto que muestra el nombre de la c�mara en las vistas *)
let camara_nombre_color = (1.0, 1.0, 1.0)

(** Nombre del archivo de configuraci�n (sin la ruta) *)
let config_archivo = "gc.config.xml"

(** Extensi�n del archivo de configuraci�n *)
let config_archivo_ext = ".config.xml"

(** Nombre del archivo actual de la escena *)
let escena_archivo = ref ""

(** Color de fondo para dibujar las escenas *)
let escena_fondo_color = (0.75, 0.75, 0.75)

(** Color de la rejilla *)
let escena_grid_color = (0.25, 0.25, 0.25)

(** Color de los ejes de la rejilla *)
let escena_grid_ejes_color = (0.0, 0.0, 0.5)

(** Color de los l�mites de la rejilla *)
let escena_grid_limites_color = (1.0, 1.0, 0.0)

(** Subdivisiones de la rejilla *)
let escena_grid_divs = 20

(** Tama�o de la rejilla en unidades de la escena *)
let escena_grid_tam = 5.0

(** Opacidad del suelo utilizado para proyectar las reflexiones y las sombras *)
let escena_suelo_alpha = 0.25

(** Color 1 del suelo utilizado para proyectar las reflexiones y las sombras *)
let escena_suelo_color1 = [|0; 0; 128|]

(** Color 2 del suelo utilizado para proyectar las reflexiones y las sombras *)
let escena_suelo_color2 = [|50; 50; 200|]

(** Funci�n de conversi�n de floats a cadenas para insertar en XML *)
let float2xml f = Printf.sprintf "%0.2f" f

(** N�mero de filas que tienen las listas del panel de herramientas como base *)
let herramientas_listas_filas = 7

(** N�mero de columnas que tienen las listas del panel de herramientas *)
let herramientas_listas_columnas = 15

(** Tama�o en unidades de la escena de los marcadores de las luces *)
let luz_marcador_tamanyo = 0.075

(** N�mero PI (probablemente no cambie) *)
let pi = 3.14159265

(** Cantidad de unidades de la escena a las que equivale un pixel
    en las traslaciones de los objetos *)
let pixel_factor_obj_t = 0.005

(** Grados a los que equivale un pixel en las rotaciones de los objetos *)
let pixel_factor_obj_r = 0.4

(** Cantidad de unidades de la escena a las que equivale un pixel
    en los escalados de los objetos *)
let pixel_factor_obj_s = 0.01

(** Cantidad de unidades de la escena a las que equivale un pixel
    en las traslaciones de las c�maras *)
let pixel_factor_cam_t = 0.01

(** Grados a los que equivale un pixel en las rotaciones de las c�maras *)
let pixel_factor_cam_r = 0.1

(** Cantidad de unidades de la escena a las que equivale un pixel
    en los zooms de las c�maras cuando la proyecci�n es ortogonal o
    factor de escala de un FOV de 45 grados al que equivale un pixel
    cuando la proyecci�n es en perspectiva *)
let pixel_factor_cam_s = 0.005

(** Color de las polil�neas del mini-editor de polil�neas *)
let polilineas_color = (1.0, 1.0, 0.0)

(** Desplazamiento sobre el plano XZ para corregir fallos en el
    Z-Buffer con las superficies coplanares *)
let sombra_compensacion = -0.01

(** Variable de entorno de la aplicaci�n (donde reside) *)
let variable_entorno = "GC_HOME"

(** Color del borde de las vista cuando el cursor est� fuera de ellas *)
let vista_borde_color_off = `Black

(** Color del borde de las vistas cuando el cursor est� encima de ellas *)
let vista_borde_color_on = `White

(** Ancho x Largo en pixels que ocupa inicialmente en pantalla una vista *)
let vista_dims = 250, 250
