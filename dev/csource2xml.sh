#!/bin/bash
#
# Conversor de formato C-Source de El GIMP al XML de textura definido en textura.dtd
#

#######################################
## Constantes
#######################################

APPNAME=`basename "$0"`
CAT=cat
CC=cc
UUIDGEN=uuidgen
RM=rm

#######################################
## Precondiciones
#######################################

if [ $# != 5 ]; then
	echo "Uso: $0 <imagen> <nombre> <entorno> <filtrado> <repeticion>"
	exit 1
fi

OUTPUT="$1"
NOMBRE="$2"
ENTORNO="$3"
FILTRADO="$4"
REPETICION="$5"

#######################################
## Opening
#######################################

# genera los archivos temporales
TEMP=`$UUIDGEN -r`
while [ -a "$TEMP.c" -o -a "$TEMP.o" ]; do
	TEMP=`$UUIDGEN -r`
done

#######################################
## Main task
#######################################

# imprime el archivo .c con la cabecera correspondiente
"$CAT" > "$TEMP.c" << END
/* estructura de imagen generada con El GIMP (formato C-Source) */
#include "$OUTPUT.c"

#include <stdlib.h>
#include <stdio.h>

/* conversión del rango [0,255] a [0.0f,1.0f] */
#define NORMALIZAR(c) ((float) (c) / 255.0)

/*
 * Convierte el formato de imagen C-Source de El GIMP al
 * formato XML de textura definido en textura.ml
 */
int main(int argc, char** argv)
{
	int ct_pixels    = 0;
	int total_pixels =
		gimp_image.width *
		gimp_image.height;

	if (gimp_image.bytes_per_pixel == 4)
	{
		/* imprime la cabecera */
		printf("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\n");
		printf("<!DOCTYPE textura SYSTEM \"../etc/textura.dtd\">\n\n");
		printf("<textura nombre=\"$NOMBRE\">\n");
		printf("\t<mapeado entorno=\"$ENTORNO\" filtrado=\"$FILTRADO\" repeticion=\"$REPETICION\"/>\n");
		printf("\t<raster ancho=\"%d\" alto=\"%d\">", gimp_image.width, gimp_image.height);

		/* imprime los pixels del raster */
		for (ct_pixels = 0; ct_pixels < total_pixels; ct_pixels++)
		{
			int r = gimp_image.pixel_data[4 * ct_pixels];
			int g = gimp_image.pixel_data[4 * ct_pixels + 1];
			int b = gimp_image.pixel_data[4 * ct_pixels + 2];
			int a = gimp_image.pixel_data[4 * ct_pixels + 3];

			/*
			printf("\t\t<pixel r=\"%.2f\" g=\"%.2f\" b=\"%.2f\" a=\"%.2f\"/>\n",
				NORMALIZAR(r), NORMALIZAR(g), NORMALIZAR(b), NORMALIZAR(a));
			*/

			printf("%03d%03d%03d%03d", r, g, b, a);
		}

		/* imprime el cierre */
		printf("\t</raster>\n");
		printf("</textura>\n");
	}
	else
	{
		printf("%s: sólo se manejan imágenes RGBA\n", argv[0]);
		return 1;
	}

	return 0;
}
END

# compila el .c
"$CC" "$TEMP.c" -o "$TEMP.o"

# ejecuta y genera el xml
"./$TEMP.o" > "$OUTPUT.xml"

#######################################
## Ending
#######################################

# borra los archivos temporales
"$RM" -f "$TEMP.c" "$TEMP.o"
