#
# Makefile, directorio 'lib'
#

.PHONY: all byte clean xmlLight.byte xmlLight.opt lib opt

#
# Objetivos
#
all: lib

byte: xmlLight.byte

clean:
	${ECHO} "Eliminando las bibliotecas construidas ... "
	${MAKE} -C xmlLight clean ${NULL_REDIRECT}
	${ECHON} ok

xmlLight.byte:
	${ECHO} "Construyendo biblioteca Xml Light ... "
	${MAKE} -C xmlLight ${NULL_REDIRECT}
	${ECHON} ok

xmlLight.opt:
	${ECHO} "Construyendo biblioteca nativa Xml Light ... "
	${MAKE} -C xmlLight opt ${NULL_REDIRECT}
	${ECHON} ok

lib: byte opt

opt: xmlLight.opt
