#
# Makefile, práctica de Gráficos por Computador
#

.PHONY: all byte clean cleanup dep dist doc doc.clean lib lib.byte lib.clean lib.opt opt top

# Directorios del proyecto
export BINDIR=${PWD}/bin
export DOCDIR=${PWD}/doc
export LIBDIR=${PWD}/lib
export SRCDIR=${PWD}/src

# Nombre de la aplicación
export APP_NAME=${BINDIR}/gc-pract
export APP_NAME_BC=${APP_NAME}.byte
export APP_NAME_NC=${APP_NAME}.opt
export APP_NAME_TL=${APP_NAME}.top

# Bibliotecas externas
export LIBS=-I ${LIBDIR}/xmlLight

# Variables de compilación
export OCAML=ocaml
export OCAMLC=@ocamlc
export OCAMLOPT=@ocamlopt
export OCAMLDEP=@ocamldep
export OCAMLDOC=@ocamldoc
export INCLUDES=-I +labltk -I +lablGL ${LIBS} # opciones -I ajenas al proyecto
export OCAMLFLAGS=-custom -linkall -w A -warn-error A ${INCLUDES} lablgl.cma labltk.cma togl.cma xml-light.cma
export OCAMLOPTFLAGS=-linkall -w A -warn-error A ${INCLUDES} lablgl.cmxa labltk.cmxa togl.cmxa xml-light.cmxa
export OCAMLDOCFLAGS=-warn-error -html -colorize-code -d ${DOCDIR} ${INCLUDES}

# Comandos
export CHMOD=@chmod
export ECHO=@echo -n
export ECHON=@echo
export MAKE=@make -s
export MV=@mv
export NULL_REDIRECT=&> /dev/null
export RM=@rm -f
export TAR=@tar

#
# Objetivos
#
all:
	${MAKE} -C ${LIBDIR} $@
	${MAKE} -C ${SRCDIR} $@

byte:
	${MAKE} -C ${SRCDIR} $@

clean:
	${MAKE} -C ${SRCDIR} $@
	${RM} *~

cleanup:
	${MAKE} -C ${SRCDIR} $@
	${RM} *~

dep:
	${MAKE} -C ${SRCDIR} $@

dist: clean lib.clean
	${ECHO} "Generando copia de distribución ... "
	${TAR} czf ../`basename ${PWD}`.tgz -C ../ `basename ${PWD}`
	${ECHON} ok

doc:
	${MAKE} -C ${SRCDIR} $@

doc.clean:
	${MAKE} -C ${SRCDIR} $@

lib:
	${MAKE} -C ${LIBDIR} $@

lib.byte:
	${MAKE} -C ${LIBDIR} byte

lib.clean:
	${MAKE} -C ${LIBDIR} clean

lib.opt:
	${MAKE} -C ${LIBDIR} opt

opt:
	${MAKE} -C ${SRCDIR} $@

top:
	${MAKE} -C ${SRCDIR} $@
