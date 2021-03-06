#
# Makefile, directorio 'src'
#

include MODULES

.PHONY: all byte clean cleanup dep doc doc.clean opt top
.SUFFIXES: .mli .ml .cmi .cmo .cmx

# Dependencias
SRCINCS=$(addprefix -I , ${SRCDIRS})
MLISRCS=$(wildcard $(addsuffix /*.mli, ${SRCDIRS}))
MLSRCS =$(wildcard $(addsuffix /*.ml, ${SRCDIRS}))
MLIS=$(foreach DIR, ${SRCDIRS}, $(wildcard ${DIR}/*.mli))
MLS=$(foreach DIR, ${SRCDIRS}, $(wildcard ${DIR}/*.ml))

#
# Objetivos
#
all: byte opt top

byte: ${APP_NAME_BC}

clean: cleanup
	${ECHO} "Eliminando los objetivos construidos ... "
	${RM} ${APP_NAME_BC} ${APP_NAME_NC} ${APP_NAME_TL}
	${ECHON} ok

cleanup:
	${ECHO} "Eliminando los archivos temporales ... "
	$(foreach DIR, ${SRCDIRS}, ${RM} ${DIR}/*.cm[iox] ${DIR}/*.o ${DIR}/*~)
	${RM} .depend
	${ECHON} ok

dep: .depend

doc:
	${ECHO} "Generando la documentaci�n ... "
	${OCAMLDOC} ${OCAMLDOCFLAGS} ${SRCINCS} ${MLIS} ${MLS}
	${ECHON} ok

doc.clean:
	${ECHO} "Eliminando la documentaci�n ... "
	${RM} ${DOCDIR}/*
	${ECHON} ok

opt: ${APP_NAME_NC}

top: ${APP_NAME_TL}

# Aplicaci�n en c�digo byte
${APP_NAME_BC}: $(subst mli,cmi,${MLIS}) $(addsuffix .cmo, ${MODULES})
	${ECHO} "Enlazando el c�digo byte de la aplicaci�n ... "
	${OCAMLC} ${OCAMLFLAGS} $(filter %.cmo, $+) -o $@
	${ECHON} ok

# Aplicaci�n en c�digo nativo
${APP_NAME_NC}: $(subst mli,cmi,${MLIS}) $(addsuffix .cmx, ${MODULES})
	${ECHO} "Enlazando el c�digo nativo de la aplicaci�n ... "
	${OCAMLOPT} ${OCAMLOPTFLAGS} $(filter %.cmx, $+) -o $@
	${ECHON} ok

# Aplicaci�n en top level
${APP_NAME_TL}: ${APP_NAME_BC}
	${ECHO} "Generando el gui�n del top level ... "
	${ECHON} "${OCAML} ${OCAMLFLAGS} $(addprefix ${SRCDIR}/, $(addsuffix .cmo, ${MODULES}))" > $@
	${CHMOD} +x $@
	${ECHON} ok

# Dependencias
.depend: ${MLIS} ${MLS}
	${ECHO} "Generando dependencias ... "
	${OCAMLDEP} ${INCLUDES} ${SRCINCS} ${MLISRCS} ${MLSRCS} > $@
	${ECHON} ok

#
# Reglas por sufijos
#
.mli.cmi:
	${ECHO} "Construyendo $@ ... "
	${OCAMLC} ${OCAMLFLAGS} ${SRCINCS} -c $<
	${ECHON} ok

.ml.cmo:
	${ECHO} "Construyendo $@ ... "
	${OCAMLC} ${OCAMLFLAGS} ${SRCINCS} -c $<
	${ECHON} ok

.ml.cmx:
	${ECHO} "Construyendo $@ ... "
	${OCAMLOPT} ${OCAMLOPTFLAGS} ${SRCINCS} -c $<
	${ECHON} ok

# Dependencias
-include .depend
