
.PHONY: all clean

OBJDIR_GUI=${OBJDIR}/gui

OBJS=$(patsubst %.ml, ${OBJDIR_GUI}/%.cmo, $(wildcard *.ml))
OBJDIRS=$(patsubst ${SRCDIR}%, ${OBJDIR}%, ${SRCDIRS})

all: ${APP_NAME}

clean:
	${RM} ${OBJDIR_GUI}/*.cmi ${OBJDIR_GUI}/*.cmo
	${RM} -r ${OBJDIR_GUI}
	${RM} ${APP_NAME}
	${RM} *~

${APP_NAME}: ${OBJDIR_GUI} ${OBJS}
	${OCAMLC} ${OBJDIRS} -o ${APP_NAME} 

${OBJDIR_GUI}:
	mkdir ${OBJDIR_GUI}

${OBJDIR_GUI}/app.cmo: app.ml
	${OCAMLC} ${INCDIRS} -c app.ml
	${MV} app.cmo app.cmi ${OBJDIR_GUI}

${OBJDIR_GUI}/herramientas.cmo: herramientas.ml
	${OCAMLC} ${INCDIRS} -c herramientas.ml
	${MV} herramientas.cmo herramientas.cmi ${OBJDIR_GUI}

${OBJDIR_GUI}/mensajes.cmo: mensajes.ml
	${OCAMLC} ${INCDIRS} -c mensajes.ml
	${MV} mensajes.cmo mensajes.cmi ${OBJDIR_GUI}

${OBJDIR_GUI}/menus.cmo: menus.ml
	${OCAMLC} ${INCDIRS} -c menus.ml
	${MV} menus.cmo menus.cmi ${OBJDIR_GUI}

${OBJDIR_GUI}/vistas.cmo: vistas.ml
	${OCAMLC} ${INCDIRS} -c vistas.ml
	${MV} vistas.cmo vistas.cmi ${OBJDIR_GUI}
