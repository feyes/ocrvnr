# Makefile ocrVnr

OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
SRC= detection.ml rotation.ml main.ml
ocrVnr: ${SRC}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocrVnr ${SRC}

clean::
	rm -f *~ *.o *.cm? ocrVnr

# FIN
