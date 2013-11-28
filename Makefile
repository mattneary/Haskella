TARGET=src/haskella

all: build clean

build:
	ocamlbuild $(TARGET).native

clean:
	ocamlbuild -clean

