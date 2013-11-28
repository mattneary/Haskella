TARGET=src/haskella

all: build

build:
	ocamlbuild $(TARGET).native

clean:
	ocamlbuild -clean

