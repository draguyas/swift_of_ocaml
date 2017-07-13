TARGET=swift_of_ocaml
TEST_FILE=let_binding.cmt


all:
	ocamlbuild -use-ocamlfind $(TARGET).native
	ocamlbuild -use-ocamlfind $(TARGET).byte

clean:
	ocamlbuild -clean
	rm -rf tests/*.swift

test: all
	cd tests && ../$(TARGET).native $(TEST_FILE)

.PHONY: all clean test
