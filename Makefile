TARGET=swift_of_ocaml
TEST_FILES=$(subst .ml,,$(subst tests/,,$(wildcard tests/*.ml)))

all:
	ocamlbuild -use-ocamlfind $(TARGET).native
	ocamlbuild -use-ocamlfind $(TARGET).byte

clean:
	ocamlbuild -clean
	rm -rf tests/*.swift

test: all
	cd tests && \
	for i in $(TEST_FILES); do \
		echo "Processing file : [$$i]"; \
		../scripts/extract_cmt.sh $$i.ml; \
		../$(TARGET).native $$i.cmt -o $$i.swift; \
	done

.PHONY: all clean test
