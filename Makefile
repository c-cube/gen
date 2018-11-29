
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

VERSION=$(shell awk '/^version:/ {print $$2}' gen.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	find -name '*.ml' -or -name '*.mli' | xargs sed -i "s/NEXT_VERSION/$(VERSION)/g"
	find -name '*.ml' -or -name '*.mli' | xargs sed -i "s/NEXT_RELEASE/$(VERSION)/g"

watch:
	while find src/ bench/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make ; \
	done

.PHONY: update_next_tag release
