.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

OPAM ?= opam
OPAM_EXEC ?= $(OPAM) exec --
DUNE ?= dune

default: build

fmt:
	$(OPAM_EXEC) $(DUNE) build @fmt
	$(OPAM_EXEC) $(DUNE) promote

build: fmt
	$(OPAM_EXEC) $(DUNE) build
	$(OPAM_EXEC) $(DUNE) format-dune-file dune-project > .dune-project-formatted
	mv .dune-project-formatted dune-project

install:
	$(OPAM_EXEC) $(DUNE) install

uninstall:
	$(OPAM_EXEC) $(DUNE) uninstall

clean:
	$(OPAM_EXEC) $(DUNE) clean
	git clean -dfXq

test: fmt
	$(OPAM_EXEC) $(DUNE) runtest

testf: fmt
	$(OPAM_EXEC) $(DUNE) runtest -f

run: test
	true

DOCS_PATH=docs/
DOCS_NAME=oasis
DOCS_DESCR=Another standard library for ocaml, just for fun
DOCS_INDEX_TITLE=$(DOCS_NAME) - $(DOCS_DESCR)
define DOCS_EMBED
<meta content="$(DOCS_NAME)" property="og:title" />\
<meta content="$(DOCS_DESCR)" property="og:description" />\
<meta content="https://github.com/CharlesAverill/oasis" property="og:url" />
endef

cleandocs:
	if [ ! -d $(DOCS_PATH) ]; then \
		mkdir $(DOCS_PATH); \
	fi
	rm -rf $(DOCS_PATH)module $(DOCS_PATH)docs $(DOCS_PATH)odoc.support $(DOCS_PATH)index.html $(DOCS_PATH)sherlodoc.js $(DOCS_PATH)db.js

docs: cleandocs build
	$(OPAM_EXEC) $(DUNE) build @doc
	mv -f _build/default/_doc/_html/* $(DOCS_PATH)
	rm -f $(DOCS_PATH)index.html
	mv $(DOCS_PATH)oasis/oasis.html $(DOCS_PATH)index.html
	mv $(DOCS_PATH)oasis $(DOCS_PATH)module
	cp -f $(DOCS_PATH)theme.css $(DOCS_PATH)odoc.support/odoc.css
	cp $(DOCS_PATH)module/db.js $(DOCS_PATH)db.js
	
	@echo "Preparing Index\n--------------"
	# Header
	sed -i 's/<title>.*<\/title>/<title>$(DOCS_INDEX_TITLE)<\/title>/g' $(DOCS_PATH)index.html
	sed -i 's@</head>@$(DOCS_EMBED)\n</head>@g' $(DOCS_PATH)index.html
	sed -i 's/..\/odoc.support/odoc.support/g' $(DOCS_PATH)index.html
	# Body
	sed -i "s@<nav class="odoc-nav">.*gbcamel</nav>@@g" $(DOCS_PATH)index.html
	grep -rlZ "details open=\"open\"" . | xargs -0 sed -i "s/details open=\"open\"/details/g"

	@echo "Fixing search\n-----------------"
	sed -i 's/search_result.href = base_url + entry.url/search_result.href = \".\/\" + entry.url.substring(\"oasis\/\".length)/g' \
		$(DOCS_PATH)odoc.support/odoc_search.js

push: cleandocs build
	@read -p "Commit message: " input; \
	if [ -z "$input" ]; then \
		echo "Error: Please provide a valid commit message."; \
		exit 1; \
	fi; \
	git add . && git commit -m "$$input" && git push origin main
