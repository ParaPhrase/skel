
all: compile

.PHONY: clean
clean:
	@./rebar clean
	@rm -f doc/skel.aux doc/skel.bbl doc/skel.blg doc/skel.fdb_latexmk doc/skel.fls
	@rm -f doc/skel.out doc/skel.synctex.gz doc/skel.pdf doc/skel.log
	@rm -f ./.skel.plt ./.otp.plt skel.tar.gz
	@rm -f ifl.data.10.log ifl.verbose.10.log
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/erlang.png
	@rm -f tutorial/bin/*.html tutorial/bin/*.png

.PHONY: compile
compile:
	@./rebar compile


.PHONY: console
console: compile
	@exec erl -args_file ./priv/default.args


examples: compile
	@echo "==> skel (examples)"
	@erlc +debug_info -I include -o ebin examples/*.erl


.PHONY: typecheck
typecheck: compile .otp.plt
	@echo "==> skel (typecheck)"
	@dialyzer --no_check_plt --no_native --plt ./.otp.plt -c ebin -Wrace_conditions


.PHONY: typer
typer: compile .otp.plt
	@echo "==> skel (typer)"
	@typer --plt ./.otp.plt --show -I include -pa ebin -r src


.PHONY: test
test: compile
	@./rebar skip_deps=true eunit


pdf: doc/skel.pdf
	@echo "==> skel (pdf)"

package: skel.tar.gz
	@echo "==> skel (package)"

todo:
	@echo "==> skel (todo)"
	@grep -ir "TODO" --exclude Makefile --exclude README.md \
		--exclude-dir .git -- .

skel.tar.gz: compile
	@tar -czf skel.tar.gz -C .. --exclude "skel.tar.gz" skel

.otp.plt:
	@echo "==> otp (plt) # This takes a while, go grab a coffee"
	@dialyzer --build_plt --output_plt ./.otp.plt --apps erts kernel stdlib debugger et tools

docs: compile
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/erlang.png
	@./rebar doc

doc/skel.pdf: doc/skel.tex
	@pdflatex -interaction=batchmode -output-directory=./doc skel.tex
	@pdflatex -interaction=batchmode -output-directory=./doc skel.tex

md: docs
	@echo "==> Compiling Tutorial"
	@multimarkdown -b -f ./tutorial/src/*.md
	@mv ./tutorial/src/*.html ./tutorial/bin
	@cp ./tutorial/src/*.png ./tutorial/bin
	@echo "Documentation may be found in tutorial/bin"

cleanmd:
	@rm -f tutorial/bin/*.html

.PHONY: tutorial
# Linux variant? (xdg-open)
tutorial: md
	@open -a /Applications/Safari.app tutorial/bin/index.html
