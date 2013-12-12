.PHONY : compile console typecheck typer clean tutorial

all: compile

clean:
	@./rebar clean
	@rm -f doc/skel.aux doc/skel.bbl doc/skel.blg doc/skel.fdb_latexmk doc/skel.fls
	@rm -f doc/skel.out doc/skel.synctex.gz doc/skel.pdf doc/skel.log
	@rm -f ./.skel.plt ./.otp.plt skel.tar.gz
	@rm -f ifl.data.10.log ifl.verbose.10.log
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/erlang.png
	@rm -f tutorial/bin/*.html tutorial/bin/*.png

compile: src/*.erl
	@./rebar compile

console: compile
	@exec erl -args_file ./priv/default.args

examples: compile
	@echo "==> skel (examples)"
	@erlc +debug_info -I include -o ebin examples/*.erl

typecheck: compile .skel.plt
	@echo "==> skel (typecheck)"
	@dialyzer --no_check_plt --plt ./.skel.plt -c ebin -Wrace_conditions

typer: compile .skel.plt
	@echo "==> skel (typer)"
	@typer --plt ./.skel.plt --show -I include -pa ebin -r src

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

.skel.plt: .otp.plt compile
	@echo "==> skel (plt)"
	@dialyzer --add_to_plt --plt ./.otp.plt --output_plt ./.skel.plt -c ebin

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

# Linux variant? (xdg-open)
tutorial: md
	@open -a /Applications/Safari.app tutorial/bin/index.html