SHELL:= /bin/bash#
.SILENT: # no output

LOUD = \033[1;34m#
HIGH = \033[1;33m#
SOFT = \033[0m#

help: ## show help
	grep '^[a-z].*:.*##' $(MAKEFILE_LIST) \
	| sort \
	| gawk 'BEGIN {FS="##"; print "\n$(LOUD)make$(SOFT) [OPTIONS]\n"} \
	              {sub(/:.*/,"",$$1); \
                 printf("$(LOUD)%10s$(SOFT) %s\n",$$1,$$2)}'
	echo -e "$(HIGH)"; cat ../etc/frog.txt; echo -e "$(SOFT)"

sh: ## run a shell
	bash --init-file  ../etc/dotbashrc -i

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status

lines:
	sed '/^-- /d' kah.lua > lines.lua 
	$(MAKE) ~/tmp/lines.pdf
	rm lines.lua

~/tmp/kah.pdf: kah.lua
	mkdir -p $$HOME/tmp
	(cd ../docs; pdflatex -shell-escape -output-directory=$$HOME/tmp kah.tex)
	open $@

luas: kah.lua
	mkdir -p ../docs/luas
	gawk '/ ## / {F="../docs/luas/"$$3".lua"; N=0; print "">F; next; next } \
				/^--/  {next} \
				/^$$/ {next } \
				// {next } \
				/^(function|local)/ { if (N>2) {print "">>F}; N++} \
				F      {N++; print >>F }' kah.lua

../docs/%.html : %.lua
	pycco -d ../docs  $^
	echo "p { text-align: right;  }  " >> ../docs/pycco.css
	gawk '/<h1>/ {print "<div class=docs>"; \
                while(getline x < "../etc/head.html") {print x}; \
                print "<h1>'$^'</h1><hr></div>"; \
                next} \
          1' $@ > tmp.tmp
	mv tmp.tmp $@

../docs/index.html : ../docs/kah.html
	echo "pycco: $^ -> $@"
	cp $^ $@

~/tmp/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q                        \
		--chars-per-line=100 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--columns 3                 \
		--borders=no --pro=color  \
		--right-footer="" --left-footer=""    \
		--pretty-print=../etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps
	open $@
