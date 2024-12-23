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

vim: ## vim install
	mkdir -p ~/.vim
	if [[ ! -d ~/.vim/bundle ]]; \
	then git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim; \
	fi
	vim +'PluginInstall --sync' +qa

sh: ## run a shell
	bash --init-file  ../etc/dotshellrc -i

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status

word: ## commit to main
	- echo -en "$(LOUD)phrase? $(SOFT)" ;  read x ; figlet -W -f mini $$x | gawk '{print "-- " $$0}' |pbcopy

../docs/%.html : ../src/%.md    
	1>&2 echo "... $@"
	sed -e '1d' -e '2d' $< | pandoc  -o $@ $(Pandoc)

~/tmp/%.html : %.lua
	pycco -d ~/tmp $^
	echo "p { text-align: right; }" >> ~/tmp/pycco.css
	open $@

../docs/%.html : %.lua
	pycco -d ../docs  $^
	echo "p { text-align: right;  }  " >> ../docs/pycco.css
	gawk '/<h1>/ {print "<div class=docs>";                       \
                while(getline x < "../etc/head.html") {print x}; \
                print "<h1>'$^'</h1></div>";                  \
                next} 1' $@ > tmp.tmp
	mv tmp.tmp $@

~/tmp/%.pdf : %.lua  Makefile
	@mkdir -p docs
	@echo "pdfing : $@ ... "
	@a2ps -Bj --landscape                           \
		--chars-per-line=90 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--columns 3                 \
		--borders=no --pro=color \
		--right-footer="" --left-footer=""    \
		--pretty-print=../etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps

heading:
	- echo -en "$(LOUD)Heading? $(SOFT)" ;  read x ; figlet -W -f mini $$x | gawk '{print "#  " $$0}'

compare: ../../moot/optimize/[chmp]*/*.csv
	$(foreach f, $^, (lua kseed.lua -compare $f &); )


