SHELL:= /bin/bash#
.SILENT: # no output

LOUD = \033[1;34m#
HIGH = \033[1;33m#
SOFT = \033[0m#

help: ## show help
	grep '^[a-z].*:.*##' $(MAKEFILE_LIST)                           \
	| sort                                                           \
	| gawk 'BEGIN {FS="##"; print "\n$(LOUD)make$(SOFT) [OPTIONS]\n"} \
	              {sub(/:.*/,"",$$1);                                  \
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

../docs/%.html : %.lua
	pycco -d ../docs  $^
	echo "p { text-align: right;  }  " >> ../docs/pycco.css
	gawk '/<h1>/ {print "<div class=docs>";                       \
                while(getline x < "../etc/head.html") {print x}; \
                print "<h1>'$^'</h1><hr></div>";                  \
                next} 1' $@ > tmp.tmp
	mv tmp.tmp $@

../docs/index.html : ../docs/kah.html
	echo "pycco: $^ -> $@"
	cp $^ $@

~/tmp/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q               \
		--chars-per-line=100          \
		--line-numbers=1               \
		--highlight-level=normal        \
		--columns 3                      \
		--borders=no --pro=color          \
		--right-footer="" --left-footer="" \
		--pretty-print=../etc/lua.ssh       \
		--footer="page %p."                  \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps
	open $@

go=guess
acq=explore
Stop=30

eg0: ../../moot/optimize/*/auto93.csv
	$(foreach f, $^, (lua kah2.lua --acquire $(acq) --$(go) $f );)

eg1: ../../moot/optimize/[hcpm]*/*.csv
	$(foreach f, $^, (lua kah2.lua --Stop $(Stop) --acquire $(acq) --$(go) $f &);) 

report1=gawk 'BEGIN {                                                                             \
   OFS=",| ";                                                                 \
   print("\n\n\#delta,| b4,| toBe,| close,| exploit,| explore,| adapt,| rand,| x,| y,| rows,| file")}  \
	{print       $$10,    $$4, $$24,  $$8,    $$12,     $$14,     $$2,    $$20, $$26, $$28, $$22, $$16     \
	}' | sort -t, -nk2 | column -s,  -t 

rep:
	cat ~/tmp/rq1 | $(report1)

rq1:; mkdir -p ~/tmp;$(MAKE) go=guess eg1 | tee ~/tmp/$@.$(Stop); cat ~/tmp/$@.$(Stop) | $(report1)
rq2:
	$(MAKE) Stop=20 rq1; $(MAKE) Stop=80 rq1;
	$(report2)
             
#pass==2 { d= (x[$$16] - $$24); d=d> $$30 * 0.2 ? d : 0; print(   d/x[$$16], $$24,$$16, $$30)}\

report2=gawk 'BEGIN {OFS="\t"; CONVFMT="%.2f"} \
             pass==1 { x[$$16]=$$24 } \
             pass==2 { d= (x[$$16] - $$24); d=d> $$30 * 0.2 ? d : 0; print($$4, x[$$16], $$24, $$30 * .2, $$16)}\
            ' pass=1 ~/tmp/rq1.20 pass=2 ~/tmp/rq1.80 | sort -nk 2

report3='BEGIN { print FILENAME }                     \
               { for(i=1;i<=6;i+=2) {                  \
							     seen[$$i] += $$(i+1) ;                 \
				           if (sub(/\*/,"",$$(i+1))) win[$$i]++ }}\
				 END  { for(w in seen) print w,win[w]/NR, seen[w]/NR }'

rq4:
	$(foreach s,12 24 48 98, $(MAKE) Stop=$s go=order eg1 | tee ~/tmp/$@.$s; )

rq4a:
	$(foreach s,12 24 48 98, echo $s; gawk --source $(report3) ~/tmp/rq4.$s;)
