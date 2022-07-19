## This is omicron_generations

all: omicron-generation.pdf
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt README.md"

######################################################################

## Structure

Sources += README.md letter.md

subdirs += figure scripts
Ignore += $(subdirs)
hotdirs += $(subdirs)
alldirs += $(subdirs)

Sources += $(wildcard R/*.R)

######################################################################

## Main document
Sources += $(wildcard *.tex *.bib)

omicron-generation.pdf: omicron-generation.tex

######################################################################

## Cache stuff
Sources += rdacache/README.md $(wildcard rdacache/*.rda)
Ignore += rdaout

use_cache:
	rsync -aur rdacache/*.Rout rdacache/*.rda scripts/

update_cache:
	rsync -aur rdaout/*.rda rdaout/*.Rout rdacache/
	git add -f rdacache/*.Rout

rdaout:
	$(mkdir)

######################################################################
### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk
-include makestuff/hotcold.mk

-include makestuff/git.mk
-include makestuff/visual.mk
