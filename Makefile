## This is a new, repo Makefile for omicron generations 2022 Apr 21 (Thu)

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

## Structure

Sources += README.md

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
	rsync rdacache/*.rda rdaout/

update_cache:
	rsync rdaout/*.rda rdacache/

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
