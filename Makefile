## This is a new, repo Makefile for omicron generations 2022 Apr 21 (Thu)

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.tex)

subdirs += figure scripts
Ignore += $(subdirs)
hotdirs += $(subdirs)
alldirs += $(subdirs)

omicron-generation.pdf: omicron-generation.tex

Sources += rdaout/README.md $(wildcard R/*.R)

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

-include makestuff/texi.mk
-include makestuff/hotcold.mk

-include makestuff/git.mk
-include makestuff/visual.mk
