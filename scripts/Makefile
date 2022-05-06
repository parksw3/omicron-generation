## This is the analysis subdirectory of omicron-generations

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def



vim_session:
	bash -cl "vmt"

######################################################################

autopipeR = defined
pardirs += rdaout data R

Ignore += $(pardirs)

######################################################################

autopipeR = defined

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

## -include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
