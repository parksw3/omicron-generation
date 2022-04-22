## This is the figure subdirectory of omicron-generations

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

autopipeR = defined
pardirs += rdaout data

figure_compare_he.Rout: figure_compare_he.R
figure_compare_he.Rout: figure_compare_he.R
figure_compare_main.Rout: figure_compare_main.R
figure_compare.Rout: figure_compare.R
figure_compare_rho.Rout: figure_compare_rho.R
figure_compare_stratified.Rout: figure_compare_stratified.R
figure_diagram.Rout: figure_diagram.R
figure_epidemic.Rout: figure_epidemic.R
figure_incubation.Rout: figure_incubation.R
figure_reproduction_advantage_between.Rout: figure_reproduction_advantage_between.R
figure_reproduction_advantage_new.Rout: figure_reproduction_advantage_new.R
figure_reproduction_advantage.Rout: figure_reproduction_advantage.R

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

-include makestuff/git.mk
-include makestuff/visual.mk
