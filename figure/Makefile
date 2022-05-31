## This is the figure subdirectory of omicron-generations

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

autopipeR = defined
pardirs += rdaout data R scripts

Ignore += $(pardirs)

.PRECIOUS: data/%
data/%:
	$(MAKE) data

.PRECIOUS: R/%.R
R/%.R:
	$(MAKE) R

.PRECIOUS: scripts/%
scripts/%:
	$(MAKE) scripts

######################################################################

Sources = $(wildcard *.R)
autopipeR = defined

## compare.ggp.pdf: compare.R 
# compare.Rout: compare.R R/serialfun.R rdaout/calculate_incubation_mle.rda rdaout/fit_lognormal_base_comb_within.rda rdaout/fit_lognormal_base_comb_between.rda rdaout/fit_lognormal_comb_within_r.rda rdaout/fit_lognormal_comb_between_r.rda
# 	$(pipeR)

## figure 1
figure_diagram.Rout: figure_diagram.R
	$(pipeR)

## figure 2
figure_epidemic.Rout: figure_epidemic.R rdaout/cases.rda
	$(pipeR)

## figure 3
## figure_incubation.rtmp: figure_incubation.R 
figure_incubation.Rout: figure_incubation.R rdaout/calculate_incubation_r.rda
	$(pipeR)
	
## figure 4
figure_compare_main.Rout: figure_compare_main.R R/serialfun.rda rdaout/serialdata.rda rdaout/calculate_incubation_mle.rda rdaout/fit_lognormal_base_comb_within.rda rdaout/fit_lognormal_base_comb_between.rda rdaout/fit_lognormal_comb_within_r.rda rdaout/fit_lognormal_comb_between_r.rda
	$(pipeR)
	
## process data more for figures... should really belong to /scripts
advantage.Rout: advantage.R rdaout/cases.rda rdaout/fit_lognormal_base_comb_within.rda
	$(pipeR)
	
## figure 5
figure_reproduction_advantage.Rout: figure_reproduction_advantage.R advantage.rda
	$(pipeR)
	
## supp figures

figure_compare_rho.Rout: figure_compare_rho.R rdaout/fit_lognormal_base_comb_rho.rda
	$(pipeR)
	
figure_compare_stratified.Rout: figure_compare_stratified.R R/serialfun.R rdaout/fit_lognormal_r_50_within.rda rdaout/fit_lognormal_r_50_between.rda rdaout/fit_lognormal_r_51_within.rda rdaout/fit_lognormal_r_51_between.rda
	$(pipeR)
	
advantage_between.Rout: advantage_between.R rdaout/cases.rda rdaout/fit_lognormal_base_comb_between.rda
	$(pipeR)
	
figure_reproduction_advantage_between.Rout: figure_reproduction_advantage_between.R advantage_between.rda
	$(pipeR)	
	
######################################################################

.PRECIOUS: rdaout/%.rda
rdaout/%.rda: $(wildcard scripts/*.R)
	$(MAKE) scripts
	cd scripts && $(MAKE) $@

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
