## This is the analysis subdirectory (scripts) of omicron-generations

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

autopipeR = defined
Sources += $(wildcard *.R)

pardirs += rdaout data R
Ignore += $(pardirs)
Makefile: | $(pardirs)

.PRECIOUS: data/%
data/%: 
	$(MAKE) data

.PRECIOUS: R/%.R
R/%.R: 
	$(MAKE) R

serialdata.Rout: serialdata.R data/serial-netherlands.xlsx

baseparam.Rout: baseparam.R
	$(pipeR)

calculate_incubation_mle.Rout: calculate_incubation_mle.R baseparam.rda sample_incubation.rda

sample_incubation.Rout: sample_incubation.R
	$(pipeR)

## process data for figures
cases.Rout: cases.R data/COVID-19_aantallen_gemeente_per_dag.csv data/variant-netherlands.xlsx
	$(pipeR)

######################################################################

## lognormal fits

fitdeps = R/serialfun.rda R/fitfun.rda sample_incubation.rda serialdata.rda baseparam.rda
betdeps = $(fitdeps) rdaout/calculate_incubation_mle.rda
witdeps = $(fitdeps) rdaout/fit_lognormal_base_comb_within.rda
bothdeps = $(witdeps) rdaout/calculate_incubation_mle.rda

fit_lognormal_base_comb_rho.Rout: fit_lognormal_base_comb_rho.R $(bothdeps)

fit_lognormal_base_comb_within.Rout: fit_lognormal_base_comb_within.R $(betdeps)

fit_lognormal_comb_within_r.Rout: fit_lognormal_comb_within_r.R $(witdeps)

fit_lognormal_base_50_between.Rout: fit_lognormal_base_50_between.R $(betdeps)

fit_lognormal_base_comb_between.Rout: fit_lognormal_base_comb_between.R $(betdeps)

fit_lognormal_base.Rout: fit_lognormal_base.R $(betdeps)

fit_lognormal_base_rho.Rout: fit_lognormal_base_rho.R fit_lognormal_base.rda $(bothdeps)

fit_lognormal_comb_between_r.Rout: fit_lognormal_comb_between_r.R rdaout/fit_lognormal_base_comb_between.rda $(fitdeps)

fit_lognormal_r_50_between.Rout: fit_lognormal_r_50_between.R fit_lognormal_base.rda $(fitdeps)

fit_lognormal_r_50_within.Rout: fit_lognormal_r_50_within.R rdaout/fit_lognormal_base.rda $(fitdeps)
fit_lognormal_r_51_between.Rout: fit_lognormal_r_51_between.R rdaout/fit_lognormal_base.rda $(fitdeps)
fit_lognormal_r_51_within.Rout: fit_lognormal_r_51_within.R rdaout/fit_lognormal_base.rda $(fitdeps)

calculate_incubation_r.Rout: calculate_incubation_r.R baseparam.rda sample_incubation.rda

# calculate_gauteng.R

######################################################################

## Automatically copy things that we might want to cache
.PRECIOUS: rdaout/%
rdaout/%: %
	$(MAKE) rdaout
	$(copy)

######################################################################

autopipeR = defined

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/01.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
