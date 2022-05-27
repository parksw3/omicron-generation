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

data/%: 
	$(MAKE) data

fit_lognormal_base_comb_rho.Rout: fit_lognormal_base_comb_rho.R R/serialfun.rda R/fitfun.rda serialdata.rda baseparam.rda rdaout/calculate_incubation_mle.rda rdaout/fit_lognormal_base_comb_within.rda

serialdata.Rout: serialdata.R data/serial-netherlands.xlsx

baseparam.Rout: baseparam.R
	$(wrapR)

calculate_incubation_mle.Rout: calculate_incubation_mle.R baseparam.rda sample_incubation.rda

sample_incubation.Rout: sample_incubation.R
	$(wrapR)

fit_lognormal_base_comb_within.Rout: fit_lognormal_base_comb_within.R R/serialfun.rda R/fitfun.rda serialdata.rda baseparam.rda rdaout/calculate_incubation_mle.rda

# calculate_gauteng.R
# calculate_incubation_r.R
# fit_lognormal_base_50_between.R
# fit_lognormal_base_comb_between.R
# fit_lognormal_base.R
# fit_lognormal_base_rho.R
# fit_lognormal_comb_between_r.R
# fit_lognormal_comb_within_r.R
# fit_lognormal_r_50_between.R
# fit_lognormal_r_50_within.R
# fit_lognormal_r_51_between.R
# fit_lognormal_r_51_within.R
# serialdata.R

######################################################################

## Automatically copy things that we might want to cache
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

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
