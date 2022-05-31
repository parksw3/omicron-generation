
## Flow

To make the whole project from source (~17 minutes on a new-ish, nice-ish desktop):

```
git clone https://github.com/parksw3/omicron-generation.git ##
cd omicron-generation
make Makefile
make pullall
## make use_cache
make omicron-generation.pdf
```

To make just the figures and the manuscript (using cached versions of the calculation files), uncomment the use_cache command

## Organization

The main tex document is in this directory.

* R/ has some files with R functions
* scripts/ has programs which do the main analysis
* figures/ has programs which make the figures
* rdaout/ is where scripts outputs go
* rdacache/ is for saved copies of rdaout/ files. The idea is to avoid re-running slow programs, but we haven't yet carefully distinguished slow programs from others

There is a lot of possibly-confusing linking: many of the directories use symbolic links for each other to avoid "../" calls. This is all supposed to be handled smoothly by make.
