
figures/ uses .rda files from rdaout/ (a parallel directory between all the project directories). 
These files are automatically made in scripts/, but with a dependency-free recipe.

• With a fresh clone, `make all` (after setup) should make everything
• To use cached files from the scripts/ directory, say `make use_cache`

Changes in the scripts/ directory will not find their way to the MS unless the files in rdaout are cleared (`make invalidate_cache` will delete all of them). ## DOES NOT WORK 2022 Jul 22 (Fri)

After changing things in scripts/ and remaking, use `make update_cache` to update the cache.
