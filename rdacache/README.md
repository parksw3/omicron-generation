This is a directory for .rdx files that we want to cache sometimes to save computation.

The project should work without caching.

There is no automatic caching; to store .rdx files in the cache, use `make update_cache` to put any new files into the cache, which will be sunk by `make sync`. To save time by loading updated .rdx files, say `make use_cache`.

2022 Jul 18 (Mon): check when and whether this will properly break the chain (i.e., check the rsync time stamps to make sure that they are new).
