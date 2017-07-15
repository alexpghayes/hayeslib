# hayeslib

This package is for personal experimentation, inspired by Hilary Parker's [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) post.

Currently the package is serving as a somewhat messy code and notes repository. I'm in the process of turning the code snippets into documented and tested functions.

### Next steps

* `zeros_like` and `ones_like` as in Numpy
* `show_dir`: nicely output directory structure
* `combine`: type happiness?
* `perc`: vectors, and count/tally like
* `retype`: rename and type convert at once
* `replace_if`: if condition, replace with value, otherwise leave be, vectorized
* `count_if`: count when predicate occurs for tidyselected columns
* `ggpie`: upgrade to tidyeval handle labels better (i.e. just get them from a label column in the original data frame)
* `spread_keep_`: use same doc as `spread_keep`
* README: document functions that other people might find useful
* Pass R CMD check
* ALL THE BADGES
* Installation instructions
* Example usages
* Automatic integration with Travis CI / Appveyor
* Automatic code coverage testing
* Using `pkgdown`
* How and when to use branches during package development (especially in case people actually end up using your code and you need things to be stable)
* Using the Github issues, projects, wiki ecosystem
* update gentle tidyeval: adding select/mutate semantics, cleanup, note that things may change, also maybe use tufte-esque blogdown theme
