# tidysynth 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Resolved spelling errors. 
* Fixed bug in `generate_predictors()` for edge-cases where no aggregate function is supplied with a NULL time_window.
* Adjusted ordering bug that can result in inconsistencies in the optimized weights when changing the time_window.
* Updated unit-tests to correspond with ordering bug fix on the weights.
* Add check the ensures post-treatment periods are included when running `grab_significance()`.
* Fixed issue where differences in temporal ordering in the data resulted in downstream distortions of the package output. Packaged now ensures the sequential time ordering when storing the original input data.
* Fixed `synthetic_control()` when the treated unit doesn't come first in the data. 
* Drop dependency on LowRankQP, which is no longer maintained. The existing default optimization method ("ipop") is now the sole optimization method used in the package. 
