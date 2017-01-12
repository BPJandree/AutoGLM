# AutoGLM
This is an R package for automated GLM modelling suitable for large datasets. The package resulted from the needs for automated procedures for the statistical calibration of the [LUISA model framework](https://ec.europa.eu/jrc/en/luisa) that is widely adopted by the European Commision for ex-ante evaluation of policies. Updated version is on it's way.

# Current Version
Current version is 1.0.1. Reinstall if you're still on v.1.0.0.

# Release Note for v1.0.1.

Release contains several crucial updates:
- Solved an issue with op.t in autoGLM().
- Solved important issues in the opt.h routine. Added a functionality for both single and joint tests.
- Changed wd() as defaultoath to getwd().
- Fixed the warning message: In if (reclasstable == "default") { :the condition has length > 1 and only the first element will be used. 
- The ITdata set has been edited. Several missing values coded with integers have been removed.
- Fixed issues with actions=c(...). 
- Added additional functionality for straightforward estimation of logit models and integrated the gmm logits within this general command.
- guessStartVal gmm functionality added, robustified the routines.
- Removed several missing values observations from the default dataset.
- Renamed getCall() to getCall2() to avoid namespace issues with the stats package.
- Updated the example codes.
- Several fixes in the reclassification function.
- Moved most of the miscellaneous functions inside routines that use them to get a cleaner manual.
- Several minor bugg fixes.

# Next Release
Provide me with feedback!

# Known bugs (Will be fixed with next update!)
Provide me with feedback!

**Example code:** You can view some example code here [autoGLM_examples.r][examples], or have a look at the [vignette][vignette].

[examples]:https://github.com/BPJandree/AutoGLM/blob/master/autoGLM_examples.r
[vignette]:https://github.com/BPJandree/AutoGLM/blob/master/autoGLM.pdf

Feel free to grab the code and contact me if you need help. Just run the following in your R terminal:

	library(devtools)
	install_github("BPJandree/AutoGLM")


