# AutoGLM
This is an R package for automated GLM modelling suitable for large datasets.

# Known bugs
This package is still in alpha, you can contact me if you encounter problems. Some bugs are known:
- Using option "opt.t" in autoGLM() does currently not work. opt.t() command itself is functional. You can also use the opt.t routine in generalizeToSpecific().
- The opt.h routine is broken. I will look into it as soon as possible. In general I always recommend to use opt.ic, but if you have specific need for optimization using LR, F or Chisq tests, opt.h. is for you. If you want to use it as soon as possible, contact me and I will make repair of this line of code an urgent matter.
-By default, the outputpath (if you do not specify one) is you working directory. If no default set in your system32 settings, the command will terminate with an error unless you have spcified an outputpath. You can run getwd() to check this whther you have set up a working directory.
- You will receive a warning messages: In if (reclasstable == "default") { :the condition has length > 1 and only the first element will be used. You can ignore this, I will fix it when I have time.
- Not a bug, but I will edit the data set. The elevation data includes a few observations with negative values. If you normalize the elevation data using the normalize() command, it will effectively remove all the variation in the data and glm models will not work. 
- Not specifying all actions will result in an error, though everything works properly. I will look in to this, for now I recommend to always use actions=c("write", "print", "log", "return")
- guessStartVal using gmm is on the todo list, but not has no priority because gmm runs very heavy on large datasets.


**Example code:** You can view some example code here [autoGLM_examples.r][examples], or have a look at the [vignette][vignette].

[examples]:https://github.com/BPJandree/AutoGLM/blob/master/autoGLM_examples.r
[vignette]:https://github.com/BPJandree/AutoGLM/blob/master/autoGLM.pdf

Feel free to grab the code and contact me if you need help. Just run the following in your R terminal:

	library(devtools)
	install_github("BPJandree/AutoGLM")


