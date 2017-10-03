
#' A sample dataset produced with \code{\link{getSamples}} with 157481 obersvations representing 100x100 meter resolution grid cells of Corine land cover data for Italy with 26 exogenous variables. Variable names starting with a "w" indicate spatial lags.
#'
#' To inspect the data, describe(data(ITdata))
#'
#' @name ITdata
#' @docType data
#' @usage data(ITdata)
#' @format a data frame with 157481 rows and 27 columns
#' @keywords datasets
NULL

#' A reclassification table for Corine land cover to a simplified aggregation used in the LUISA platform of the JRC.
#'
#' A table that maps 51 Corine Land-cover classes into 8 classes and a NoData class.
#' \itemize{
#' 	\item 0. Urban.
#' 	\item 1. Industry.
#' 	\item 2. Arable.
#' 	\item 3. Permanent crops.
#' 	\item 4. Pastures.
#' 	\item 5. Forest.
#' 	\item 6. Herbaceous cover.
#' 	\item 16. Transitional.
#' 	\item -9999. NoData.
#' }
#'
#' @name corinetable
#' @docType data
#' @usage data(corinetable)
#' @keywords datasets
NULL


#' A function to load and install packages.
#'
#' This function checks if a package is installed before loading it, and pulls it from the nearest CRAN server if the package is missing.
#' @param package Character string indicating th package to be loaded. Accepts a vector of packages to load multiple packages.
#' @param silent Should startup messages printed by the packages be surpressed? Will not surpress warnings or errors. Defaults to FALSE.
#' @keywords package loading
#' @return The messages printed by the loaded package, or a message if the package is not found.
#' @export
#' @examples
#' verbose_packages <- c("gmm", "foreign", "sp")
#' silent_packages <- c("data.table", "compiler", "speedglm")
#'
#' pkgTest(verbose_packages)
#' pkgTest(silent_packages)


pkgTest <- function (package, silent=FALSE){

	pkgTest3 <- function (package, silent) {
		timeout=60
		pkgTest2 <- function(package)
		  {
		    if (!require(package,character.only = TRUE))
		    {
		      install.packages(package,dep=TRUE,repos='http://cloud.r-project.org/')
		        if(!require(package,character.only = TRUE)) stop("Package not found")
		    }
		  }
		 if(silent){
			suppressPackageStartupMessages(pkgTest2("R.utils"))
		} else {
			pkgTest2("R.utils")
		}
		f <- function (){tryCatch(evalWithTimeout(pkgTest2(package), timeout=timeout,
	  		onTimeout=c("error")), error = function(e) {stop("Package loading got stuck.")})}
		if (silent){suppressPackageStartupMessages(f())} else {f()}
		}

		if (silent == FALSE){
			pload <- function(package){
				pkgTest3(package, silent = FALSE)
			}
		} else {
			pload <- function(package){
				pkgTest3(package, silent = TRUE)
			}
		}

	loadobject <- lapply(package, pload)
	rm(loadobject)
}




#' A function for memory management.
#'
#' A call to tgc() forces a garbage collection if memory usage is above a certain threshold, by default when approximately half of the available mememory is in use by R.
#' Calling tgc(0) performs a standard garbage collection and is therefore the same as gc() with the additional option to delete the output of gc() from memory,
#' such that the call is entirely silent. The function makes a request to the OS (windows only support) to retun memory usage statistics, if this call fails garbage collection will be forced even if memory usage is below the supplied threshold.
#' @param maxmemshare Numeric between 0 and 1 indicating the threshold share of memory usage that triggers garbage collection.
#' @param verbose Should diagnostics be printed? FALSE by default.
#' @param emptythrash Should the object that containsthe output from the garbage collection, including messages, be deleted from memory? TRUE by default. If TRUE, tgc() is silent.
#' @return Prints diagnostics of the garbage collection, or silent when emptytrash is TRUE.
#' @export
#' @examples
#' tgc()
#'
#' tgc(maxmemshare=0, verbose = FALSE, emptytrash = TRUE)
#'
#' tgc(0, TRUE, FALSE)

tgc <- function (maxmemshare=.5, verbose =FALSE, emptythrash = TRUE){
	garbageStatus <- function (maxmemshare){
		request<-function(maxmemshare){
			return(if((memory.size()/memory.limit())>maxmemshare){TRUE}else{FALSE})
		}
		return(suppressWarnings(tryCatch(request(maxmemshare), error = function (e) {TRUE})))
	}
	if(garbageStatus(maxmemshare)){
		can<-gc(verbose)
		if(emptythrash){rm(can)}else(return(can))
	}
}


#' A function to generate random data from a logit model.
#'
#' This function generates random data simulated from a logit model based on the parameters supplied.
#' @param nobs number of observations that the returned dataset should include. Defaults to 2500.
#' @param pars A vector of parameters to be used in the data generating process. Regressors may have a 0 coefficient. Defaults to 0 for all variables.
#' @keywords generate random logit data
#' @return A simulated dataset from a logistic probability model with nobs observations, a binary response variable, and nxregs regressors ranging from 0 to 100.
#' @export
#' @examples
#' logitpars <- c(-0.5, 0.4, -0.3, 0.2, -0.1, 0, 0, 0, 0, 0, 0, 0)
#' logit_data <- simulateLogit(nobs=2500, pars=logitpars)
#'
#' random_binaryResponse_data <- simulateLogit(nobs=500)

simulateLogit <- function (nobs=2500, pars=c(0,0,0,0,0,0,0,0,0,0)) {
  nxregs=length(pars)
  randomStandardizedBinDF <- function (nobs, nxregs) {
    N=nobs
    Y=sample(0:1,N, replace = TRUE)
    data = matrix(nrow=N, ncol = (nxregs+1))
    data[,1] <- Y
    for (c in 2:ncol(data)){
      data[,c] <- sample(1:100,N, replace = TRUE)
    }
    return(data)
  }
  gen = randomStandardizedBinDF (nobs, nxregs)
  Xdata <- gen[,-1]

  pr <- logistic(pars, Xdata)

  Ydata <- rbinom(nobs,1,pr)

  returndf <- data.frame(cbind(Ydata,Xdata))
  nc<-ncol(Xdata)
  if(is.null(nc)){nc<-1}
  colnames(returndf) <- c("Y", c(paste("x",as.character(1:nc),sep="")))
  return (returndf)
}



#' Simple wrapper to estimate a binomial logit model by IWLS or GMM.
#'
#' This function returns an estimated logit model.
#' @param Z dataset with dependent variable in the first column and explanatory variables in other columns.
#' @param method either IWLS to estimate a glm by Iterated Weighted Least Squares, or GMM to estimate with method of moments.
#' @param start starting values, defaults to vector of zeros for IWLS and \code{\link{guessStartVal}} for gmm.
#' @param maximizer, maximizer used for GMM, either "nlminb" for port routines or "BFGS".
#' @param gmmtype, Either "twostep" for the two step GMM proposed by Hansen(1982),
#' and the "cue" and "iterative" for respectively the continuously and the
#' iteratively updated methods proposed by Hansen, Eaton et Yaron (1996). See package "gmm" for details. Defaults to "iterative".
#' @param wmatrix Defaults to "ident" for identity matrix weighting of the gmm estimator. Other option is "optimal" for inverse covariance weighting. See package "gmm" for details.
#' @keywords biomial logit estimation
#' @return A logit model fitted by glm()
#' @export
#' @examples
#' set.seed(10101)
#' Z <- simulateLogit(nobs=1000, pars =c(0.25,-0.5, 0.1, -0.1, 0, 0))
#'
#' # Iterated Weighted Least Squares
#' test<- logit(Z)
#' # GMM using port routines and the results of the IWLS as starting values
#' test2<- logit(Z, method="gmm", start=coef(test))
#' # GMM using BFGS and starting at correct parameters
#' test3<- logit(Z, method="gmm", maximizer="BFGS", start =c(0, 0.25,-0.5, 0.1, -0.1, 0, 0))
#' test4<- logit(Z, method="gmm")
# # depending of the starting values, gmm may have problems estimating the covariance matrix when the data is quasi-completely separable. Carefully read the model output.
# test5<- logit(Z[,-2], method="gmm") # a mis-specified model.

logit <- function (Z, method="IWLS", start="default", maximizer="nlminb", wmatrix="ident", gmmtype="iterative"){
  if (method=="IWLS"){
    if(length(start)==1){
      if(start == "default"){
        start <- c(1:(ncol(Z)))*0
      }
    }
    return(glm(formula(Z), start=start, family =binomial(link="logit"), data=Z))
  }

  if (method =="gmm"){
    easygmmlogit <-function(Y, X, start="default", maximizer="nlminb") {
      pkgTest("gmm")
      if(length(start)==1){
        if(start == "default"){
          start <- guessStartVal(Y, X, model="logit")#c(1:(ncol(X)+1))*0
        }
      }
      buildgmmdata <- function (Y, X){
        return (data.matrix(cbind(Y, cbind(constant=1,X))))
      }

      moments <- function (theta, data){
        y <- as.numeric(data[,1])
        x <- data.matrix(data[,2:ncol(data)])
        m <- x * as.vector((y - logistic(theta, x)))
        return(cbind(m))
      }

      if (maximizer == "nlminb"){
        gmmlogit <- gmm(moments, x = buildgmmdata(Y, X), t0 = start, type = gmmtype, crit = 1e-10, wmatrix =wmatrix, optfct = maximizer)
      }
      if (maximizer == "BFGS"){
        gmmlogit <- gmm(moments, x = buildgmmdata(Y, X), t0 = start, type = gmmtype, crit = 1e-10, wmatrix =wmatrix, method = maximizer)
      }
      return(gmmlogit)
    }
    easygmmlogit (Y=Z[,1], X=Z[,-1], start=start, maximizer=maximizer)
  }
}



#' A function for reclassification of integer data.
#'
#' This function works similar to ArcMap's raster reclassify function, only fast and without starting hell on earth.
#' @param LUdata A dataset (vector, matrix or dataframe) that contains data to be reclassified, e.g., CORINE land cover data. If an object with multiple collumns is supplied, the function will reclassify only the first collumn.
#' @param reclasstable the path to a .csv file containing the reclass table.
#' By default, the function loads "corinetable", supplied with the package. This table maps Corine land-cover into the Classification scheme used in the LUISA framework of the Joint European Research centre.
#' To inspect the table: data(corinetable); describe(corinetable).
#' @param JIT a boolean indicating whether Just in Time compilation should be used. Defaults to TRUE.
#' @param dropNA TRUE/FALSE indicating whether values that are reclassified into NoData should be dropped from the output. By default TRUE.
#' @param NAval the value in the reclasstable that corresponds to noData. By default the function assumes that the classes have positive values, and that
#' that the lowest negative value corresponds to the class that should be dropped (e.g., if your relcass table maps 1,2,3 into 1,2,-9999, the function will return a dataset with classes 1,2., and treat 3 as NoData, dropping it unless specified to keep NoData in the output).
#' @return A vector of reclassified values, or matrix with the firs column being reclassified.
#' @export
#' @examples
#'
#' A=c(1,2,3,4,5,6,7,8,9)
#' B=c(9,8,7,6,5,4,3,2,1)
#' reclasstable=cbind(A,B)
#'
#' reclassify(A, reclasstable)
#' # by default, the data is not reclassified
#' reclassify(A)
#'
#' # however, if dropNA is TRUE, thge default NA value is dropped (the most negative value)
#' C = c(1,2,3,4,5,6,7,8,9,-9999)
#' reclassify(C)
#'
#'
#'
#' data(ITdata)
#' data(corinetable)
#'
#'
#' # reclassify a vector of land-use data using the corine_to_LUISA scheme:
#' landcover <- reclassify(LUdata=ITdata[,1], reclasstable=corinetable, JIT=TRUE)
#'
#' #reclassify a multi-collumn dataset in which the first collumn represents factorial data:
#' reclass_IT <- reclassify(LUdata=ITdata, reclasstable=corinetable, JIT=TRUE)
#'
#'
#'
#' # the LUISA codes are a simplification and drop certain land cover classes. By default,
#' # the categories reclassified into the lowest negative values, are dropped.
#' # If you wish to keep all categories:
#' reclass_IT <- reclassify(LUdata=ITdata, reclasstable=corinetable, dropNA=FALSE)
#'


reclassify <- function(LUdata, reclasstable="default", JIT=TRUE, dropNA = TRUE, NAval="default"){

  if(length((dim(LUdata)))>1){
    LUvec = LUdata[,1]
    if (is.character(reclasstable)){
      reclasstable <- cbind(LUdata[,1],LUdata[,1])
    }
  } else {LUvec = LUdata
  if (is.character(reclasstable)){
    reclasstable <- cbind(LUdata,LUdata)
  }
  }

  if(dropNA){
    NAval = min(reclasstable[,2],-1)
  }

  maxval = max(LUvec)+1
  if (JIT == TRUE) {
    pkgTest("compiler", silent = TRUE)
    fast <- function (LUvec, reclasstable) {
      LU <- sort(unique(LUvec))

      for (lu in LU){
        LUvec[LUvec==lu] <- as.numeric(reclasstable[reclasstable[,1]==lu,][2])*maxval
      }
      return(LUvec)
    }

    fast.reclassify <- cmpfun(fast)
    LUvec <- fast(LUvec, reclasstable)
  } else {
    LU <- sort(unique(LUvec))

    for (lu in LU){
      LUvec[LUvec==lu] <- as.numeric(reclasstable[reclasstable[,1]==lu,][2])*maxval
    }
  }
  LUvec = LUvec/maxval

  if(length(dim(LUdata))>1){
    LUdata[,1]<- LUvec
    if(dropNA){
      return( if(NAval%in%LUdata[,1]){LUdata[LUdata[,1]!=NAval,]}else{LUdata} )
    } else {
      return(LUdata)
    }
  } else {
    LUdata<- LUvec
    if(dropNA){
      return( if(NAval%in%LUdata){LUdata[LUdata!=NAval]}else{LUdata} )#(LUdata[LUdata!=NAval,])
    } else {
      return(LUdata)
    }
  }
}




#' A simple function to describe a dataset.
#'
#' This function returns a dataframe with variable names, their minimum and maximum values, means and standard deviations.
#' @param X Matrix or dataframe to be summarized.
#' @return A dataframe with names and statistics.
#' @export
#' @examples
#' someVector = 1:10
#' describe(someVector)
#'
#' df <- data(ITdata)
#' description <- describe(df)
#' print(description)

describe <- function (X){
	Xcols = max(1,ncol(X))
	describeframe = data.frame(matrix(NA,ncol = 5))
	if (Xcols>1){
		for (x in 1:Xcols){
			name = if(is.null(colnames(X)[x]) == FALSE){(colnames(X)[x])} else {paste("variable",as.character(x),sep="")}
			min = (min(X[,x]))
			max = (max(X[,x]))
			mean = (mean(X[,x]))
			sd = (sd(X[,x]))
			description = c(name = name, min = min, max = max, mean = mean, sd = sd)
			describeframe[x,] <- description
		}
	} else {
		X=cbind(X,X)
		for (x in 1:2){
			name = if(is.null(colnames(X)[x]) == FALSE){(colnames(X)[x])} else {paste("variable",as.character(x),sep="")}
			min = (min(X))
			max = (max(X))
			mean = (mean(X))
			sd = (sd(X))
			description = c(name = name, min = min, max = max, mean = mean, sd = sd)
			describeframe[x,] <- description
		}
		describeframe=describeframe[1,]
	}
	colnames(describeframe) <- c("name", "min", "max", "mean", "sd")
	return(describeframe)
}

#' A simple function to normalize a dataset of variables to the 0-1 interval.
#'
#' This function returns a dataframe with normalized variables.
#' @param X Matrix or dataframe to be normalized.
#' @return A dataframe with normalized values ranging from 0 to 1.
#' @export
#' @examples
#' someVector = 1:10
#' normalize(someVector)
#' df <- data(ITdata)
#' normalizedIT <- normalize(df)
#' describe(normalizedIT)

normalize <- function (X){
	Xcols = max(1,ncol(X))
	if (Xcols > 1){
		for (x in 1:Xcols){
			col = X[,x]
			normalizedcol = (col - min(col))/max((col - min(col)))
			X[,x]<-normalizedcol
				}
		} else {
			col = X
			normalizedcol = (col - min(col))/max((col - min(col)))
			X<-normalizedcol
		}
	return(X)
}

#' A simple function to extract information criteria from a model object.
#'
#' This function returns a Kullback-Leibler Information Criterion.
#' @param model A model object that is supported by AIC(). For example produced by lm(), glm(), or speedglm().
#' @param KLIC either "AIC" for the AIC, or "AICc" for the corrected criterion. defaults to "AICc".
#' @param sigma.is.estimated boolean indicating whether variance has been estimated as part of the parameters (Likelihood), or not (LS), If FALSE, number of parameters is increased by 1 for comparison with MLE objects. Defaults to FALSE.
#' Should not impact the results when models of the same type are compared, but the option has been added to allow users to change the number of counted parameters when comparing MLE results with LS results.
#' @return numeric value.
#' @export
#' @examples
#' somepars <- c(-0.5, 0.4, -0.3, 0.2, -0.1, 0, 0, 0)
#' someData <- simulateLogit(nobs=2500, pars=somepars)
#' someModel <- logit(someData)
#' IC(someModel, KLIC="AIC")
#' AIC(someModel) # compare
#' IC(someModel) # AICc


IC <- function (model, KLIC = "AICc", sigma.is.estimated = FALSE){
	k <- function (model, sigma.is.estimated = FALSE){
		if(sigma.is.estimated == FALSE){s=1} else {s=0}
		return(length(coef(model))+s)
	}
	n <- function(model){
		return(length(model$residuals))
	}

	if(KLIC == "AIC") {return(AIC(model))
	} else if(KLIC == "AICc"){return(AIC(model) +(2*k(model)*(k(model) +1))/(n(model)- k(model) -1))
		} else {stop("supplied information criterion not supported")}
}


#' A function to export a csv file containing the weights file in a format that can be loaded into geoDMS applications.
#'
#' This function exports a csv file containing the names of the weights, coefficients and country name in a standardized format supported by geoDMS.
#' @param model The model object from which the estimated coefficients should be extracted.
#' @param originaldata The complete dataset from which the model data has been extracted. Used by the function to determine which variables have a zero weight.
#' @param modeldata The dataset on which the model is estimated. Used by the function to determine which variables have a zero weight.
#' @param coefnamelist A character vector containing the weight names that should be printed in the weights file.
#' @param outdir The directory to which the weightsfile should be exported.
#' @param modelname A string containing the name of the country that should be printed in the weightsfile.
#' @param filename The name of the exported weightsfile.
#' @return Any messages that may be printed by file(), writeLines() or close().
#' @export
#' @examples
#' logitpars <- c(-0.5, 0.4, -0.3, 0.2, -0.1, 0, 0, 0, 0, 0, 0, 0)
#' someData <- simulateLogit(nobs=2500, pars=logitpars)
#' useData <- someData[,c(1:6)]
#' someModel <- logit(UseData)
#'
#' exportWeightsfile(model = someModel, originaldata = someData, modeldata = useData,
#'					coefnamelist = colnames(someData[-1,]), outdir = "C:\\Users\\",
#'						modelname = "ImportantModel", filename = "important_result.csv")
#'
#' data(ITdata)
#' data(corinetable)
#' ITsample <- getSamples(data = ITdata, share = 0.25, confidence.alternative=0.85, max.iter =100)
#' sampledIT = ITdata[ITsample,]
#' reclIT <- reclassify(sampledIT,  reclasstable=corinetable)
#' trainY <- MLtoBinomData(reclIT[,1], class=0)
#' trainX = reclIT[,-1]
#' bestX <- selectX(trainY, trainX, share = 0.25, returntype="colnames")
#' # total accessibility and domestic accessibility are multicollinear.
#' bestlogit <- logit(cbind(trainY,trainX[,bestX]))
#' exportWeightsfile(model = bestlogit, originaldata = ITdata, modeldata = cbind(trainY,trainX),
#' 	coefnamelist = colnames(trainX), outdir = "C:\\Users\\",
#' 					modelname = "IT", filename = "Urban_weights.csv")


exportWeightsfile <- function (model, originaldata, modeldata, coefnamelist, outdir, modelname, filename) {
		tryCatch({
			allvars = c("Constant",colnames(originaldata))
			usedvars = c("Constant",intersect(colnames(originaldata), colnames(modeldata)))
			droppedvars = setdiff(colnames(originaldata), colnames(modeldata))

			params = 1:length(allvars)
			names(params) <- allvars
			estimated = coef(model)
			zero = droppedvars
			params[usedvars] <- estimated
			params[droppedvars] <- 0
			#coefnamelist[1]<-"Constant"
			names(params) <- c("Constant",coefnamelist)

			header = paste("ZoneName;",toString(names(params)))
			content = paste(paste(modelname, ";",sep=""), toString(as.character(params)))

			header <- gsub(",", ";", header)
			content <- gsub(",", ";", content)

			fileConn<-file(paste(outdir,filename, sep=""))
			writeLines(c(header,content), fileConn)
			close(fileConn)
		},
		error = function (e){
			stop("writing failed")
			})
	}

#' A function to extract small samples that maintain important characteristics of the population sample.
#'
#' This function returns a sample extracted from the supplied population data, that has a similar distribution to the supplied population dataset. The function is called by guessStartval() to estimate inital values for numerical optimization procedures, but can also be used directly to reduce the sample size such that computationally intensive models can be estimated on a representative sample of an entire dataset. The function makes use of var.test() to compute an F test for the ratio of sample/population variance, and t.test() to compare their means.
#' @param data The population data from which a sample needs to be taken.
#' @param share The size of the sample in terms of the share of the population data. Defaults to .25.
#' @param confidence.alternative The confidence level used in the F and t-tests defined as the probability level at which the alternative is accepted. For confidence.alternative = .9, we need less evidence to accept the alternative hypothesis that the samples are unequal than at confidence.alternative = .95, hence .90 is stricter than .95.
#' @param max.iter The maximum number of draws to be taken. The programm breaks either when a suitable sample is found or when max.iter is reached.
#' @param tracelevel Similar to a verbose statement. Should information be printed during execution? defaults to 1 for printing. set to 0 for no printing.
#' @param memorymanagement TRUE/FALSE indicating whether garbage collection should be forced using tgc(). Defaults to TRUE. Recommended setting for large datasets.
#' @return A sample of the population dataset that has significantly similar means and variances, or a message indicating that no suitable dat
#' @export
#' @examples
#' getSamples (data = ITdata, share = 0.025, confidence.alternative=0.90, max.iter =100)


getSamples <- function (data, share = .25, confidence.alternative=.90,
                        max.iter=50, tracelevel=1, memorymanagement = TRUE) {
  resample = TRUE
  iter = 0
  while (resample == TRUE){
    iter = iter+1
    if (tracelevel>0){
      message(paste("Getsample iteration:",as.character(iter)),"\r",appendLF=FALSE)
      flush.console()
    }

    samples = sort(c(sample(1:nrow(data), round(share*nrow(data)))))
    trainSample <- data[samples,]#round(100*data[samples,])/100
    #testSample <- round(100*data)/100#[-samples,]

    for (x in 1:ncol(data)){
      F <- var.test(trainSample[,x], data[,x])$statistic
      if(is.na(F)){stop(paste("could not compute an F statistic for column", as.character(x), "of the data. Is there variation in this variable?"))}
      QF <- qf(confidence.alternative, (length(trainSample)-1), (length(data)-1))

      #print(p)
      if (F > QF){
        resample = TRUE
        break
      } else {
        resample = FALSE
      }

      crit = qt((1-((1-confidence.alternative)/2)), df=length(trainSample[,x]))

      t = tryCatch(abs(t.test(trainSample[,x], mu = mean(data[,x]), alternative = "two.sided", var.equal=TRUE)$statistic),
                   error = function (e) {crit+1})

      if (t > crit){
        resample = TRUE
        break
      } else {
        resample = FALSE
      }

    }
    if (memorymanagement == TRUE){
      tgc()
    }
    if (iter >= max.iter){
      stop("max.iter reached, sampling failed")
    }
  }
  if (tracelevel>0){message(paste("total iterations", as.character(iter)))}
  return(samples)
}





#' The logistic function.
#'
#' Called by various routines in the package. May also be used to predict fitted conditional probabilities by supplying a set of variables and corresponding coefficients estimated from a logit model.
#' @param theta A vector of coefficients.
#' @param data A dataframe of multiple exogenous regressors.
#' @return A vector of values produced by a logistic formula under specified parameters and data.
#' @export
#' @examples
#' logitdata <- simulateLogit(1000, c(1,0.5,-0.5,-0.3))
#' model <- logit(logitdata)
#' pars <- coef(model)
#' # predict with logistic function
#' predicted <- logistic(pars, cbind(1,logitdata[,-1]))
#' # compare with data
#' describe(logitdata[,1])
#' # compare with predict function from R
#' describe(fitted(model))


logistic <- function (theta, data){
	data=data.matrix(data)
	return (1/(1 + exp(-data %*% theta)))
}



#' A function to efficiently obtain starting values for numerical optimization procedures. Used to initialize the "warm start" optimization routines in generalizeTospecific. The funciton itself uses a "warm start" algorithm over a growing dataset similar to a sieves estimator for an unbounded parameter space.
#' When (quasi-)complete seperation is detected in subsamples, the starting values are returned as a vector of zeros.
#' Only relevant when working with large datasets, (multiple times the size of the example data). Has some robustnes checks, returns a vector of zeros when the solution to the criterion is non-unique and the initial guess lands in a parameter regions of extreme values.
#' Multicollinear values will be return with a parameter guess of 0.
#'
#' This function is called by generalizeToSpecific(), but may also be called by users directly to obtain an initial gues of starting values to be passed on to easygmmlogit().
#' @param Y A binary response variable.
#' @param X A dataset containing multiple exogenous regressors.
#' @param model The model for which starting values should be estimated. Either "logit" or "probit" for the logit or probit model, or "gmm_nlminb" for a logit model estimated with gmm using PORT routines (reliable) or "gmm_bfgs" using the BFGS algorithm (fast, but still very slow compared to option "logit").
#' @param s1 Share of the sample used for the guess
#' @param s2 share of the subsample used to initialize the guess. If s2 =0.25, s1 =0.25, the guess is initialized at a .05 share of the entire dataset, or .25 of  s1*datasize.
#' @param c1 confidence of first sample, see \code{\link{getSamples}}.
#' @param c2 confidence of subsample, see \code{\link{getSamples}}.
#'#' @param tracelevel Whether information should be printed during execution. Defaults to 1 for printing, set to 0 for no printing.
#' @param memorymanagement TRUE/FALSE indicating whether garbage collection should be forec regularly when memory usage is high. Defaults to TRUE, recommended setting for large datasets.
#' @return A vector of coefficients that can be passed on to numerical optimization algorithms.
#' @export
#' @examples
#' set.seed(234)
#'
#' randomlogit <- simulateLogit(nobs=50000, pars = c(0.25, -0.2, -0.3, 0.1, 0.05, 0.025, 0.01,
#'                              0.005, 0.005, 0.005,0.005,0.0025,
#'                              0.0025,0.0025,0.0025,0,0,0,0,0,0))
#'
#' Y=randomlogit[,1]
#' X=randomlogit[,-1]
#'
#'
#'
#' # i5 4570 @ 3.2 GHz
#' system.time(guessStartVal(Y, X, model="logit"))
#' # user  system elapsed
#' 0.29    0.00    0.30
#' system.time(logit(cbind(Y,X)))
#' # user  system elapsed
#' 0.79    0.00    0.80
#' # the IWLS algorithm used for glm is already quite fast.
#'
#' system.time(guessStartVal(Y, X, model="gmm_nlminb"))
#' # user  system elapsed
#' 40.78    4.17    45.35
#' system.time(logit(cbind(Y,X), method ="gmm"))
#' # user  system elapsed
#' 179.52   27.55  207.76
#' # the difference for gmm is quite large. It pays to do:
#' system.time(logit(cbind(Y,X), method ="gmm", start=guessStartVal(Y, X, model="logit")))
#' # user  system elapsed
#' 160.55   21.79  182.48



guessStartVal <- function (Y, X, model="logit", s1=.25, s2=s1, c1=.85, c2=c1, tracelevel =1, memorymanagement = TRUE){
  guessStartVal1 <- function (Y, X, model, tracelevel, memorymanagement) {
    if (tracelevel > 0){
      message("Sampling from data")
    }
    samples2 <- getSamples (data = cbind(Y,X), share = s1, confidence.alternative=c1, max.iter =50, tracelevel, memorymanagement)
    if(is.numeric(samples2) == FALSE){
      samples2 = sample(1:length(Y),length(Y)*0.5)
    }
    x2 = X[samples2,]
    y2 = Y[samples2]
    if(memorymanagement == TRUE) {rm(samples2)}
    if (tracelevel > 0){
      message("Sampling from sample")
    }
    samples <- getSamples (data = cbind(y2,x2), share = s2, confidence.alternative=c2, max.iter =50, tracelevel, memorymanagement)
    if(is.numeric(samples) == FALSE){
      samples = sample(1:length(y2),length(y2)*0.5)
    }
    x = x2[samples,]
    y = y2[samples]
    if(memorymanagement == TRUE) {rm(samples)}

    if (model == "logit"){
      logit <- tryCatch(speedglm(formula(cbind(y,x)), family=binomial(link='logit'), data=x, start=c(1:ncol(x)*0,0)),
                        error = function (e) {glm(formula(cbind(y,x)), family=binomial(link='logit'), data=x, start=c(1:ncol(x)*0,0))}
      )
      initial = coef(logit)
      initial[is.na(initial)==TRUE]<-0
      if(sum(abs(initial))>3|abs(initial[2])>3){initial=initial*0}
      if(memorymanagement == TRUE) {rm(logit)}
    }
    if (model == "probit"){
      probit <- tryCatch(speedglm(formula(cbind(y,x)), family=binomial(link='probit'), data=x, start=c(1:ncol(x)*0,0)),
                         error = function (e) {glm(formula(cbind(y,x)), family=binomial(link='probit'), data=x, start=c(1:ncol(x)*0,0))}
      )
      initial = coef(probit)
      initial[is.na(initial)==TRUE]<-0
      if(sum(abs(initial))>3|abs(initial[2])>3){initial=initial*0}
      if(memorymanagement == TRUE) {rm(probit)}
    }
    if (model == "logit"){
      logit2 <- tryCatch(speedglm(formula(cbind(y2,x2)), family=binomial(link='logit'), data=x2, start = initial),
                         error = function (e) {glm(formula(cbind(y2,x2)), family=binomial(link='logit'), data=x2, start = initial)}
      )
      startval2 = coef(logit2)
      if(memorymanagement == TRUE) {rm(logit2)}
    }
    if (model == "probit"){
      probit2 <- tryCatch(speedglm(formula(cbind(y2,x2)), family=binomial(link='probit'), data=x2, start = initial),
                          error = function (e) {glm(formula(cbind(y2,x2)), family=binomial(link='probit'), data=x2, start = initial)}
      )
      startval2 = coef(probit2)
      if(memorymanagement == TRUE) {rm(probit2)}
    }
    if (model == "gmm_nlminb") {
      pkgTest("gmm")
      buildgmmdata <- function (Y, X){
        return (data.matrix(cbind(Y, cbind(constant=1,X))))
      }

      moments <- function (theta, data){
        y <- as.numeric(data[,1])
        x <- data.matrix(data[,2:ncol(data)])
        m <- x * as.vector((y - logistic(theta, x)))
        return(cbind(m))
      }
      Z = cbind(y,x)
      init <- tryCatch(c(coef((speedlm(formula(Z), data = Z)))),
                       error = function (e) {c(coef((lm(formula(Z), data = Z))))}
      )
      init[is.na(init)==TRUE]<-0
      if(sum(abs(init))>3|abs(init[2])>3){initial=init*0}
      gmmlogit <- gmm(moments, x = buildgmmdata(y2, x2), t0 = init, type = "iterative", crit = 1e-10, wmatrix ="ident", optfct = "nlminb")
      #gmmlogit <- gmm(moments, x = buildgmmdata(y2, x2), t0 = init, type = "iterative", crit = 1e-10, wmatrix ="optimal", method = "BFGS")
      startval2 = coef(gmmlogit)
      if(memorymanagement == TRUE) {rm(gmmlogit)}
    }
    if (model == "gmm_bfgs") {
      pkgTest("gmm")
      buildgmmdata <- function (Y, X){
        return (data.matrix(cbind(Y, cbind(constant=1,X))))
      }

      moments <- function (theta, data){
        y <- as.numeric(data[,1])
        x <- data.matrix(data[,2:ncol(data)])
        m <- x * as.vector((y - logistic(theta, x)))
        return(cbind(m))
      }
      Z = cbind(y,x)
      init <- tryCatch(c(coef((speedlm(formula(Z), data = Z)))),
                       error = function (e) {c(coef((lm(formula(Z), data = Z))))}
      )
      init[is.na(init)==TRUE]<-0
      if(sum(abs(init))>3|abs(init[2])>3){initial=init*0}
      #gmmlogit <- gmm(moments, x = buildgmmdata(y2, x2), t0 = init, type = "iterative", crit = 1e-10, wmatrix ="optimal", optfct = "nlminb")
      gmmlogit <- gmm(moments, x = buildgmmdata(y2, x2), t0 = init, type = "iterative", crit = 1e-10, wmatrix ="ident", method = "BFGS")
      startval2 = coef(gmmlogit)
      if(memorymanagement == TRUE) {rm(gmmlogit)}
    }
    return(startval2)
  }

  guess<-guessStartVal1(Y, X, model, tracelevel, memorymanagement)
  guess[is.na(guess)]<-0
  if(TRUE %in% (abs(guess)>3) ){
    guess2<-guessStartVal1(Y, X, model, tracelevel, memorymanagement)
    guess2[is.na(guess2)]<-0
  } else{return(guess)}
  if(TRUE %in% (abs(guess2)>3) ){
    return(guess2*0)
  } else{return(guess2)}
}






#' A main function of the package for variable selection based on moidel type and a generalize to specific approach.
#'
#' This function is a wrapper to the functions bestlinearX(), bestlogitX() and bestprobitX(), with an additional option to call getSamples for improved speed. Take into account that sampling itself takes time, such that total computational burden is a trade-off between the load of the getSample function and the model optimization itself.
#' @param Y A binary response variable.
#' @param X A dataframe of multiple exogenous regressors.
#' @param model Either "lm" for the linear probability model, "logit" for the logistic probability model, or "probit", for the probit model. The logit and probit models are solved using Iterated Weighted Least Squares, and optimization of the logit model is significantly faster than the probit model. Defaults to "lm".
#' @param returntype Either "data" to return a dataset, or colnames" to only return the collumn names of the variables that are used in the optimal model. "data" by default.
#' @param method The optimization strategy. Either "opt.ic" to optimize using information criteria, "opt.t" for step-wise elimination of insignificant values
#' (statistically speaking not a sound procedure, but it will provide a parsimonious model that can be usefull as a benchmark), or "opt.h" to optimize by classical hypothesis tests.
#' defaults to "opt.ic".
#' @param KLIC the information criterion used by "opt.ic", either "AIC" or "AICc", defaults to the latter.
#' @param crit.t The t-value indicating significance when using method "opt.t", defaults to 1.64.
#' @param crit.p the p-value used by method "opt.h" in the hypothesis tests. Defaults to 0.05.
#' @param test The hypothesis test used by "opt.h". Defaults to "LR" for the Likelihood Ratio test. Other options are "F", for an F test for joint significance of insignificant parameters, or "Chisq" for a wald test against the Chi squared distribution.
#' Recommended setting is either "LR" as it is less dependent on correct estimation of the standard errors. Keep in mind that "Chisq" is an asymptotic test, anf "F" is more appropiate for small sample tests. Howver "Chisq" holds under milder conditions and should be used if no small sample theory is available for the model.
#' @param share between 0-1, specifying the amount of data that should be passed on to the optimization strategies. Defaults to 0.75, to improve speed. Uses getSamples() to maintain first and second moments of the data.
#' @param confidence.alternative passed on to getSample. Defaults to .85.
#' @param max.iter passed on to getSample. Defaults to 50.
#' @param tracelevel the amount of information to be printed. Passed on to underlying routines. Defaults to 1 for printing, set to 0 for no printing.
#' @param memorymanagement TRUE/FALSE indicating whether garbage collection should be forec regularly when memory usage is high. Defaults to TRUE, recommended setting for large datasets.
#' @return Either a dataframe of exogenous variables, or a vector containing the collumn names indicating the optimal variables extracted from the supplied dataset.
#' @export
#' @examples
#' # load data
#' data(ITdata)
#' data(corinetable)
#' #Grab a sample (optional).
#' sample <- ITdata[getSamples(ITdata, share =.05),]
#' # Reclassify
#' catITdata <- reclassify(sample, reclasstable = corinetable)
#' # create a binary response dataset.
#' Y <- MLtoBinomData(catITdata[,1], class =1)
#' X <- catITdata[,-1]
#' selectX(Y, X, model ="lm", returntype = "colnames", method = "opt.t")
#' bestX <- selectX(Y, X)
#' describe(bestX)


selectX <- function (Y, X, model="lm", returntype="data",  method ="opt.ic",
                     KLIC = "AICc", crit.t=1.64, crit.p=.05, test = "LR",
                     share = 0.75, confidence.alternative=0.85, max.iter =50,
                     tracelevel = 1, memorymanagement =TRUE){

  samples <- getSamples (data = cbind(Y,X), share, confidence.alternative, max.iter, tracelevel, memorymanagement)
  x = X[samples,]
  y = Y[samples]

  if (memorymanagement == TRUE){
    rm(samples)
    rm(Y)
    rm(X)
    tgc()
  }

  if (!(returntype %in% c("data", "colnames"))){
    warning("Unknow returntype, returning colnames instead.")
    returntype = "colnames"
  }
  if (!(method %in% c("opt.ic", "opt.t", "opt.h"))){
    warning("Method unknow. Supported: opt.ic, opt.h and opt.t. Using opt.ic.")
  }

  if (method == "opt.ic"){
    opt <- opt.ic(model, Y=y, X=x, KLIC, returntype, tracelevel, memorymanagement)
  }
  if (method == "opt.t"){
    opt <- opt.t(model, Y=y, X=x, returntype, tracelevel, crit.t, memorymanagement)
  }
  if (method == "opt.h"){
    opt <- opt.h(model, Y=y, X=x, returntype, method ="joint", tracelevel, crit.p, test, memorymanagement)
  }
  return(opt)
}


#' A main function of the package to apply generalize to specific to generalized linear models.
#'
#' This function is a wrapper to the functions opt.ic(), opt.t() and opt.h().
#' @param model Either "lm" for the linear probability model, "logit" for the logistic probability model, or "probit", for the probit model. The logit and probit models are solved using Iterated Weighted Least Squares, and optimization of the logit model is significantly faster than the probit model. Defaults to "lm".
#' @param Y A binary response variable.
#' @param X A dataframe of multiple exogenous regressors.
#' @param method The optimization strategy. Either "opt.ic" to optimize using information criteria, "opt.t" for step-wise elimination of insignificant values
#' (statistically speaking not a sound procedure, but it will provide a parsimonious model that can be usefull as a benchmark), or "opt.h" to optimize by classical hypothesis tests.
#' defaults to "opt.ic".
#' @param KLIC the information criterion used by "opt.ic", either "AIC" or "AICc", defaults to the latter.
#' @param crit.t The t-value indicating significance when using method "opt.t", defaults to 1.64.
#' @param crit.p the p-value used by method "opt.h" in the hypothesis tests. Defaults to 0.05.
#' @param test The hypothesis test used by "opt.h". Defaults to "LR" for the Likelihood Ratio test. Other options are "F", for an F test for joint significance of insignificant parameters, or "Chisq" for a wald test against the Chi squared distribution.
#' Recommended setting is either "LR" as it is less dependent on correct estimation of the standard errors. Keep in mind that "Chisq" is an asymptotic test, anf "F" is more appropiate for small sample tests. Howver "Chisq" holds under milder conditions and should be used if no small sample theory is available for the model.
#' @param share between 0-1, specifying the amount of data that should be passed on to the optimization strategies. Defaults to 0.75, to improve speed. Uses getSamples() to maintain first and second moments of the data.
#' @param tracelevel the amount of information to be printed. Passed on to underlying routines. Defaults to 1 for printing, set to 0 for no printing.
#' @param memorymanagement TRUE/FALSE indicating whether garbage collection should be forec regularly when memory usage is high. Defaults to TRUE, recommended setting for large datasets.
#' @return Either a dataframe of exogenous variables, or a vector containing the collumn names indicating the optimal variables extracted from the supplied dataset.
#' @export
#' @examples
#' randomlogit <- simulateLogit(nobs=8000, pars = c(0.5, -0.4, -0.3, 0.1, 0.05, 0.025, 0.01,
#'                                                  0.005, 0.005, 0.005, 0.005, 0.005, 0.005,
#'                                                  0.0025, 0.0025, 0.0025, 0.0025, 0.0, 0.0, 0.0))
#' # add multicollinear vector, to see how the method responds to faulty variables.
#' randomlogit<-cbind(randomlogit,mcv = randomlogit[,2])
#'
#' Y=randomlogit[,1]
#' X=randomlogit[,-1]
#'
#' logit_ic <- generalizeToSpecific(model ="logit", Y, X)
#'
#' logit_t <- generalizeToSpecific(model ="logit", Y, X, "opt.t")
#'
#' logit_h <- generalizeToSpecific(model ="logit", Y, X, "opt.h")
#'
#' probit_ic <- generalizeToSpecific(model ="probit", Y, X)
#' linear_ic <- generalizeToSpecific(model ="probit", Y, X)


generalizeToSpecific <- function (model="lm", Y, X, method = "opt.ic", KLIC="AICc",
                                  crit.t=1.64, crit.p=.1, test = "LR",
                                  tracelevel = 1, memorymanagement =TRUE){

  if (!(method %in% c("opt.ic", "opt.t", "opt.h"))){
    warning("method unknow. supported: opt.ic, opt.h and opt.t. Using op.ic instead.")
    method = "opt.ic"
  }
  if (method == "opt.ic"){
    opt <- opt.ic(model, Y, X, KLIC, returntype= "model", tracelevel, memorymanagement)
  }
  if (method == "opt.t"){
    opt <- opt.t(model, Y, X, returntype= "model", tracelevel, crit.t, memorymanagement)
  }
  if (method == "opt.h"){
    opt <- opt.h(model, Y, X, returntype= "model", method ="joint", tracelevel=tracelevel, crit.p=crit.p, test=test, memorymanagement=memorymanagement)
  }
  return(opt)
}



#' A main function of the package for automated generalized linear model fitting. Includes additional options w.r.t. the \code{\link{generalizeToSpecific}} command.
#'
#' This function is a wrapper around the optimization and selection routines in the package and can be used for automated calibration of GLM's on semi large datasets.
#' \code{\link{generalizeToSpecific}} is more appropiate for manual R sessions, \code{\link{autoGLM}} is more appropiate for situations when calibration takes a long time.
#' E.g., it allows to run \code{\link{generalizeToSpecific}} over a vector of dependent variable classes, and to log and write outputs to disk.
#'
#' @param data A dataframe with a categorical response variable in the first column, and covariates in subsequent columns. Typically the product of cbind(Y,X).
#' @param reclasstable A table that maps the first column of data into a binary response variable. By default it will be ommitted (the binary response variable will be identical to data[,1]). See also \code{\link{corinetable}}, See also \code{\link{reclassify}}.
#' @param class The class that should be 1 in the binary response variable, all other classes in the categorical variable will be set to 0. Defaults to 1. See also \code{\link{reclassify}}.
#' @param outputpath The location on the hard drive where output wwill be written to. Defaults to getwd().
#' @param modelname The name of the model, will be used when writing a weightsfile. Defaults to "autoGLM". See also \code{\link{exportWeightsfile}}.
#' @param tracelevel The amount of information to be printed. Passed on to underlying routines. Defaults to 1 for printing, set to 0 for no printing.
#' @param actions Actions to be taken by autoGLM, by default c("print", "return"), may include any combination of c("write", "print", "log", "return"), for writing a geoDMS weightsfile, See also \code{\link{exportWeightsfile}}, printing results, writing a log file, and returning results as a list object.
#' @param NAval Optional categorical variable that should be dropped by the reclassification scheme. See also \code{\link{reclassify}}.
#' @param model Main model type that should be calibrated, either "lm", "probit", or "logit". See also \code{\link{generalizeToSpecific}}.
#' @param preselect Optional variable preselection using a first order approximation (linear model) of the logit or probit model, by specifying "lm" (default setting). See also \code{\link{selectX}}.
#' @param method The optimization strategy. Either "opt.ic" to optimize using information criteria, "opt.t" for step-wise elimination of insignificant values
#' (statistically speaking not a sound procedure, but it will provide a parsimonious model that can be usefull as a benchmark), or "opt.h" to optimize by classical hypothesis tests.
#' defaults to "opt.ic". See also See also \code{\link{opt.ic}}, \code{\link{opt.t}}, See also \code{\link{opt.h}}.
#' @param crit.t The t-value indicating significance when using method "opt.t", defaults to 1.64. \code{\link{opt.t}}.
#' @param crit.p the p-value used by method "opt.h" in the hypothesis tests. Defaults to 0.05. \code{\link{opt.h}}.
#' @param test The hypothesis test used by "opt.h". Defaults to "LR" for the Likelihood Ratio test. Other options are "F", for an F test for joint significance of insignificant parameters, or "Chisq" for a wald test against the Chi squared distribution. \code{\link{opt.h}}.
#' @param KLIC The information criterion used by "opt.ic", either "AIC" or "AICc", defaults to the latter. \code{\link{opt.ic}}.
#' @param accuracytolerance When aut of sample and within sample accuracy differ more than accuracytolerance, a warning will be issued, which is also logged when specifying "log" in actions. Defaults to 0.01. \code{\link{accuracy}}.
#' @param confidence.alternative See also \code{\link{getSamples}}, confidence level used for the alternative of dissimilar samples in the sampling routine. Defaults to .85.
#' @param use.share Share of the data used, See also \code{\link{getSamples}}. Defaults to .25.
#' @param maxsampleruns See also \code{\link{getSamples}}, defaults to 50.
#' @param memorymanagement TRUE/FALSE indicating whether garbage collection should be forced regularly when memory usage is high. Defaults to TRUE, recommended setting for large datasets. See also \code{\link{tgc}}.
#' @param returnall TRUE, FALSE, or "writedisk" indicating whether all the outputted objects for each class should be returned in an array as produced by lapply, or whether only the final output should be returnd as an object.
#' Specifying "writedisk" will write the objects containing results of each class as seperate .RDS files, which you can use to restore the output using readRDS(). \code{\link{iapply}}.
#' Returning an array of all results can consume large amounts of memory as each object contains copies of the used datasets. When working with countrysize datasets, these array objects can easily require over 64gb of RAM.
#' Specifying returnall = FALSE (default setting), is much more more RAM friendly as it stores results for each class in the same memory adress, overwriting previous results. Seting returnall = FALSE, will still write log files and print diagnostics to screen if specified in actions.
#' returnall="writedisk" is the recommended setting, but it is not default. \code{\link{iapply}}.
#' @param compress, passed on to iapply. Defaults to no compression of RDS output, which is the recommended setting if computation time is valued of disk space. Keep in mind that when using large datasets, autoGLM objects can be several gigabytes in size. \code{\link{iapply}}.
#' @param JIT, logical indicating whether just-in-time compilation of internal functions should be used. Mainly for historical reasons.
#' @export
#' @examples
#'
#' data(ITdata)
#' datacorinetable)
#'
#' results <- autoGLM(data=randomlogit, reclasstable=corinetable, class=0, method ="opt.ic")
#'
#'
#' # All options:
#' autoGLM <- function (data, reclasstable = "default", class=1, outputpath=paste(getwd(),"//", sep=""),
#'  modelname="autoGLM", tracelevel=1,
#'  actions = c("print", "return"), NAval = -9999,
#'  model="logit", preselect = "lm", method = "opt.ic", crit.t = 1.64, crit.p =.05,
#'  test = "LR", KLIC = "AICc", accuracytolerance =0.01, confidence.alternative =0.90,
#'  use.share = 0.25, maxsampleruns=50, memorymanagement = TRUE, returnall = FALSE,
#'  compress = FALSE, JIT = TRUE)
#'

autoGLM <- function (data, reclasstable = "default", class=1, outputpath=paste(getwd(),"//", sep=""), modelname="autoGLM",
                     tracelevel=1, actions = c("print", "return"), NAval = -9999,
                     model="logit", preselect = "lm", method = "opt.ic", crit.t = 1.64, crit.p =.05,
                     test = "LR", KLIC = "AICc", accuracytolerance =0.01, confidence.alternative =0.90,
                     use.share = 0.25, maxsampleruns=50, memorymanagement = TRUE, returnall = FALSE,
                     compress = FALSE, JIT = TRUE) {
  if (JIT == TRUE){
    pkgTest("compiler", silent = TRUE)
    enableJIT(3)
  }

  f <- function(x) {tryCatch(opt.glm (data, reclasstable, class=x,
                                      outputpath, modelname, tracelevel, actions, NAval,
                                      model, preselect, method, crit.t, crit.p, test, KLIC, accuracytolerance, confidence.alternative,
                                      use.share, maxsampleruns, memorymanagement),
                             error = function (e) {warning(paste(paste("Calibration for class", as.character(x)), "failed"))}
  )
  }

  if (returnall == TRUE){
    multi <- lapply(X=class, f)
  } else {
    if(returnall == "writedisk"){
      writedisk = TRUE
      outdir = outputpath
    } else {
      writedisk = FALSE
      outdir = "default"
    }
    name = getCall2()
    multiresults <- iapply(X=class, f, writedisk, outdir, name, compress)
  }
  if (JIT == TRUE){
    enableJIT(0)
  }
  return(multiresults)
}


#' Returns the name of a call as string.
#'
#' When called inside a function, returns the parent function's name.
#' @param level 0 returns "getCall2", -1 returns the name of the function in which getCall2(-1) is called etc.
#' @return string
#' @export
#' @examples
#' getCall2(0)
#'
#' foo <- function(){
#'	return(getCall2())
#' }
#' foo()
#'
#' bar <- function (somevar){
#'	foo <- function(){
#'		return(getCall2(-2))
#'	}
#'	return(foo())
#'}


getCall2 <- function (level=-1){
	fname <-deparse(sys.calls()[[sys.nframe()+level]])

	chars <- strsplit(fname, split='(|)')[[1]]
	chars=cbind(chars,1:length(chars))
	chars[1:(as.numeric(chars[chars[,1]=="("][2])-1),1]
	paste0(chars[1:(as.numeric(chars[chars[,1]=="("][2])-1),1], collapse = "")
}



#' lapply without memory builtup. Iterative application of a function over a vector of arguments, returning only the last result as an object or writing all output to disk.
#'
#' Similar to well-know lapply, but returns only the last result as an object. Very usefull when working with big datasets. If the object to be returned is large in memory, say it contains copies of a large dataset,
#' iapply iteratively applies a function but returns only the last output of the function application. This is useful if the function prints diagnostics to screen or saves results in a log file,
#' but you would like to evaluate the last object to understand the output better. Additionally, the function allows you to write all the output to disk using a Serialization Interface for Single Objects.
#' This allows you to restore any output to an object, possibly with a different name.
#' @param X a vector of input variables similar to lapply.
#' @param FUN a function to be applied iteratively over the input arguments. similar to FUN in lapply.
#' @param writedisk TRUE/FALSE indicating whether output of application of FUN to elements of X should be written to disk as serialized representions in RDS files before it gets overwritten in memory. Will also write the final output to disk.
#' @param name When writedisk = TRUE, output is saved with this name followed by the iteration. By default, uses getCall2(), thus RDS files are "iapply" followed by the number of th element. However storing iapply in another object, will change the output name.
#' @param compress a logical specifying whether outputted RDS files use "gzip" compress, or one of "gzip", "bzip2" or "xz" to indicate the type of compress to be used. Defaults to no compress.
#' You can later restore the objects using readRDS().
#' @param outdir output directory where .RDS files should be written.
#' @return the output of FUN(X[X[length(X)]], plus any prints, messages, warnings, errors that lapply(X, FUN) would produce. If FUN writes results to a disk, these files will be created too.
#' @export
#' @examples
#' f <- function(x){
#'	print (x)
#'	return(x)
#' }
#' x=1:10
#' # this will print all elements of x, finaloutput will only contain the last element of x.
#' finaloutput <- iapply (x, f)
#' print(finaloutput)
#'
#' # this will print all elements of x, create 10 RDS files that you can use to restore f(x),
#' # finaloutput will contain the last element of x.
#' finaloutput <- iapply (x, f, writedisk = TRUE, outdir = "C:\\Users\\")
#' print(finaloutput)
#'
#' # this will do the same, but stores names as "binomials1", "binomials2" etc.
#' binomials <- iapply
#' finaloutput <- binomials (x, f, writedisk = TRUE, outdir = "C:\\Users\\")
#' print(finaloutput)

iapply <- function (X, FUN, writedisk = FALSE, outdir = "default", name = "default", compress = FALSE) {
  if(name == "default"){
    name="iapply"#getCall2()
  }
  for (i in 1:length(X)){
    if(i>1){rm(last)}
    last <-FUN(X[i])
    if (writedisk == TRUE){
      if (outdir == "default"){
        saveRDS(last, file = paste(as.character(last,".RDS", sep="")), compress=compress)
      } else {
        saveRDS(last, file =paste(paste(paste(outdir,name,sep=""),as.character(i),sep=""),".RDS", sep =""), compress=compress)
      }
    }
    tgc()
  }
  return(last)
}


#' Optimization routine based on hypothesis testing.
#'
#' This function uses hypothesis tests to optimize a glm. If method is "joint", a joint significance tests of the full model against the model with only paramaeters that are significant in the full model is performed. If the insignificant variables are jointly significant, the alternative specification is returned.
#' If the insignificant variables are jointly insignificant, the parameter with the largest p-value is dropped, and the test is repeated.
#' If method is "single", the hypothesis tests are made between the full model and a model with one variable less. The tests are repeated till removing variables is not supported by the specified test.
#' Single wald tests are equal to t-tests, F-tests are finite sample tests. Note that optimizing by single parameter tests is restrictive, for a discussion look up bonferonni corrections. Only the joint tests are available through autoGLM, for single tests opt.t is available.
#' Though both methods use hypothesis testing a a decision criterion, the returned models may differ. This is a common result of hypothesis tests, and is one argument why
#' model selection may be based on information criteria. for a discussion see for example "Comments on testing economic theories and the use of model selection criteria" by Granger, King and White (1995).
#' @param model The model to be optimized. Supports "lm" for the linear probability model, "logit" for the logistic probability model, and "probit" for the probit model.
#' @param Y The binary response variable.
#' @param X A dataframe with collumns of exogenous regressors.
#' @param returntype "model", "data", or "colnames"
#' @param tracelevel level of printing.
#' @param crit.p p value used in hypothesis tests.
#' @param test type of test, either "LR", "F", or "Chisq".
#' @param memorymanagement logical, indicating whether memory should be more actively managed.
#' @return "model", "data", or "colnames", to be specified in returnype.
#' @export
#' @examples
#'
#' pars = c(0.5, -0.4, -0.3, 0.1, 0.05, 0.025, 0, 0, 0, 0,0,0,0)
#' randomlogit <- simulateLogit(nobs=8000, pars = pars)
#' Y=randomlogit[,1]
#' X=randomlogit[,-1]
#' hmod <- opt.h(model="logit", Y, X, returntype="model", tracelevel=1, crit.p=0.05, test="LR")




opt.h <- function (model, Y, X, returntype="model", tracelevel=0, crit.p=0.1, test="LR", method="joint", memorymanagement=FALSE){
  pkgTest("lmtest")
  oftype <- function (bestfit){
    return((if("glm" %in% as.character(bestfit$call)){
      if(bestfit$family$link=="logit"){"logit"}else{"probit"}
    } else {"lm"}))
  }
  warmstart<-function(oldstart, warmth=1){
    oldstart[is.na(oldstart)==TRUE]<-0
    return(oldstart*warmth)
  }
  fitnew <- function (newspec, model, newZ, newstart){
    convert.speed <- function (bestfit, model, data){
      if(is.speed(bestfit)){
        initial<-warmstart(coef(bestfit))
        bestfit=suppressWarnings(
          glm(newspec, family=binomial(link=model), data=data, start = initial)
        )
      }
      return(bestfit)
    }
    newfit=suppressWarnings(
      if(model %in% c("logit", "probit")){
        tryCatch(speedglm(newspec, family=binomial(link=model), data=newZ, start = newstart),
                 error = function (e) {glm(newspec, family=binomial(link=model), data=newZ, start = newstart)}
        )
      } else if(model=="lm"){
        lm(newspec, data=newZ)
      }
    )
    newfit <- convert.speed(bestfit=newfit, model, data=newZ)
    return(newfit)
  }
  update.X <- function (bestfit, newX){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newX = newX[,-mintID]
    return(newX)
  }
  update.Z <- function (Y, newX){
    newZ = data.frame(cbind(Y,newX))
    return(newZ)
  }
  update.start <- function (bestfit){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newstart = warmstart(coef(bestfit)[-(mintID+1)])
    return(newstart)
  }
  drop.na.model <- function (namodel){
    type = oftype(namodel)
    varnums = coef(namodel)[-1]*0+1
    nums= 1:length(varnums)
    ok = nums*varnums
    ok = ok[complete.cases(ok)]
    mintID = setdiff(nums, ok)
    #newX= namodel$model[,-1][,-mintID]
    #newX = newX

    newZ = data.frame(cbind(namodel$model[,1],namodel$model[,-1][,-mintID]))
    colnames(newZ)[1]<-"Y"
    newspec = formula(newZ)

    newstart<-warmstart(coef(namodel)[-(mintID+1)])

    newfit= fitnew (newspec, model=type, newZ, newstart)
    return(newfit)
  }
  drops<-function(bestfit){
    ts = abs(coef(bestfit) / sqrt(diag(vcov(bestfit))))
    tsid <-cbind(ts, 1:length(ts))
    tsid[tsid[,1]>1.64]<-0
    tsid[1,1]<-0
    tsid<-tsid[rev(order(tsid[,1])),]
    drops<-(tsid[,2][tsid[,2]>1])
    return(drops)
  }
  update.iter<-function(iter=0){
    return(iter+1)
  }
  print.iter <- function (tracelevel,iter){
    if(tracelevel>0){
      message(paste("opt.h iteration:",as.character(iter)),"\r",appendLF=FALSE)
      flush.console()
    }
  }
  if(method=="joint"){
    opt.iter = update.iter()
    print.iter(tracelevel,opt.iter)

    if(is.null(model)){model = "lm"}
    #newX=data.frame(X)
    Xnames <- colnames(X)
    if (length(Xnames) == 0){
      #colnames(X) <- c(paste("x",as.character(1:ncol(X)),sep=""))
      Xnames <- c(paste("x",as.character(1:ncol(X)),sep=""))
    }
    #newZ = update.Z(Y,newX)
    newZ = update.Z(Y,X)
    spec = formula(newZ)
    newX=X
    #if (model != "lm"){
    #  newstart = suppressWarnings(
    #    if(model == "logit") {
    #      suppressWarnings(guessStartVal(Y=Y, X=X, model="logit"))
    #    } else if(model == "probit"){
    #      suppressWarnings(guessStartVal(Y=Y, X=X, model="probit"))
    #    }
    #  )
    #  newstart<-warmstart(newstart)
    #}
    newstart =c(rep(0,length.out=ncol(newZ)))

    if (memorymanagement == TRUE){
      rm(Y)
      rm(X)
      tgc()
    }

    fullfit= fitnew(newspec=spec, model, newZ=newZ, newstart=newstart)
    bestfit = fullfit
    newfit=NA
    Y= fullfit$model[,1]
    mtype=oftype(fullfit)

    opt.iter<-update.iter(opt.iter)

    if(length(coef(fullfit)[complete.cases(coef(fullfit))==FALSE])>0){dropstatus=TRUE}else{
      ts <- drops(fullfit) # ordered from most significant to least significant
      if(sum(ts)>0){dropstatus=TRUE}else{dropstatus=FALSE}
    }

    #newstart=coef(fullfit)[-ts]
    while (dropstatus){
      if (memorymanagement == TRUE){
        tgc()
      }
      print.iter(tracelevel,opt.iter)

      if (length(coef(fullfit)[complete.cases(coef(fullfit))==FALSE])>0) {

        fullfit = drop.na.model(namodel=fullfit)
        Y=fullfit$model[,1]
        X=fullfit$model[,-1]
        fullfit<-fitnew(formula(cbind(Y,X)), model=mtype, newZ=cbind(Y,X), newstart =coef(fullfit))

        ts <- drops(fullfit)
        if(sum(ts)>0){dropstatus=TRUE}else{dropstatus=FALSE; bestfit = fullfit}

        if(dropstatus){
          newX <- fullfit$model[,-1]
          newZ <- update.Z(Y, newX)

          newts = drops(fullfit)
          ts=newts
          newstart=coef(fullfit)[-ts]
        }
      } else {

        # old: joint test on insignificant variables. if jointly significant, increase null model with 1 par till all drops are jointly insignificant.
        # new: change alternative mode to one less till all are jointly significant or dropped

        dropfromfull <- rev(drops(fullfit))[1]
        newZ = fullfit$model[,-c(dropfromfull)]
        colnames(newZ)[1]<-"Y"

        newstart=rep(0,length.out=dim(newZ)[2])
        fullfit <- fitnew(formula(newZ), model=mtype, newZ=newZ, newstart = newstart)

        dropnull <- drops(fullfit)

        if(sum(dropnull)>0){
          newZ = fullfit$model[,-c(dropnull)]
          colnames(newZ)[1]<-"Y"
          newstart=rep(0,length.out=dim(newZ)[2])
          newfit <- fitnew(formula(newZ), model=mtype, newZ=newZ, newstart = newstart*0)

          #nullmodel = newfit
          #alternativemodel = fullfit

          if (test %in% c("Chisq", "F")){

            p <-waldtest(fullfit, newfit, test = test)$Pr[2]

          } else if (test == "LR"){

            p <- lrtest(fullfit, newfit)$Pr[2]
            #LR <- 2*(logLik(fullfit)-logLik(newfit))
            #p <- 1- pchisq(LR, df=(length(coef(fullfit))-length(coef(newfit))), ncp = 0, lower.tail = TRUE, log.p = FALSE)
          }
        } else {p=0}

        if(p<crit.p){dropstatus<-FALSE; bestfit=fullfit} else {if( dim(fullfit$model)[2]==dim(newfit$model)[2] ){dropstatus<-FALSE}}
        #{ts=ts[-1]; newstart = c(coef(newfit),0); if( dim(fullfit$model)[2]==dim(newfit$model)[2] ){dropstatus<-FALSE}} #

      }

      opt.iter<-update.iter(opt.iter)
    }

    if (tracelevel > 0){
      message(paste("total opt.h iterations:", as.character(opt.iter)))
    }
    if (returntype == "model"){
      return(bestfit)
    }
    if (returntype == "data"){
      return(bestfit$model[,-1])
    }
    if (returntype == "colnames"){
      return(colnames(bestfit$model[,-1]))
    }
  } else if(method=="single"){
    droptrue <- function (bestfit, crit.p){
      if(length(coef(bestfit)[complete.cases(coef(bestfit))==FALSE])>0){
        return(TRUE)
      } else {

        ts = abs(coef(bestfit) / sqrt(diag(vcov(bestfit))))
        ts[ts>1.64]<-0
        ts[1]<-0


        if(sum(ts)>0){
          fullmodel <- bestfit

          ns<-1:length(ts)
          dropts<-cbind(ns,ts)[,1][cbind(ns,ts)[,2]==min(ts[ts>0])]

          cX = bestfit$model[,-c(1,dropts)]
          Y= bestfit$model[,1]

          cfit <- fitnew(formula(cbind(Y,cX)), model=oftype(bestfit), newZ=cbind(Y,cX), newstart = coef(bestfit)[-dropts])

          if (test %in% c("Chisq", "F")){
            p <-waldtest(fullmodel, cfit, test = test)$Pr[2]
          } else if (test == "LR"){

            p <- lrtest(fullmodel, cfit)$Pr[2]
            #LR <- lrtest(fullmodel, cfit)$Pr[2]
            #p <- 1-pchisq(LR, df=(length(coef(bestfit))-length(coef(cfit))), ncp = 0, lower.tail = TRUE, log.p = FALSE)
          }
        } else {p= 0}
        return(if (p>crit.p){TRUE}else{FALSE})

      }
    }
    opt.iter = update.iter()
    print.iter(tracelevel,opt.iter)

    if(is.null(model)){model = "lm"}
    #newX=data.frame(X)
    Xnames <- colnames(X)
    if (length(Xnames) == 0){
      #colnames(X) <- c(paste("x",as.character(1:ncol(X)),sep=""))
      Xnames <- c(paste("x",as.character(1:ncol(X)),sep=""))
    }
    #newZ = update.Z(Y,newX)
    newZ = update.Z(Y,X)
    spec = formula(newZ)

    if (model != "lm"){
      newstart = suppressWarnings(
        if(model == "logit") {
          suppressWarnings(guessStartVal(Y=Y, X=X, model="logit"))
        } else if(model == "probit"){
          suppressWarnings(guessStartVal(Y=Y, X=X, model="probit"))
        }
      )
      newstart<-warmstart(newstart)
    }

    bestfit= fitnew(newspec=spec, model, newZ=newZ, newstart=newstart)

    drop=droptrue(bestfit,crit.p)

    opt.iter<-update.iter(opt.iter)

    newX=X
    if (memorymanagement == TRUE){
      rm(Y)
      rm(X)
      tgc()
    }

    while (drop==TRUE){
      if (memorymanagement == TRUE){
        tgc()
      }
      print.iter(tracelevel,opt.iter)

      if (length(coef(bestfit)[complete.cases(coef(bestfit))==FALSE])>0) {

        bestfit = drop.na.model(namodel=bestfit)

        drop=droptrue(bestfit,crit.p)
        if(drop){
          newX <- bestfit$model[,-1]
          newZ <- update.Z(bestfit$model[,1], newX)
        }
      } else {

        newX <- update.X(bestfit, newX)
        newZ <- update.Z(bestfit$model[,1], newX)

        newspec = formula(newZ)

        newstart = update.start(bestfit)

        bestfit = fitnew (newspec, model, newZ, newstart)

        drop=droptrue(bestfit,crit.p)

      }
      opt.iter<-update.iter(opt.iter)
    }
    if (tracelevel > 0){
      message(paste("total opt.h iterations:", as.character(opt.iter)))
    }
    if (returntype == "model"){
      return(bestfit)
    }
    if (returntype == "data"){
      return(bestfit$model[,-1])
    }
    if (returntype == "colnames"){
      return(colnames(bestfit$model[,-1]))
    }
  }
}








#' Optimization routine based on step-wise elimenation using t-values.
#'
#' This function searches
#' @param model The model to be optimized. Supports "lm" for the linear probability model, "logit" for the logistic probability model, and "probit" for the probit model.
#' @param Y The binary response variable.
#' @param X A dataframe with collumns of exogenous regressors.
#' @param returntype "model", "data", or "colnames"
#' @param tracelevel level of printing.
#' @param crit.t t-value used for significance test.
#' @param memorymanagement logical, indicating whether memory should be more actively managed.
#' @return "model", "data", or "colnames", to be specified in returnype.
#' @export
#' @examples
#' randomlogit <- simulateLogit(nobs=500, pars = c(0.5, -0.4, -0.3, 0.1, 0.05, 0.025, 0, 0, 0, 0))
#' Y=randomlogit[,1]
#' X=randomlogit[,-1]
#' opt.t(model="lm", Y, X)



opt.t <- function (model, Y, X, returntype="model", tracelevel=0, crit.t=1.64, memorymanagement=FALSE){
  oftype <- function (bestfit){
    return((if("glm" %in% as.character(bestfit$call)){
      if(bestfit$family$link=="logit"){"logit"}else{"probit"}
    } else {"lm"}))
  }
  warmstart<-function(oldstart, warmth=1){
    oldstart[is.na(oldstart)==TRUE]<-0
    return(oldstart*warmth)
  }
  fitnew <- function (newspec, model, newZ, newstart){
    convert.speed <- function (bestfit, model, data){
      if(is.speed(bestfit)){
        initial<-warmstart(coef(bestfit))
        bestfit=suppressWarnings(
          glm(newspec, family=binomial(link=model), data=data, start = initial)
        )
      }
      return(bestfit)
    }
    newfit=suppressWarnings(
      if(model %in% c("logit", "probit")){
        tryCatch(speedglm(newspec, family=binomial(link=model), data=newZ, start = newstart),
                 error = function (e) {glm(newspec, family=binomial(link=model), data=newZ, start = newstart)}
        )
      } else if(model=="lm"){
        lm(newspec, data=newZ)
      }
    )
    newfit <- convert.speed(bestfit=newfit, model, data=newZ)
    return(newfit)
  }
  update.X <- function (bestfit, newX){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newX = newX[,-mintID]
    return(newX)
  }
  update.Z <- function (Y, newX){
    newZ = data.frame(cbind(Y,newX))
    return(newZ)
  }
  update.start <- function (bestfit){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newstart = warmstart(coef(bestfit)[-(mintID+1)])
    return(newstart)
  }
  drop.na.model <- function (namodel){
    type = oftype(namodel)
    varnums = coef(namodel)[-1]*0+1
    nums= 1:length(varnums)
    ok = nums*varnums
    ok = ok[complete.cases(ok)]
    mintID = setdiff(nums, ok)
    #newX= namodel$model[,-1][,-mintID]
    #newX = newX

    newZ = data.frame(cbind(namodel$model[,1],namodel$model[,-1][,-mintID]))
    newspec = formula(newZ)

    newstart<-warmstart(coef(namodel)[-(mintID+1)])

    newfit= fitnew (newspec, model=type, newZ, newstart)
    return(newfit)
  }
  	droptrue <- function (bestfit, crit.t){
		if(length(coef(bestfit)[complete.cases(coef(bestfit))==FALSE])>0){
			return(TRUE)
			} else {
			ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
			ts= ts[-1]
			return(if (min(abs(ts))<crit.t){TRUE}else{FALSE})
		}
	}
	update.iter<-function(iter=0){
		return(iter+1)
	}

	print.iter <- function (tracelevel,iter){
		if (tracelevel>0){
			message(paste("opt.t iteration:",as.character(iter)),"\r",appendLF=FALSE)
			flush.console()
		}
	}
	opt.iter = update.iter()
	print.iter(tracelevel,opt.iter)

	if(is.null(model)){model = "lm"}
	#newX=data.frame(X)
	Xnames <- colnames(X)
	if (length(Xnames) == 0){
		#colnames(X) <- c(paste("x",as.character(1:ncol(X)),sep=""))
		Xnames <- c(paste("x",as.character(1:ncol(X)),sep=""))
	}
	#newZ = update.Z(Y,newX)
	newZ = update.Z(Y,X)
	spec = formula(newZ)

	if (model != "lm"){
		newstart = suppressWarnings(
									if(model == "logit") {
										suppressWarnings(guessStartVal(Y=Y, X=X, model="logit"))
									} else if(model == "probit"){
										suppressWarnings(guessStartVal(Y=Y, X=X, model="probit"))
									}
								)
		newstart<-warmstart(newstart)
	}

	bestfit= fitnew(newspec=spec, model, newZ=newZ, newstart=newstart)

	drop=droptrue(bestfit,crit.t)

	opt.iter<-update.iter(opt.iter)

	newX=X
	if (memorymanagement == TRUE){
		rm(Y)
		rm(X)
		tgc()
	}

	while (drop==TRUE){
		if (memorymanagement == TRUE){
			tgc()
		}
		print.iter(tracelevel,opt.iter)

		if (length(coef(bestfit)[complete.cases(coef(bestfit))==FALSE])>0) {

			bestfit = drop.na.model(namodel=bestfit)

			drop=droptrue(bestfit,crit.t)
			if(drop){
				newX <- bestfit$model[,-1]
				newZ <- update.Z(bestfit$model[,1], newX)
			}
		} else {

			newX <- update.X(bestfit, newX)
			newZ <- update.Z(bestfit$model[,1], newX)

			newspec = formula(newZ)

			newstart = update.start(bestfit)

			bestfit = fitnew (newspec, model, newZ, newstart)

			drop=droptrue(bestfit,crit.t)

		}
		opt.iter<-update.iter(opt.iter)
	}
	if (tracelevel > 0){
		message(paste("total opt.t iterations:", as.character(opt.iter)))
	}
	if (returntype == "model"){
		return(bestfit)
	}
	if (returntype == "data"){
		return(bestfit$model[,-1])
	}
	if (returntype == "colnames"){
		return(colnames(bestfit$model[,-1]))
	}
}



#' Optimization routine based on information criteria.
#'
#' This function optimizes a model using information critera as a decision rule. This solves many of the problems related to model selection based on hypothesis tests, see also \code{\link{opt.h}}.
#' In finite samples, the AIC is known to favor large models. The corrected AIC is a slightly stricter measure.
#' @param model The model to be optimized. Supports "lm" for the linear probability model, "logit" for the logistic probability model, and "probit" for the probit model.
#' @param Y The binary response variable.
#' @param X A dataframe with collumns of exogenous regressors.
#' @param KLIC information criterion to be used, "AIC" or "AICc", See also \code{\link{IC}}
#' @param returntype "model", "data", or "colnames"
#' @param tracelevel level of printing.
#' @param memorymanagement logical, indicating whether memory should be more actively managed.
#' @return "model", "data", or "colnames", to be specified in returnype.
#' @export
#' @examples
#' randomlogit <- simulateLogit(nobs=500, pars = c(0.5, -0.4, -0.3, 0.1, 0.05, 0.025, 0, 0, 0, 0))
#' Y=randomlogit[,1]
#' X=randomlogit[,-1]
#' opt.ic(model="lm", Y, X)


opt.ic <- function (model, Y, X, KLIC="AICc", returntype="model", tracelevel=0, memorymanagement=FALSE){
  oftype <- function (bestfit){
    return((if("glm" %in% as.character(bestfit$call)){
      if(bestfit$family$link=="logit"){"logit"}else{"probit"}
    } else {"lm"}))
  }
  warmstart<-function(oldstart, warmth=1){
    oldstart[is.na(oldstart)==TRUE]<-0
    return(oldstart*warmth)
  }
  fitnew <- function (newspec, model, newZ, newstart){
    convert.speed <- function (bestfit, model, data){
      if(is.speed(bestfit)){
        initial<-warmstart(coef(bestfit))
        bestfit=suppressWarnings(
          glm(newspec, family=binomial(link=model), data=data, start = initial)
        )
      }
      return(bestfit)
    }
    newfit=suppressWarnings(
      if(model %in% c("logit", "probit")){
        tryCatch(speedglm(newspec, family=binomial(link=model), data=newZ, start = newstart),
                 error = function (e) {glm(newspec, family=binomial(link=model), data=newZ, start = newstart)}
        )
      } else if(model=="lm"){
        lm(newspec, data=newZ)
      }
    )
    newfit <- convert.speed(bestfit=newfit, model, data=newZ)
    return(newfit)
  }
  update.X <- function (bestfit, newX){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newX = newX[,-mintID]
    return(newX)
  }
  update.Z <- function (Y, newX){
    newZ = data.frame(cbind(Y,newX))
    return(newZ)
  }
  update.start <- function (bestfit){
    ts = coef(bestfit) / sqrt(diag(vcov(bestfit)))
    ts= ts[-1]
    ID = 1:length(ts)

    results = cbind(ts,ID)

    mint = results[abs(results[,1])==min(abs(results[,1]))][1]
    mintID = results[abs(results[,1])==min(abs(results[,1]))][2]

    newstart = warmstart(coef(bestfit)[-(mintID+1)])
    return(newstart)
  }
  drop.na.model <- function (namodel){
    type = oftype(namodel)
    varnums = coef(namodel)[-1]*0+1
    nums= 1:length(varnums)
    ok = nums*varnums
    ok = ok[complete.cases(ok)]
    mintID = setdiff(nums, ok)
    #newX= namodel$model[,-1][,-mintID]
    #newX = newX

    newZ = data.frame(cbind(namodel$model[,1],namodel$model[,-1][,-mintID]))
    newspec = formula(newZ)

    newstart<-warmstart(coef(namodel)[-(mintID+1)])

    newfit= fitnew (newspec, model=type, newZ, newstart)
    return(newfit)
  }
    update.iter<-function(iter=0){
    return(iter+1)
  }
  print.iter <- function (tracelevel,iter){
    if (tracelevel>0){
      message(paste("opt.ic iteration:",as.character(iter)),"\r",appendLF=FALSE)
      flush.console()
    }
  }
  opt.iter = update.iter()
  print.iter(tracelevel,opt.iter)

  if(is.null(model)){model = "lm"}
  #newX=data.frame(X)
  Xnames <- colnames(X)
  if (length(Xnames) == 0){
    #colnames(X) <- c(paste("x",as.character(1:ncol(X)),sep=""))
    Xnames <- c(paste("x",as.character(1:ncol(X)),sep=""))
  }
  #newZ = update.Z(Y,newX)
  newZ = update.Z(Y,X)
  spec = formula(newZ)

  if (model != "lm"){
    newstart = suppressWarnings(
      if(model == "logit") {
        suppressWarnings(guessStartVal(Y=Y, X=X, model="logit"))
      } else if(model == "probit"){
        suppressWarnings(guessStartVal(Y=Y, X=X, model="probit"))
      }
    )
    newstart<-warmstart(newstart)
  }

  bestfit= fitnew(newspec=spec, model, newZ=newZ, newstart=newstart)

  oldaic = IC(bestfit, KLIC =KLIC)

  continue.search = TRUE

  opt.iter<-update.iter(opt.iter)

  newX=X
  if (memorymanagement == TRUE){
    rm(Y)
    rm(X)
  }

  while (continue.search==TRUE){
    if (memorymanagement == TRUE){
      tgc()
    }
    print.iter(tracelevel,opt.iter)

    if (length(coef(bestfit)[complete.cases(coef(bestfit))==FALSE])>0) {

      bestfit <- drop.na.model(namodel=bestfit)

      oldaic  <-IC(bestfit, KLIC =KLIC)

      newX <- bestfit$model[,-1]
      newZ <- update.Z(bestfit$model[,1], newX)

    } else {

      newX <- update.X(bestfit, newX)
      newZ <- update.Z(bestfit$model[,1], newX)

      newspec <- formula(newZ)
      newstart <- update.start(bestfit)

      oldaic  <-IC(bestfit, KLIC =KLIC)

      oldbestfit=bestfit
      bestfit <- fitnew (newspec, model, newZ, newstart)

      newaic  <-IC(bestfit, KLIC =KLIC)

      if (newaic > oldaic) {continue.search = FALSE; bestfit=oldbestfit}
    }
    opt.iter<-update.iter(opt.iter)
  }
  if (tracelevel > 0){
    message(paste("total opt.ic iterations:", as.character(opt.iter)))
  }
  if (returntype == "model"){
    return(bestfit)
  }
  if (returntype == "data"){
    return(bestfit$model[,-1])
  }
  if (returntype == "colnames"){
    return(colnames(bestfit$model[,-1]))
  }
}





#' A function that determines whether a point lies in a specified interval.
#'
#' @param x numeric.
#' @param interval vector of length 2 specifying the outer values of the interval.
#' @return TRUE/FALSE
#' @export
#' @examples withinInterval(5,c(1,10))

withinInterval <- function(x, interval){
   stopifnot(length(interval) == 2L)
   interval[1] < x & x < interval[2]
 }

#' Miscelaneous function used in main routines.
#'
#' @param Pmap a vector of probabilities.
#' @return A binary vector indicating occurrences.
#' @export
#' @examples
#' probabilities<-c(1:100)/100
#' occurrences<-PtoBin(probabilities)

PtoBin <- function (Pmap){
	Pmap[Pmap>0.5]<-1
	Pmap[Pmap<0.5]<-0
	return(Pmap)
}

#' Miscelaneous function used in main routines.
#'
#' @param testdata a vector of occurrences.
#' @param fitteddata a vector of occurrences.
#' @return A numeric indicating the degree of correspondence.
#' @export
#' @examples
#' observed <- c(1,1,1,0,0,0,0,0,0,0)
#' predicted <-c(1,0,1,0,0,0,0,1,1,0)
#'
#' print(accuracy(observed, predicted))

accuracy <- function (testdata, fitteddata){
	return(mean(1- abs(testdata-fitteddata)))
}

#' Miscelaneous function used in main routines.
#'
#' @param model a modelobject.
#' @return Boolean indicating whether supplied object is of class speedglm from package speedglm.
#' @export
#' @examples
#' y=c(1,0,1,0,1,0)
#' x=c(0,0,1,1,1,0)
#' is.speed(lm(y~x))


is.speed <- function (model){
	return(if("speedglm" %in% as.character(model$call) == TRUE){TRUE} else {FALSE})
}

#' Miscelaneous function used in main routines.
#'
#' @param data factorial or numeric data of a categorial nature.
#' @param class the category that should be reclassified into 1, all other values return as 0.
#' @return a binary dataset.
#' @export
#' @examples
#' y = c(1,2,3,4,1,2,3,4,1,2,3,4)
#, MLtoBinomData (y,4)

MLtoBinomData <- function (data, class){
	x = (max(data)+1)
	data[data==class]<- x
	data[data!=x]<- 0
	data[data==x]<- 1

	return(data)
}


#' Core function used by autoGLM, for details and usage see autoGLM.
#'
#' @param data See also \code{\link{autoGLM}}.
#' @param reclasstable \code{\link{autoGLM}}.
#' @param class \code{\link{autoGLM}}.
#' @param outputpath \code{\link{autoGLM}}.
#' @param modelname \code{\link{autoGLM}}.
#' @param tracelevel  \code{\link{autoGLM}}.
#' @param actions \code{\link{autoGLM}}.
#' @param NAval \code{\link{autoGLM}}.
#' @param model \code{\link{autoGLM}}.
#' @param preselect \code{\link{autoGLM}}.
#' @param method  \code{\link{autoGLM}}.
#' @param crit.t  \code{\link{autoGLM}}.
#' @param crit.p  \code{\link{autoGLM}}.
#' @param test  \code{\link{autoGLM}}.
#' @param KLIC  \code{\link{autoGLM}}.
#' @param accuracytolerance \code{\link{autoGLM}}.
#' @param confidence.alternative \code{\link{autoGLM}}.
#' @param use.share \code{\link{autoGLM}}.
#' @param maxsampleruns \code{\link{autoGLM}}.
#' @param memorymanagement  \code{\link{autoGLM}}.
#' @return a model object.
#' @export
#' @examples
#' randomlogit <- simulateLogit(nobs=5000, pars = c(0.5, -0.4, -0.3, 0.1, 0, 0, 0, 0, 0, 0))
#' opt.glm(randomlogit, outputpath="C:\\users\\")



opt.glm <- function (data, reclasstable="default", class=1, outputpath=paste(getwd(),"//", sep=""), modelname="autoGLM",
                     tracelevel=1, actions = c("print", "return"),
                     NAval =-9999, model="logit", preselect = "lm", method = "opt.ic",
                     crit.t = 1.64, crit.p =.1, test ="LR", KLIC = "AICc",
                     accuracytolerance =0.025, confidence.alternative =0.85,
                     use.share = 0.25, maxsampleruns=50, memorymanagement = TRUE){



  opt.glm.core <-function (data, reclasstable, class, outputpath, modelname,
                           tracelevel, actions,
                           NAval, model, preselect,
                           method, crit.t, crit.p,
                           test, KLIC, accuracytolerance,
                           confidence.alternative, use.share,
                           maxsampleruns, memorymanagement){

    if (tracelevel == 1){
      message("starting opt.glm.core")
    }
    if (memorymanagement == TRUE){
      tgc()
    }
    messages = character()
    m=1
    if (tracelevel == 1){
      message(paste("Printing for class:", as.character(class)))
    }
    if (tracelevel==1){
      message("Checking if required libraries are available")
    }
    packages<-c("plyr", "foreign", "sp", "data.table", "compiler", "speedglm")

    pkgTest(packages)

    if(is.character(data)==TRUE){
      if (tracelevel==1){
        message("Importing data from file")
      }
      data<-data.frame(fread(data))
    } else {
      if (tracelevel==1){
        message("Using in-memory data")
      }
      data<-as.data.frame(data)
    }

    suppressWarnings(
      if (reclasstable == "default"){
        reclasstable = cbind(data[,1], data[,1])
      } else if(is.character(reclasstable)==TRUE){
        if (tracelevel==1){
          message("Importing reclasstable from file")
        }
        reclasstable<-data.frame(fread(reclasstable))
      } else {
        if (tracelevel==1){
          message("Using in-memory reclasstable")
          reclasstable <- as.data.frame(reclasstable)
        }
      }
    )


    coefnamelist =colnames(data[,-1])
    if (tracelevel==1){
      message("Searching for a sample that is representative of the population dataset")
    }

    samples <- getSamples (data = data, share=use.share, confidence.alternative=confidence.alternative,
                           max.iter =maxsampleruns, tracelevel = tracelevel, memorymanagement = memorymanagement)

    if(is.character(samples) == TRUE){
      message = "Sampling failed, this is a probabilistic property. Try again. If sampling fails again, consider increasing datashare or increasing confidence.alternative, see the documentation of getSamples."
      messages[m]<-message
      m=m+1
      stop(message)
    } else {message("F-tests and t-tests do not find evidence for a deviating sample. Sampling successful, preparing training and testing datasets.")}
    trainSample = data[samples,]
    testSample = data[-samples,]

    if (tracelevel==1){
      message("Applying reclassification")
    }

    if(FALSE %in% ((range(trainSample[,1])==c(0,1)))){
      trainSample = reclassify(LUdata=trainSample, reclasstable=reclasstable, JIT =TRUE)
      testSample = reclassify(LUdata=testSample, reclasstable=reclasstable, JIT =TRUE)

      trainY=MLtoBinomData(trainSample[,1], class)
      trainX=trainSample[,2:ncol(trainSample)]
      testY =MLtoBinomData(testSample[,1], class)
      testX =testSample[,2:ncol(testSample)]
    } else {
      trainY=trainSample[,1]
      trainX=trainSample[,2:ncol(trainSample)]
      testY =testSample[,1]
      testX =testSample[,2:ncol(testSample)]
    }
    # split data for fitting


    if (memorymanagement == TRUE){
      rm(trainSample)
      rm(testSample)
      rm(samples)
      rm(data)
      tgc()
    }

    if (preselect == "lm"){
      if (tracelevel==1){
        message("Running a linear preselection algorithm")
        message(paste("Using optimization routine:", method))
      }
      bestX <- selectX(Y=trainY, X=trainX, model=preselect, returntype="colnames", method=method, KLIC = KLIC, crit.t=crit.t, crit.p = crit.p, test =test,
                       share = 0.75, confidence.alternative=confidence.alternative, max.iter =maxsampleruns, tracelevel = tracelevel, memorymanagement = memorymanagement)
    } else {bestX = colnames(trainX)}

    if (tracelevel==1){
      message("Running generalize to specific")
      message(paste("Using model specification =", model))
      message(paste("Using optimizer =", method))
    }
    bestmodel <- generalizeToSpecific(model=model, Y=trainY, X=trainX[bestX], method =method, crit.t =crit.t, crit.p = crit.p, test =test,
                                      KLIC = KLIC, tracelevel = tracelevel, memorymanagement =memorymanagement)

    bestX = colnames(bestmodel$model[,-1])
    #usedvars = bestX#colnames(bestmodel$model[,-1])
    #bestX= trainX[usedvars]
    droppedvars = setdiff(colnames(trainX), bestX)

    if (memorymanagement == TRUE){
      tgc()
    }

    # predict the conditional probabilities
    Pglm <- predict(bestmodel, newdata=trainX[bestX], type = "response")
    if(0 %in% Pglm || 1 %in% Pglm){
      message = "Probabilities of 0 and 1 are predicted by the final model, some of your variables might have variation only within or outside land use cover or otherwise perfect discriminatory power. This is not necessarily problematic, but expert judgement on your data quality is recommended."
      messages[m]<-message
      m=m+1
      warning(message)
    } else {message("Probabilities of 0 and 1 are not predicted by the final model.")}
    if (tracelevel==1){
      message("Checking accuracy of results")
    }
    # within sample the predicted occurence
    binglm = PtoBin(Pglm)
    # obtain the within sample overall accuracy
    accuracyglm = accuracy(testdata= trainY, fitteddata = binglm)
    # obtain the within sample accuracy at true LU sites
    accuracyglm3 = accuracy(testdata=trainY[trainY==1], fitteddata = binglm[trainY==1])
    # obtain the accuracy at sites predicted by the fitted model
    accuracyglm5 = accuracy(testdata=trainY[binglm>0.5], fitteddata = binglm[binglm>0.5])
    # out of sample prediction
    Pglm2 <- predict(bestmodel, newdata=testX[bestX], type = "response")
    binglm2 = PtoBin(Pglm2)
    # out of sample overall accuracy
    accuracyglm2 = accuracy(testdata=testY, fitteddata=binglm2)
    # obtain out of sample accuracy at true LU sites
    accuracyglm4 = accuracy(testdata=testY[testY==1], fitteddata=binglm2[testY==1])
    # obtain the accuracy at sites predicted by the fitted model
    accuracyglm6 = accuracy(testdata=testY[binglm2>0.5], fitteddata = binglm2[binglm2>0.5])
    accuracyresults<- data.frame(withinaoverall = accuracyglm, withinatsites = accuracyglm3,  withinatpredicted = accuracyglm5,
                                 OOSoverall = accuracyglm2, OOSatsites = accuracyglm4, OOSatpredicted = accuracyglm6)

    descriptionP <- describe(c(Pglm,Pglm2))
    descriptionP[1]<-"Phat"

    if (memorymanagement == TRUE){
      rm(Pglm)
      rm(binglm)
      rm(Pglm2)
      rm(binglm2)
      tgc()
    }

    if ("write" %in% actions){
      if (tracelevel==1){
        message("Attempting to write weightsfile")
      }
      # export weightsfile
      filename = paste(as.character(class),"_weights_binomial.csv",sep="")
      outdir = outputpath
      exportWeightsfile(model=bestmodel, originaldata=trainX, modeldata=trainX[bestX],
                        coefnamelist=coefnamelist, outdir=outdir,
                        modelname=modelname, filename=filename)
    }

    if (memorymanagement == TRUE){
      rm(trainX)
      rm(trainY)
      rm(testX)
      rm(testY)
      tgc()
    }

    if ("print" %in% actions){
      message(paste("Used:", toString(bestX)))
      message(paste("Dropped:", toString(droppedvars)))
      message(paste("Accuracy for class", as.character(class), ":", toString(droppedvars)))
      message(paste("Overall within:", as.character(accuracyglm)))
      message(paste("Overall out of sample:", as.character(accuracyglm2)))
      message(paste("Within sample at true sites:", as.character(accuracyglm3)))
      message(paste("Out of sample at true sites:", as.character(accuracyglm4)))
      message(paste("Within sample at predicted sites:", as.character(accuracyglm5)))
      message(paste("Out of sample at predicted sites:", as.character(accuracyglm6)))
      crange = c((accuracyglm3-accuracytolerance), (accuracyglm3+accuracytolerance))
      if (withinInterval(accuracyglm4, crange) == FALSE) {
        message = paste("For class ",as.character(class), ": Out of sample accuracy at true land use sites is not within the specified tolerance range around the within sample accuracy, inspect the printed accuracy measures and consider increasing the share of used data!")
        messages[m]<-message
        m=m+1
        warning(message)
      } else {message(paste("For class ",as.character(class), ": out of sample accuracy is within tolerated interval. Sample is representative of entire dataset."))}
      message(paste("Description of the total sample predicted probabilities", "for class", as.character(class), ":" ) )
      print(descriptionP)

      if ("write" %in% actions){
        printedfiles = list.files(path=outdir)
        if (filename %in% printedfiles){
          message(paste("Weightsfile printed to", outdir))
        } else {
          message = "Write weightsfile was specified, but could not find weightsfile in directory after printing attempt. Check if it's there, and export the weights file manually with the exportWeightsfile function if needed."
          messages[m]<-message
          m=m+1
          warning(message)}
      }
      print(summary(bestmodel))
    }

    if ("log" %in% actions){
      if (length(messages) <1) {messages[1] <- "No warnings"}
      currenttime = Sys.time()
      content = data.frame(capture.output(list(GENERATED= currenttime, messages= data.frame(messages), usedvars = data.frame(bestX), droppedvars = data.frame(droppedvars),
                                               logitmodel = summary(bestmodel), accuracyresults = accuracyresults, predictedPdescription = descriptionP)))
      content = data.frame(LOGFILE=content[-1,])

      printname =paste(paste(as.character(class), paste("_",as.character(as.numeric(currenttime)),sep=""),sep=""), "_log.txt", sep ="")
      outdir = outputpath
      logfile<-file(paste(outdir,printname, sep=""))
      write.csv(content, logfile)

      printedfiles = list.files(path=outputpath)
      if (printname %in% printedfiles){
        message(paste(paste("Logfile", printname), "printed to", outdir))
      } else {if("log" %in% actions){
        message = "Write logfile was specified, but could not find logfile in directory after printing attempt. Check if it's there, and save the screen messages manually if needed."
        warning(message)}
      }
    }
    if (tracelevel == 1){
      message("closing opt.glm.core")
    }
    if ("return" %in% actions){
      robject = list(logitmodel = bestmodel, usedvars = bestX, droppedvars = droppedvars,
                     predictedPdescription = descriptionP, accuracyresults = accuracyresults)
      if (memorymanagement == TRUE){
        rm(list = setdiff(ls(), "robject"))
        tgc()
      }
      return(robject)
    } else {
      robject=0
      if (memorymanagement == TRUE){
        rm(list = setdiff(ls(), "robject"))
        tgc()
      }
      return(robject)
    }
  }

  returnobject <- tryCatch(
    opt.glm.core(data, reclasstable, class, outputpath, modelname,
                 tracelevel, actions, NAval, model, preselect, method, crit.t, crit.p, test, KLIC,
                 accuracytolerance, confidence.alternative, use.share, maxsampleruns, memorymanagement),
    error = function (e) {"Numerical problem, trying one more time from start under stricter conditions."}
  )
  if (memorymanagement == TRUE){
    tgc()
  }
  if(is.character (returnobject) == TRUE) {
    warning(returnobject)
    returnobject <- tryCatch(
      opt.glm.core(data, reclasstable, class, outputpath, modelname,
                   tracelevel, actions, NAval, model ="logit", preselect ="lm", method, crit.t, crit.p, test, KLIC,
                   accuracytolerance, confidence.alternative = confidence.alternative*0.95,
                   use.share = min(use.share*1.25,1), maxsampleruns, memorymanagement),
      error = function (e) {"Calibration failed. Numerical problem on second run. Please check your data or try a larger sampling share."}
    )
    if(is.character (returnobject) == TRUE) {
      warning("All your error are belong to us. You have no chance to survive make your time.")
      stop(returnobject)
    } #else {returnobject = newreturn}
  }
  return(returnobject)
}









