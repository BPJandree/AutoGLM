#########################################################################################################
#      __    __  ___________ ___       _________    __    ________  ____  ___  ______________  _   __  	#
#     / /   / / / /  _/ ___//   |     / ____/   |  / /   /  _/ __ )/ __ \/   |/_  __/  _/ __ \/ | / /  	#
#    / /   / / / // / \__ \/ /| |    / /   / /| | / /    / // __  / /_/ / /| | / /  / // / / /  |/ /   	#
#   / /___/ /_/ // / ___/ / ___ |   / /___/ ___ |/ /____/ // /_/ / _, _/ ___ |/ / _/ // /_/ / /|  /    	#
#  /_____/\____/___//____/_/  |_|   \____/_/  |_/_____/___/_____/_/ |_/_/  |_/_/ /___/\____/_/ |_/     	#
#  								  			               												    																	  #
#                 ____           ___              __                  ___   ____ ________              	#
#                / __ )____     /   |  ____  ____/ /_______  ___     |__ \ / __ <  / ___/	       	#
#               / __  / __ \   / /| | / __ \/ __  / ___/ _ \/ _ \    __/ // / / / / __ \ 	       	#
#              / /_/ / /_/ /  / ___ |/ / / / /_/ / /  /  __/  __/   / __// /_/ / / /_/ / 	       	#
#             /_____/\____/  /_/  |_/_/ /_/\__,_/_/   \___/\___/   /____/\____/_/\____/  	       	#
# 												       													                                              #
# Example code for package autoGLM v 1.0.1. 			  					       								                            #
# @Bo Andree											       											                                          #
# @ b.p.j.andree@vu.nl 										       										                                    #
# All the function are in the file "autoGLM.r"	 						       							                        #
#########################################################################################################

#### BUGS

# A list of known issues is maintained here: https://github.com/BPJandree/AutoGLM/blob/master/README.md

#### INSTALLING

# On your first run, install the package from github.
library(devtools)
install_github("BPJandree/AutoGLM")
# Downloading might take a while as the package contains sample data.


#### LOADING

# Load the library:
library(autoGLM)
# if you want to work with a large dataset, I recommend to use fread from the data.table package:
# pkgTest("data.table")


#### BASIC ANALYSIS

# If you want to work with you own data, specify the path to the CSV files that includes all observations.
# Land Use should be the first colummn.
# csvdatapath = "C:\\Users\\"

# Also speficy the path to the reclass table with CORINE to LUISA codes if you wish to use your own reclassification scheme.
# corinereclasstablepath = "C:\\Users\\"

# You can load the files with the following dashed out commands:
# ITdata <-data.frame(fread("csvdatapath"))
# corinetable<-data.frame(fread("corinereclasstablepath"))

# It is convenient to load data to memory first, but you can also point autoGLM to a path.
# It is memory efficient to pass the filepath to the function, as in this case there is no unnecesary duplicate stored in the RAM,
# but if you calibrate for multiple land use classes, passing a loaded object can be slightly faster as it saves on reading time.

# Specify the outputpath for the weightsfile. Names of the weights default to the column names of the data. If these are not the "w__" names, you need to manually edit the outputted file.
weightsfilepath= "C:\\Users\\"
# Specify the country for which you generate a weights file.
countryname = "IT"
# The outputted weightsfile will start with the number of the land use supplied. (e.g, "1_weights_binomial"). Manual edit of the name is required.

# The programm is robust to bad variables, but it can't know if you supply endogenous variables.
# Always think about what you feed into a model.

# As an example case, we can work with the sample data supplied in the package.
# Load the data
data(ITdata)
data(corinetable)

# I've implemented a simple function to describe the data:
describe(ITdata)

# RUN the autoGLM command:
# By default, the outputpath is you working directory.
# If no default set in your system32 settings, the command will not work unless you supply an outputpath.
# you can run getwd() to check this.
##### KNOWN BUG: Warning messages: In if (reclasstable == "default") { :the condition has length > 1 and only the first element will be used
##### You can ignore this, I will fix this.
# There are multiple options that you can specify, but most have a default.

# default settings, no log file, no writing of a weightsfile
results <- autoGLM(data=ITdata, reclasstable=corinetable, class=0)
# log file and a weightsfile
results <- autoGLM(data=ITdata, reclasstable=corinetable, class=0, outputpath=weightsfilepath, actions =c("write", "log", "return", "print"))
# optimize with t-tests
results <- autoGLM(data=ITdata, reclasstable=corinetable, class=0, method ="opt.t") # quasi-comple separability
# optimize with hypothesis testing
results <- autoGLM(data=ITdata, reclasstable=corinetable, class=0, method ="opt.h")



# Some more options (for all options, please see the manual).
results <- autoGLM(data=ITdata, reclasstable=corinetable, class=0, outputpath=weightsfilepath, modelname="IT",
 							tracelevel=1, actions=c("write", "print", "log", "return"), NAval="default", model="logit", preselect="lm",
 								method="opt.ic", KLIC="AICc", accuracytolerance=0.01, confidence.alternative=0.85,
 									use.share=0.5, maxsampleruns=50, memorymanagement=TRUE)


# You can also calibrate over multiple classes. When working with large data, I recommend to leave a copy on the hard disk instead of the RAM, so pass on a path to the files:

#ITdata<- "C:\\Users"
landusevec <- c(0,1,2,3,4,5,6,16)

#### The command by default only prints resutls and returns only the object of the last estimation.
# If you want to restore the objects of all the results, you can specify  returnall="writedisk" to
# write your estimation objects as serialized images to your outputpath. See the manual for more detail.
#### Be sure to delete them afterwards if you do not wish to keep them. They can be large in size.
returnall="writedisk"
actions=c("write", "print", "log", "return")
calibration <- autoGLM(data=ITdata, reclasstable=corinetable, class=landusevec, 
		       outputpath=weightsfilepath, returnall=FALSE, actions = c("print", " return")) # <- defaults


# to see some of the core functions in action:
set.seed(2304)
######## Test with Random data:
randomlogit <- simulateLogit(nobs=5000, pars = c(0.5, -0.4, -0.3, 0.1, 0, 0, 0, 0, 0, 0))

results <- autoGLM(data=randomlogit, reclasstable=corinetable, class=0, method ="opt.ic")
results <- autoGLM(data=randomlogit, reclasstable=corinetable, class=0, method ="opt.h")



# More general use is through generalizeToSpecific()
gtslin <- generalizeToSpecific(model="lm", Y=randomlogit[,1], X=randomlogit[,-1])
summary(generalizeToSpecific(model="lm", Y=randomlogit[,1], X=randomlogit[,-1]), method="opt.h")
summary(gtslin)

gtslogit <- generalizeToSpecific(model="logit", Y=randomlogit[,1], X=randomlogit[,-1])
summary(gtslogit)



# More specific use is through the opt routines.
bestlogitIC<-opt.ic(model="logit", Y=randomlogit[,1], X=randomlogit[,-1])
bestprobitICdata <-opt.ic(model="probit", Y=randomlogit[,1], X=randomlogit[,-1], returntype="data")
colnames(bestprobitICdata)


# to see some of the core functions in action:
set.seed(2304)
######## Test with Random data:

randomlogit <- simulateLogit(nobs=2000, pars = c(0.5, -0.4, -0.3, 0.1, 0.05, 0.025, 0.01, 0.005, 0.005, 0.005,0.005,0.005,0.005,0.0025,0.0025,0.0025,0.0025,0.0,0.0,0.0))
randomlogit<-cbind(randomlogit,mcv = randomlogit[,2]) # add multicollinear vector, to see how the method responds to faulty variables.

Y=randomlogit[,1]
# if your data is quasi-perfectly separable, coefficient estimates, standard errors and z-scores may have exreme values
# for the example, we optimize over a mis-specified model
X=randomlogit[,-1]
X=X[,-c(10,14,16)]

test1<-opt.ic(model="logit", Y, X, returntype="model", tracelevel=0, memorymanagement=TRUE)
test2<-opt.t(model="logit", Y, X, returntype="model", tracelevel=0, memorymanagement=TRUE)
test3<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=0, crit.p=0.1, test="LR", memorymanagement=TRUE)
test4<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=0, crit.p=0.1, test="F", memorymanagement=TRUE)
test5<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=0, crit.p=0.1, test="Chisq", memorymanagement=TRUE)
test6<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=0, crit.p=0.1, test="LR", memorymanagement=TRUE)
test7<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=0, crit.p=0.1, test="F", memorymanagement=TRUE)
test8<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=0, crit.p=0.1, test="Chisq", memorymanagement=TRUE)
fullmodel <- logit(randomlogit)

# routines based on the covariance matrix, e.g.,
# test2 and test6 will always identify a model that suffers quasi-separability as optimal because the z-scores
# falsely suggest that the variables are extremely significant.
# AIC joint significanse tests are more resistant.


# with example data for one class


Y <- reclassify(ITdata, reclasstable = corinetable)

classes <- sort(unique(Y[,1]))
scheme2 <- cbind(classes,c(1, rep(0, length.out=(length(classes)-1)) ) )

Y <- reclassify(Y, reclasstable = scheme2)
X=Y[,-1]
Y=Y[,1]

# run on the complete datasets
test1b<-opt.ic(model="logit", Y, X, returntype="model", tracelevel=1, memorymanagement=TRUE)
test2b<-opt.t(model="logit", Y, X, returntype="model", tracelevel=1, memorymanagement=TRUE)
test3b<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=1, crit.p=0.1, test="LR", memorymanagement=FALSE)
test4b<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=1, crit.p=0.1, test="F", memorymanagement=FALSE)
test5b<-opt.h(model="logit", Y, X, returntype="model", method="joint", tracelevel=1, crit.p=0.1, test="Chisq", memorymanagement=FALSE)
test6b<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=1, crit.p=0.1, test="LR", memorymanagement=FALSE)
test7b<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=1, crit.p=0.1, test="F", memorymanagement=FALSE)
test8b<-opt.h(model="logit", Y, X, returntype="model", method="single", tracelevel=1, crit.p=0.1, test="Chisq", memorymanagement=FALSE)

# type ?autoGLM for more information.



#########################################################################################################################################################################
#	EXAMPLES TO ALL OTHER FUNCTIONS. THIS IS FOR MORE ADVANCED ANALYSIS.																								#
#########################################################################################################################################################################

library(autoGLM)
pkgTest(c("gmm", "foreign", "sp", "data.table", "compiler", "speedglm"))

##########################################
# Generate some data or import some data #
##########################################

# Import a file from csv, or go with the data preinstalled with the package.
data(ITdata)

# we will be working with large datasets. key to fitting good models in a feasible way, is to fit the model on a sample that is representative for the population (country) data.
# we will grab samples that have similar second and first moments by calling the function "getSamples".
# it is important to get a representative sample BEFORE reclassification.
# confidence.alternative is defined as the probability level at which the alternative is accepted.
# For confidence.alternative = .9, we need less evidence to accepted that the samples are unequal, than at confidence.alternative = .95.
# Hence, .90 is stricter than .95.
# smaller shares, and stricter the confidence levels, reduce the probability of grabbing a proper sample. Specify max.iter to kill the function if no proper sample is found.

samples <- getSamples (data = ITdata, share = 0.2, confidence.alternative=0.85, max.iter =100, tracelevel =1)

trainSample <- ITdata[samples,]
testSample <- ITdata[-samples,]

# compare the samples:
describe(trainSample)
describe(testSample)

# we can compare the histograms of land use, to see whether the datasets are indeed comparable in terms of land use occurence:
par(mfrow=c(1, 2))

hist(trainSample[,1])
hist(testSample[,1])

# the raw corine data has high detail in LU classes. If you work with raw corine you can reclassify within the R project.
# I recommend this, because the getSample command does a better job at grabbing a representative sample if you supply it with pre-reclassified data.

# load a reclass table or go with the one preinstalled with the package:

reclasstable <- corinetable

# a compiled relclassify function can be called:
reclassified_trainSample <- reclassify(LUdata=trainSample, reclasstable=reclasstable, JIT =TRUE, dropNA = TRUE, NAval="default")
reclassified_testSample <- reclassify(LUdata=testSample, reclasstable=reclasstable, JIT =TRUE, dropNA = TRUE, NAval="default")


######## TO WORK WITH THE IT DATA:
	trainY=trainSample[,1]
	trainX=trainSample[,2:ncol(trainSample)]
	testY =testSample[,1]
	testX =testSample[,2:ncol(testSample)]


# we are doing binomial analysis, so we're gonna pick a single land use to work with.
# here we work with land use class "0"
analyzeLU = 0 # <- urban
#analyzeLU = 5 # <- forest
#analyzeLU = 1 # <- industry

# convert to categorical data to binary data
trainY<-MLtoBinomData(trainY, analyzeLU)
testY<-MLtoBinomData(testY, analyzeLU)


##########################################
# 		Start of Modelling Workflow		 #
##########################################


# I've implemented an automated optimization of the generalized linear models with a logit/probit link (iterative weighted least squares estimation)
# and without a link function (least squares). note that this latter approach is not a smart way to actually model conditional probabilities, but for explorative purposes, it can be useful.
# functions sorted by computational load, try the lm first!

# linear model using corrected AIC
bestlm <- generalizeToSpecific(model="lm", Y=trainY, X=trainX, method = "opt.ic", KLIC = "AICc")
#summary(bestlm)
# linear model using t-values
bestlm2 <- generalizeToSpecific(model="lm", Y=trainY, X=trainX, method = "opt.t", crit.t = 1.64)
#summary(bestlm2)
# linear model with join significance F-tests
bestlm3 <- generalizeToSpecific(model="lm", Y=trainY, X=trainX, method = "opt.h", crit.p = .1, test = "F")
#summary(bestlm3)

# logit model using the AIC
bestlogit <- generalizeToSpecific("logit", trainY, trainX, KLIC = "AIC")
# logit model using stricter t-tests
bestlogit2 <- generalizeToSpecific("logit", trainY, trainX, method = "opt.t", crit.t = 2.54)
# logit model using LR tests between nested models
bestlogit3 <- generalizeToSpecific("logit", trainY, trainX, method = "opt.h")

# probit with corrected AIC
bestprobit <- generalizeToSpecific("probit", trainY, trainX, KLIC = "AICc")
# probit with t-tests
bestprobit2 <- generalizeToSpecific("probit", trainY, trainX, method = "opt.t", crit.t = 1.64)


###### IMPORTANT NOTE: By now it should have occured in several model results that the parameter/standard errors/z-values
# all obtain extreme values. This is a sign of (quasi)-complete seperation. Especially strict significance strategies
# that result it the removal of most variables may result in quasi-completely seprated models.



# If you plan not to use the model results, but you are only interested in preselecting data, you can use selectX to find variables that work well.

# LPM:
bestXlinear <- selectX(trainY, trainX, model ="lm", returntype = "data", share = 1)
bestXlinear2 <- selectX(trainY, trainX, model ="lm", method = "opt.t", crit.t = 1.64, returntype = "colnames", share = 0.75)
describe(bestXlinear)

# for logit:
bestXlogit <- selectX(trainY, trainX, model ="logit", returntype = "data", share = 1)
bestXlogit2 <- selectX(trainY, trainX, model ="logit", method = "opt.t", crit.t = 1.64, returntype = "colnames", share = 0.75)
describe(bestXlogit)

# for probit:
bestXprobit <- selectX(trainY, trainX, model ="probit", returntype = "data", share = 1)
bestXprobit2 <- selectX(trainY, trainX, model ="probit", method = "opt.t", crit.t = 1.64, returntype = "colnames", share = 0.75)
describe(bestXprobit)


# I recommend to use the bestXlinear data, and then using generalizeToSpecific on the remaing dataset (it's the fastest algorithm, and robust to overfitting and numerical problems in the IWLS procedure):
bestX = bestXlinear
oldtestX=testX
	testX = testX[colnames(bestX)]

# set starting values for optimization, in this case we start at 0 for al parameters.
start <- rep(0,length.out=dim(bestX)[2]+1)

# ANALYSIS:

############ fit the linear probability model ############
glmF = formula(cbind(trainY,bestX))
#LPM
LPM <- lm(glmF,data=bestX)
#inspect estimation results
summary(LPM)
# predict the conditional probabilities.
Plin <- predict(LPM, newdata=trainX)
# obtain the predicted occurence
yhatlm=Plin
yhatlm[yhatlm>0.5]<-1
yhatlm[yhatlm<0.5]<-0
# obtain the within sample overall accuracy
accuracylm = mean(1- abs(trainY-yhatlm))
print(paste("overall within:", as.character(accuracylm)))
# obtain out of sample overall accuracy
Plin2 <- predict(LPM, newdata=testX)
yhatlm2=Plin2
yhatlm2[yhatlm2>0.5]<-1
yhatlm2[yhatlm2<0.5]<-0
accuracylm2 = mean(1- abs(testY-yhatlm2))
print(paste("overall out of sample:", as.character(accuracylm2)))
# obtain the within sample accuracy at true LU sites
accuracylm3 = mean(1- abs(trainY[trainY==1]-yhatlm[trainY==1]))
print(paste("within sample at true sites:", as.character(accuracylm3)))
# obtain out of sample accuracy at true LU sites
accuracylm4 = mean(1- abs(testY[testY==1]-yhatlm2[testY==1]))
print(paste("out of sample at true sites:", as.character(accuracylm4)))


############ fit the logit probability model as a generalized linear model ############
glmF = formula(cbind(trainY,bestX))
#logit
logit <- glm(glmF, family=binomial(link='logit'), data=bestX, start = start)
#inspect estimation results
summary(logit)
# predict the conditional probabilities.
# "predict" works with "glm". More generally you may use my logistic function implementation: logistic(parameters, data)
# where the first column of data must be a unit vector that interacts with the constant in the parameter set.
Pglm <- predict(logit, newdata=trainX, type = "response")#fitted(logit)
test <- logistic(coef(logit), data=cbind(1,bestX))

# obtain the predicted occurence
binglm = Pglm
binglm[binglm>0.5]<-1
binglm[binglm<0.5]<-0
# obtain the within sample overall accuracy
accuracyglm = mean(1- abs(trainY-binglm))
print(paste("overall within:", as.character(accuracyglm)))
# obtain out of sample overall accuracy
Pglm2 <- predict(logit, newdata=testX, type = "response")
binglm2 = Pglm2
binglm2[binglm2>0.5]<-1
binglm2[binglm2<0.5]<-0
accuracyglm2 = mean(1- abs(testY-binglm2))
print(paste("overall out of sample:", as.character(accuracyglm2)))
# obtain the within sample accuracy at true LU sites
accuracyglm3 = mean(1- abs(trainY[trainY==1]-binglm[trainY==1]))
print(paste("within sample at true sites:", as.character(accuracyglm3)))
# obtain out of sample accuracy at true LU sites
accuracyglm4 = mean(1- abs(testY[testY==1]-binglm2[testY==1]))
print(paste("out of sample at true sites:", as.character(accuracyglm4)))



############ fit the probit model as a generalized linear model ############
glmF = formula(cbind(trainY,bestX))
#logit
probit <- glm(glmF, family=binomial(link='probit'), data=bestX, start = start)
#inspect estimation results
summary(probit)
# predict the conditional probabilities
Pglmp <- predict(probit, newdata=trainX, type = "response")#fitted(logit)
# obtain the predicted occurence
binglmp = Pglmp
binglmp[binglmp>0.5]<-1
binglmp[binglmp<0.5]<-0
# obtain the within sample overall accuracy
accuracyglmp = mean(1- abs(trainY-binglmp))
print(paste("overall within:", as.character(accuracyglmp)))
# obtain out of sample overall accuracy
Pglmp2 <- predict(probit, newdata=testX, type = "response")
binglmp2 = Pglmp2
binglmp2[binglmp2>0.5]<-1
binglmp2[binglmp2<0.5]<-0
accuracyglmp2 = mean(1- abs(testY-binglmp2))
print(paste("overall out of sample:", as.character(accuracyglmp2)))
# obtain the within sample accuracy at true LU sites
accuracyglmp3 = mean(1- abs(trainY[trainY==1]-binglmp[trainY==1]))
print(paste("within sample at true sites:", as.character(accuracyglmp3)))
# obtain out of sample accuracy at true LU sites
accuracyglmp4 = mean(1- abs(testY[testY==1]-binglmp2[testY==1]))
print(paste("out of sample at true sites:", as.character(accuracyglmp4)))


# compare results

par(mfrow=c(1, 3))

results = data.frame(
	data = trainY,
	lin = Plin,
	logit =Pglm,
	probit = Pglmp
	)

cor(results)

plot(sort(results$data), type = "l", lty =1, main = "Within Sample")
lines(sort(results$lin), lty =1, col=2)
lines(sort(results$logit), lty =1, col=3)
lines(sort(results$probit), lty =2, col=4)

legend("bottomright", c("data", "lin", "logit", "probit"), col=c(1,2,34) , lty=c(1,1,1,2))


results2 = data.frame(
	data = testY,
	lin = Plin2,
	logit =Pglm2,
	probit = Pglmp2
	)

cor(results2)

plot(sort(results2$data), type = "l", lty =1, main = "Out of Sample")
lines(sort(results2$lin), lty =1, col=2)
lines(sort(results2$logit), lty =1, col=3)
lines(sort(results2$probit), lty =2, col=4)

legend("bottomright", c("data", "lin", "logit", "probit"), col=c(1,2,3,4) , lty=c(1,1,1,2))



results3 = data.frame(
	data = testY[testY==1],
	lin = Plin2[testY==1],
	logit =Pglm2[testY==1],
	probit = Pglmp2[testY==1]
	)

cor(results3)

plot(sort(results3$data), type = "l", lty =1, main = "Out of Sample at actual sites")
lines(sort(results3$lin), lty =1, col=2)
lines(sort(results3$logit), lty =1, col=3)
lines(sort(results3$probit), lty =2, col=4)

legend("bottomright", c("data", "lin", "logit", "probit"), col=c(1,2,3,4) , lty=c(1,1,1,2))



# compare the logit with the model fitted using "generalizeToSpecific"
# predict the conditional probabilities
Pglmb <- predict(bestlogit, newdata=trainX[,names(coef(bestlogit))[-1]], type = "response")#fitted(logit)
# obtain the predicted occurence
binglmb = Pglmb
binglmb[binglmb>0.5]<-1
binglmb[binglmb<0.5]<-0
# obtain the within sample overall accuracy
accuracyglmb = mean(1- abs(trainY-binglmb))
print(paste("overall within:", as.character(accuracyglmb)))
# obtain out of sample overall accuracy
Pglmb2 <- predict(bestlogit, newdata=oldtestX[,names(coef(bestlogit))[-1]], type = "response")
binglmb2 = Pglmb2
binglmb2[binglmb2>0.5]<-1
binglmb2[binglmb2<0.5]<-0
accuracyglmb2 = mean(1- abs(testY-binglmb2))
print(paste("overall out of sample:", as.character(accuracyglmb2)))
# obtain the within sample accuracy at true LU sites
accuracyglmb3 = mean(1- abs(trainY[trainY==1]-binglmb[trainY==1]))
print(paste("within sample at true sites:", as.character(accuracyglmb3)))
# obtain out of sample accuracy at true LU sites
accuracyglmb4 = mean(1- abs(testY[testY==1]-binglmb2[testY==1]))
print(paste("out of sample at true sites:", as.character(accuracyglmb4)))

# The accuracy of the automated logit is highest of all models in most of the runs I did. It also is the fastest algorithm and smallest amount of code.
# we can export the results to a file that geoDMS can use. Replace the coefnamelist with the names of the weights before using it.

exportWeightsfile(model = bestlogit, originaldata = trainX, modeldata = trainX[,names(coef(bestlogit))[-1]],
					coefnamelist =names(ITdata[-1,]), outdir = "C:\\Users\\",
						modelname = "IT", filename = "Urban_weights_binomial.csv")



######################################################################################################
# LOGIT ESTIMATION WITH GMM <---- DOES NOT REQUIRE A UNIQUE SOLUTION, NOT SUITABLE FOR LARGE SAMPLES #
######################################################################################################
### GMM is a very general estimator. For some models, a unique solution is not garuanteed to exist.
# In theses cases, M estimation is not garantueed to satisfy asymptotic theory.
# Note that uniqueness of a solution depends on smootheness of a function's argument.
# For robustness, or if you have doubt about the uniqueness of a solution, you can estimate a logit model with GMM.

randomlogit <- simulateLogit(nobs=10000, pars = c(0.25, -0.2, -0.3, 0.1, 0.05, 0.025, 0.01, 0.005, 0.005, 0.005,0.005,0.0025,0.0025,0.0025,0.0025,0,0,0,0,0,0))


# I have implemented a GMM logit approach. the "guess" function works with gmm. Port routines are quite stable,
# whereas bfgs is faster but may not always properly converge if there is string seperation in your dataset.
gmminit <- guessStartVal(Y=randomlogit[,1], X=randomlogit[,-1], model ="gmm_nlminb")


# GMM estimation run  heavy compared to the IWLS estimator in the glm logit.
# maximizer = "nlminb" calls port routines which are usually quite robust
# method = "BFGS" calls the bfgs optimizer which is usually quite fast, but less robust.
# I suggest to use the port routines.


gmmlogit <- logit(randomlogit, method="gmm", start =gmminit, maximizer ="nlminb")
iwlslogit <- logit(randomlogit, start =gmminit)

X = randomlogit[,-1]
Y = randomlogit[,1]
# predict the conditional probabilities of gmm model
Pgmm <- logistic(c(coef(gmmlogit)), cbind(1,X))
# obtain the predicted occurence
bingmm = Pgmm
bingmm[bingmm>0.5]<-1
bingmm[bingmm<0.5]<-0
# obtain the within sample overall accuracy
accuracygmm = mean(1- abs(Y-bingmm))
print(paste("overall within:", as.character(accuracygmm)))
# obtain the within sample accuracy at true LU sites
accuracygmm3 = mean(1- abs(Y[Y==1]-bingmm[Y==1]))
print(paste("within sample at true sites:", as.character(accuracygmm3)))


# predict the conditional probabilities of logit model
Piwls <- logistic(c(coef(iwlslogit)), cbind(1,X))
# obtain the predicted occurence
biniwls = Piwls
biniwls[biniwls>0.5]<-1
biniwls[biniwls<0.5]<-0
# obtain the within sample overall accuracy
accuracyiwls = mean(1- abs(Y-biniwls))
print(paste("overall within:", as.character(accuracyiwls)))
# obtain the within sample accuracy at true LU sites
accuracyiwls3 = mean(1- abs(Y[Y==1]-biniwls[Y==1]))
print(paste("within sample at true sites:", as.character(accuracyiwls3)))

