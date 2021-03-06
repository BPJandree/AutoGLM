% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoGLM.R
\name{iapply}
\alias{iapply}
\title{lapply without memory builtup. Iterative application of a function over a vector of arguments, returning only the last result as an object or writing all output to disk.}
\usage{
iapply(X, FUN, writedisk = FALSE, outdir = "default", name = "default",
  compress = FALSE)
}
\arguments{
\item{X}{a vector of input variables similar to lapply.}

\item{FUN}{a function to be applied iteratively over the input arguments. similar to FUN in lapply.}

\item{writedisk}{TRUE/FALSE indicating whether output of application of FUN to elements of X should be written to disk as serialized representions in RDS files before it gets overwritten in memory. Will also write the final output to disk.}

\item{outdir}{output directory where .RDS files should be written.}

\item{name}{When writedisk = TRUE, output is saved with this name followed by the iteration. By default, uses getCall2(), thus RDS files are "iapply" followed by the number of th element. However storing iapply in another object, will change the output name.}

\item{compress}{a logical specifying whether outputted RDS files use "gzip" compress, or one of "gzip", "bzip2" or "xz" to indicate the type of compress to be used. Defaults to no compress.
You can later restore the objects using readRDS().}
}
\value{
the output of FUN(X[X[length(X)]], plus any prints, messages, warnings, errors that lapply(X, FUN) would produce. If FUN writes results to a disk, these files will be created too.
}
\description{
Similar to well-know lapply, but returns only the last result as an object. Very usefull when working with big datasets. If the object to be returned is large in memory, say it contains copies of a large dataset,
iapply iteratively applies a function but returns only the last output of the function application. This is useful if the function prints diagnostics to screen or saves results in a log file,
but you would like to evaluate the last object to understand the output better. Additionally, the function allows you to write all the output to disk using a Serialization Interface for Single Objects.
This allows you to restore any output to an object, possibly with a different name.
}
\examples{
f <- function(x){
print (x)
return(x)
}
x=1:10
# this will print all elements of x, finaloutput will only contain the last element of x.
finaloutput <- iapply (x, f)
print(finaloutput)

# this will print all elements of x, create 10 RDS files that you can use to restore f(x),
# finaloutput will contain the last element of x.
finaloutput <- iapply (x, f, writedisk = TRUE, outdir = "C:\\\\Users\\\\")
print(finaloutput)

# this will do the same, but stores names as "binomials1", "binomials2" etc.
binomials <- iapply
finaloutput <- binomials (x, f, writedisk = TRUE, outdir = "C:\\\\Users\\\\")
print(finaloutput)
}

