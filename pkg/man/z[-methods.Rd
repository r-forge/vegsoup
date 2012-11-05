\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,VegsoupData,ANY,ANY,missing-method}
\alias{[,VegsoupDataPartition,ANY,ANY,missing-method}
\title{Methods for Function [}
\description{
Method to subset an object by indexing the species matrix.
}
%\details{

%}
\section{Methods}{
\describe{
Method to subset the object by indexing the species matrix. 
\item{\code{signature(x = "VegsoupData", i = "ANY", j = "ANY", drop = "missing")}}{

}

\item{\code{signature(x = "VegsoupDataPartition", i = "ANY", j = "ANY", drop = "missing")}}{
The method will raise a waring each time the subset operation data will loose a partion. Values of \code{getK(obj)} will be set to the next lowest number.
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
