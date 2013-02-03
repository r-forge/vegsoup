\name{reshape}
\alias{reshape.species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape Already Stacked Species Matrix Where Abundance Values for Layers are in Separate Columns
}
\description{
Utility function to translate some commonly used data models to store species data.
}
\usage{
reshape.species(x, file, layers, csv2 = TRUE, verbose = TRUE)
}

\arguments{
\item{x}{ A data frame. }
\item{file}{ Path to a csv-file. }
\item{schema}{ Names of columns to be searched for. See \sQuote{Details}. }
\item{csv2}{ use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
\item{verbose}{ if \code{TRUE} prints diagnostic meassages. }   
}
\details{
Currently there is limited to only one case.
The values of \code{schema} are seeked in the column names of \code{x}, respectively, \code{file} to tabulate observations for layers. The first element idientifies plots, the second species abbreviations, and the remaining columns that contain observations per layer. 
}
\value{
Returns a four-column \code{data.frame}. See \link{Vegsoup-class} or \link{stack.species} for details.
}

\author{
Roland Kaiser
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoupa-class},
\link{stack.species} % something similar for species data in wide format (species by plot matrix) ,
}

\examples{
}
\keyword{ import }

