\name{Taxonomy-class}
\Rdversion{1.1}
\docType{class}
\alias{Taxonomy-class}
\alias{taxonomy,Taxonomy-method}
\alias{SpeciesTaxonomy-class}



\title{Class \code{"Taxonomy"}}
\description{
Data representation of taxonomic reference list.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Taxonomy", ...)}. .
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} with at least two columns named exactly \code{'abbr'} and \code{'taxon'}. }
  }
}
\section{Methods}{
  \describe{
    \item{taxonomy}{\code{signature(obj = "Taxonomy")}: accessor function for slot \code{obj@data} }
		\item{\code{signature(obj = "data.frame")}}{
	Method to create object from class \code{data.frame}
		}
		\item{\code{signature(obj = "matrix")}}{
	Method to create object from class \code{matrix}. \code{obj} does not need to have \code{dimnames}.
		}	
	}
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Taxonomy")
}
\keyword{classes}
