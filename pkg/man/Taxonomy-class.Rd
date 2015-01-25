\encoding{UTF-8}

\name{Taxonomy-class}

\Rdversion{1.1}
\docType{class}
\alias{taxonomy}

\alias{Taxonomy-class}

\alias{nrow,Taxonomy-method}

\alias{[,Taxonomy,ANY,ANY,missing-method}

\alias{$,Taxonomy-method}

\alias{bind,Taxonomy-method}

\alias{taxonomy}
\alias{taxonomy,Vegsoup-method}
\alias{taxonomy,character-method}
\alias{taxonomy,data.frame-method}
\alias{taxonomy,matrix-method}
\alias{taxonomy,Taxonomy-method}

\alias{taxonomy<-,Vegsoup,SpeciesTaxonomy-method}

\title{Class \code{"Taxonomy"}}
\description{
Data representation of taxonomic reference list.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Taxonomy", ...)},
  where \code{...} expands to slot \code{'data'}. Using the \code{taxonomy}
  method with appropriate objects is also fast and convenient
  (see \sQuote{Methods}).

  \itemize{
	\item \code{abbr} contains unique abbreviations of a scientific latin name
	  that translate those of class \code{\linkS4class{Species}}.
	\item \code{taxon} gives the full scientific latin name.
  }
}
\section{Slots}{
  \describe{
	\item{\code{data}:}{Object of class \code{"data.frame"} with at least
	  two columns named \code{'abbr'} and \code{'taxon'}. }
  }
}
\section{Methods}{
  \describe{
	\item{ taxonomy }{
	  \code{signature(obj = "Taxonomy")}:
		accessor function (\emph{slot "data"}).
	  
	  \code{signature(obj = "SpeciesTaxonomy")}:
		accessor function for objects of class \code{SpeciesTaxonomy}.
	  
	  \code{signature(obj = "data.frame")}:
		promote \code{data.frame} to object of class \code{"Taxomomy"}.

	  \code{signature(obj = "matrix")}:
		promote \code{matrix} to object of class \code{"Taxomomy"}.
		Note, \code{obj} does not need to have \code{dimnames}.

	  \code{signature(obj = "character")}:
		supply a file to be read by \code{read.csv} and promote that object to
		class \code{"Species"}. Remember to pass appropriate arguments to
		\code{\link{read.csv}} that meet the specifications of your file.
	}

	\item{ nrow }{
	  \code{signature(x = "Taxonomy")}:
		the number of rows is the number of taxa.
	}

	\item{ [ }{
	  \code{signature(x = "Taxonomy")}:
		Subset parts of an object. Note, indexing that will
		evaluate to an object with less than two columns will fail,
		due to the class inherent data model (see \sQuote{Slots}).
	}

	\item{ bind }{
	  \code{signature(x = "Taxonomy")}:
		\code{bind} method to combine two or more objects. If there are any
		duplicates in the \code{'abbr'} key column only one instance is kept,
		the first one!
	}

  }
}
\author{ Roland Kaiser }
\seealso{
  \code{\linkS4class{SpeciesTaxonomy}}
  and function \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Species}},
  \code{\linkS4class{Sites}}
  \code{\link[vegdata:tax]{tax}} in package \pkg{vegdata}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein
# from demonstration extract from object
x <- taxonomy(taxonomy(x)) # returns object of class 'data.frame'

t <- new("Taxonomy", data = x)

# taxonomy method for class data.frame
( t <- taxonomy(x) )
# or matrix
( t <- taxonomy(as.matrix(x)) )

#	get slot data from object
taxonomy(t)			   # returns object of class 'data.frame'

# combine objects
data(windsfeld)
x <- windsfeld
t2 <- taxonomy(x)

bind(t, t2)
}
\keyword{ classes }

