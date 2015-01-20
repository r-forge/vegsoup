\encoding{UTF-8}

\name{Sites-class}
\Rdversion{1.1}
\docType{class}

\alias{sites}

\alias{Sites-class}

\alias{nrow,Sites-method}

\alias{[,Sites,ANY,ANY,missing-method}

\alias{$,Sites-method}
\alias{$<-,Sites-method}

\alias{variable}
\alias{variable,Sites-method}

\alias{variable<-}
\alias{variable<-,Sites,character,ANY-method}

\alias{variables}
\alias{variables,Sites-method}

\alias{rbind,Sites-method}

\alias{sites,Sites-method}

\alias{sites-method}
\alias{sites,character-method}
\alias{sites,data.frame-method}
\alias{sites,matrix-method}
\alias{sites,Sites-method}
\alias{sites,rbind-method}

\title{ Sites Class }

\description{
  Data representation of sites data in \emph{long format} where
  each observation on a particular plot (environmental variables,
  stand parameters) is represented by one row. Note, the initialize method
  of the class performs ordering first by plot and then by variable.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Sites", ...)},
  where \code{...} expands to slot \code{'data'}. Using the
  \code{sites} method with appropriate objects is also convenient
  (see \sQuote{Methods}). Function \code{\link{stackSites}} is usefull
  when the data is in \emph{wide} format (plot by variable matrix).
}

\section{Slots}{
  \describe{
  The \code{data} slot holds a \code{"data.frame"} with three columns,
  all of type \code{character} and with names \code{plot}, \code{variable}
  and \code{value}. The meaning of these columns is as detailed below:

  \itemize{
    \item \code{plot} contains the plot identifier associated with each
      observation (cf. \code{\linkS4class{Species}}).
    \item \code{variable} contains a name for the measured variable.
    \item \code{value} holds the value of a particular variable.
  }  
  \item{\code{data}:}{Object of class \code{"data.frame"}
  with three columns named
  \code{'plot'}, \code{'variable'} and \code{'value'}. }
  }
}

\section{Methods}{
  \describe{
    \item{ sites }{
      There exists \code{species} methods for a number of R base classes:

      \code{signature(obj = "data.frame")}:
        promote \code{data.frame} to object of class \code{"Sites"}

      \code{signature(obj = "matrix")}:
        promote \code{matrix} to object of class \code{"Sites"}
        Note, \code{obj} does not need to have \code{dimnames}.

      \code{signature(obj = "character")}:
        supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Sites"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file
        (e.g. argument \code{sep}).

      Only the Sites class has a \code{sites} method so far.

      \code{signature(obj = "Sites")}:
        accessor function (\emph{slot "data"}).
    }

    \item{ $ }{
      \code{signature(x = "Sites")}:
        retrieve column by name (see \sQuote{Slots}).
    }
    
	\item{ [ }{
      \code{signature(x = "Sites")}:
        subset parts of an object. Note, only subsetting by rows
		is currently implemented due to the class inherent data model
		(see \sQuote{Slots}).
    }
	
    \item{ variable }{
      \code{signature(x = "Sites")}:
        retrieves the values for a given variable. The method returns \code{NA}
		for plots missing that variable. If the variable is not present, the
		method returns \code{NULL}.	The class stores the values for each
		variable as mode \code{character}. Therefore, you may want to use
		\code{type.convert(variable(obj, name))}.
    }
 
 \item{ variable<- }{
   \code{signature(x = "Sites")}:
     retrieves the values for a given variable. The method returns \code{NA}
	for plots missing that variable. If the variable is not present, the
	method returns \code{NULL}.	The class stores the values for each
	variable as mode \code{character}. Therefore, you may want to use
	\code{type.convert(variable(obj, name))}.
 }
    
	\item{ variables }{
      \code{signature(x = "Sites")}:
        retrieves the unique set of variables as a character vector.
		Useful for inspectino of the objects contents.
    }
			
    \item{ nrow }{
      \code{signature(x = "Sites")}:
        the number of rows. Each plot and variable combination constitutes a row.
    }

    \item{ rbind }{
      \code{signature(x = "Sites")}:
        \code{rbind} method to combine two or more objects.
    }

    \item{ coordinates }{
      \code{signature(x = "Sites")}:
        method to retrieve matrix of coordinates from object.
        The method assumes to find values for variables \code{longitude} and
        \code{latitude}. If not present a random pattern is returned.
    }
  }
}

\author{ Roland Kaiser }

\seealso{
  \code{\link{stackSites}}
  \code{\linkS4class{Species}}
  \code{\linkS4class{Vegsoup}}
}

\examples{
require(vegsoup)
showClass("Sites")

data(barmstein)
x <- barmstein

# extract from Vegsoup object for demonstration
# get sites slot, it is returned in wide format
s <- Sites(barmstein)

# add plot names
s <- cbind(plot = rownames(x), s)
class(s)

#	promote to class Sites
s <- stackSites(s)

# get slot 'data' from object of class 'Sites'
s.df <- sites(s)
class(s.df)

# matrix method 
s.m <- as.matrix(s.df)
s <- sites(s.m)

# combine objects
data(windsfeld)

s2 <- stackSites(cbind(plot = rownames(windsfeld), Sites(windsfeld)))

rbind(s, s2)


}

\keyword{ classes }

