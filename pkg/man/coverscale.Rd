\name{coverscale}

\alias{Coverscale}

\alias{coverscale}
\alias{coverscale,Vegsoup-method}

\alias{coverscale<-}
\alias{coverscale<-,Vegsoup-method}
\alias{coverscale<-,Vegsoup,character-method}
\alias{coverscale<-,Vegsoup,Coverscale-method}

\alias{is.ordinal}
\alias{is.ordinal,Coverscale-method}
\alias{is.ordinal,Vegsoup-method}

\alias{is.continuous}
\alias{is.continuous,Coverscale-method}
\alias{is.continuous,Vegsoup-method}


\title{ Set or Retrieve an Abundance Scale for \code{Vegsoup*} Objects }

\description{
  Function \code{Coverscale} creates an object of class
  \code{\link[vegsoup:Coverscale-class]{Coverscale}} containing a definition
  for an ordinal scale, or alternatively setting up a \code{Coverscale} object
  representing species abundances on a continuous scale (counts, frequencies
  or percentages). Objects of this class are mandatory for \code{Vegsoup} and
  it's extending classes.
}

\usage{
Coverscale(name, codes, lims)

\S4method{coverscale}{Vegsoup}(x)

\S4method{coverscale}{Vegsoup,Coverscale}(x) <- value

\S4method{coverscale}{Vegsoup,character}(x) <- value

\S4method{is.ordinal}{Vegsoup}(x)

\S4method{is.continuous}{Vegsoup}(x)
}

\arguments{
  \item{ name }{
    character. Giving a name of a built in abundance scale, or a name to be
    referred to a user supplied definition.
  }
  \item{ codes }{
    character. Vector assigning the level codes (ordinal scales).
  }
  \item{ lims }{
    numeric. Vector defining the mean percentage cover for each entry
    in \code{codes}.
  }
  \item{ x }{
    A \code{Vegsoup*} object.
  }
  \item{ value }{
    A \code{Coverscale} object or a \code{character} specifying a coverscale as accepted by
	argument name of function \code{Coverscale}.
  }
}

\details{
The constructor Function \code{Coverscale} creates an object of class \code{"Coverscale"}, whereas method \code{coverscale} (lower case letters!) returns the abundance scale set up for an \code{Vegsoup*} object. The corresponding replacement method (\code{"coverscale<-"}) can be used to assign a different scale for an object where it has already been set. Most of the time it is convenient to use one of the built in definitions. Call \code{Coverscale()} without arguments to see which ones are available. Note, a continuous scale can simply be defined by having a value of \code{NULL} for slots \code{'codes'} and \code{'lims'} (cf. \code{Coverscale("percentage")}). %If the later are defined abundance values are broken into intervals and the original data is modified.

The methods \code{is.continuous} and \code{is.ordinal} test objects of class \code{"Coverscale"} if they conform to the given type of cover scale. Note, this is just a simple test if slots \code{'codes'} and \code{'lims'} are \code{NULL}. If so, the coverscale is treated as continuous, otherwise as ordinal. There are methods defined for \code{Vegsoup*} object (see \sQuote{examples}).

The replacement function can also be used to transform a continuous scale to an ordinal one. This functionality is restricted to situations where \code{is.continous(x)} evaluates to \code{TRUE} and the \code{"Coverscale"} object that is assigned (argument \code{value}) can be treated as ordinal (\code{is.ordinal(value)} evaluates to \code{TRUE}). When doing so, the cover values stored in the \code{Vegsoup*} object are transformed (the original data is lost) and a message is issued. Such a type of transformation makes sense, for example, when an output of function \code{link{write.verbatim}} is intended.
}

\value{
Function \code{Coverscale} returns an object of class \code{"Coverscale"}.

Method \code{coverscale} modifies a \code{Vegsoup*}.
}

\references{
There are many textbooks dealing with coverscale. A recent treatment is found in:

Peet, R.K. & Roberts D.W. (2013). Classification of natural and semi-natural vegetation. In Vegetation Ecology (van der Maarel, E. & Franklin, J. eds). Wiley-Blackwell.

Currall, J. (1987). A transformation of the Domin scale. Vegetatio, 72(2):81–87.

%Pfadenhauer, J., Poschlod, P., and Buchwald, R. (1986). Überlegungen zu einem Konzept geobotanischer Dauerbeobachtungsflächen für bayern, Teil I. Berichte der ANL, 10:41–60.
}

\author{ Roland Kaiser }

\seealso{
  \code{\link{Coverscale-class}},
  \code{\link{BraunBlanquetReduce}},
  \code{\link{Vegsoup}} in package \pkg{vegsoup},
  \code{\link[vegdata:tv.coverperc]{tv.coverperc}} in package \pkg{vegdata} and
  \code{\link[vegan:coverscale]{coverscale}} in package \pkg{vegan} for the
  ancestor of this implementation.
}

\examples{
# shows the built in cover scales provided with package vegsoup
Coverscale()

# creates an object of class Coverscale by selecting a built in definition
Coverscale("braun.blanquet")

# define your own ordianl scale
Coverscale("myscale", codes = LETTERS[1:5], lims = seq(20, 100, 20))

# define your own continuous scale (e.g. counts)
Coverscale("myscale.continuous", NULL, NULL)

# using the coerce method for class Coverscale
as(list("myscale", LETTERS[1:5], seq(20, 100, 20)), "Coverscale")

# test vegsoup objects
data(barmstein)
barmstein
is.ordinal(barmstein)
coverscale(barmstein)

# transform coverscale
sim1 <- sim2 <- coenoflex(100,100)
is.continuous(sim1)
coverscale(sim1) <- "ordinal"
is.ordinal(sim1)

# coverscales are inhertited
sim1.prt <- VegsoupPartition(sim1, k = 2)
coverscale(sim1.prt)

sim1.prt.fid <- Fidelity(sim1.prt)
coverscale(sim1.prt.fid)
}

\keyword{ methods }
