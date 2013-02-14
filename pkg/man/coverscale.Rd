\name{coverscale}
\alias{Coverscale}

\alias{coverscale}
\alias{coverscale-method}
\alias{coverscale<--method}
\alias{is.ordinal-method}
\alias{is.continuous-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define or Change an Abundance Scale for Objects of Class Vegsoup*
}
\description{
Constructs an object of class \code{\link[vegsoup:Coverscale-class]{Coverscale}} containing a definition for an ordinal scale, or alternatively setting up a \code{Coverscale} object representing species abundances on a continuous scale (counts, frequencies or percentages). Objects of this class are mandatory for all classes extending \code{Vegsoup}.
}
\usage{
## S3 function to construct an object of class \code{Coverscale}
Coverscale(name, codes, lims)

## S4 method for \code{Vegsoup*} objects
coverscale(obj)

## S4 replacement method for \code{Vegsoup*} objects
coverscale(obj) <- value

## S4 methods for \code{Coverscale} objects
is.ordinal(obj)

is.continuous(obj)
}
\arguments{
  \item{name}{
A character giving a name of a built in abundance scale, or a name to be referred to a user supplied definition.
}
  \item{codes}{
A \code{character} vector giving the level codes (ordinal scales).
}
  \item{lims}{
A \code{numeric} vector defining the mean percentage cover for each entry in \code{codes}.
}
  \item{obj}{
A \code{Vegsoup*} or \code{Coverscale} object.
}
  \item{value}{
An object of class \code{Coverscale}.
}
}
\details{
The constructor Function \code{Coverscale} creates an object of class \code{"Coverscale"}, whereas method \code{coverscale} (lower case letters!) returns the abundance scale set up for an \code{Vegsoup*} object. The corresponding replacements method (\code{"coverscale<-"}) can be used to assign a different scale for an object where it has already been defined. 
Most of the time it is convenient to use one of the built in definitions. Call \code{Coverscale()} without arguments to see which ones are available. Note, a continuous scale can simply be defined by having a value of \code{NULL} for slots \code{'codes'} and \code{'lims'} (cp. \code{Coverscale("percentage")}). %If the later are defined abundance values are broken into intervals and the original data is modified.

Methods \code{is.continuous} and \code{is.ordinal} test objects of class \code{"Coverscale"} if they conform to the given type of cover scale. Note, this is just a simple test if slots \code{'codes'} and \code{'lims'} are \code{NULL}. If so, the coverscale is treated as continuous, otherwise as ordinal.

The replacement function can also be used to transform a continuous scale to an ordinal one. This functionality is restricted to situations where \code{is.continous(obj)} evaluates to \code{TRUE} and the \code{"Coverscale"} object that is assigned (\code{value}) is ordinal (\code{is.ordinal(value)}). In doing so the cover values stored in the \code{Vegsoup*} object is transformed and a warning is issued. Such a type of transformation makes sense, for example, when an output of function \code{link{write.verbatim}} is intended.
}
\value{
An object of class \code{Coverscale}.
}
\references{
Peet, R.K. & Roberts D.W. (2013). Classification of natural and semi-natural vegetation. In Vegetation Ecology (van der Maarel, E. & Franklin, J. eds). Wiley-Blackwell.

Currall, J. (1987). A transformation of the Domin scale. Vegetatio, 72(2):81–87.

%Pfadenhauer, J., Poschlod, P., and Buchwald, R. (1986). Überlegungen zu einem Konzept geobotanischer Dauerbeobachtungsflächen für bayern, Teil I. Berichte der ANL, 10:41–60.
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Coverscale-class}},
\code{\link{BraunBlanquetReduce}},
\code{\link{Vegsoup}},
\code{\link{tv.coverperc}} in \pkg{vegdata} and
\code{\link[vegan:coverscale]{coverscale}} in \pkg{vegan}
}
\examples{
# shows the buit in cover scales provided with package vegsoup	
Coverscale()

# creates an object of class Coverscale by selecting a buit in definition
Coverscale("braun.blanquet")

# define your own ordianl scale
Coverscale("myscale", codes = LETTERS[1:5], lims = seq(20, 100, 20))

# define your own continous scale (e.g. counts)
Coverscale("counts", NULL, NULL)

#	using the coerce method for class Coverscale
as(list("myscale", LETTERS[1:5], seq(20, 100, 20)), "Coverscale")

data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
