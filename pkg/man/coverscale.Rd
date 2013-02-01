\name{coverscale}
\alias{Coverscale}

\alias{coverscale}
\alias{coverscale-methods}
\alias{coverscale<--methods}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define an Abundance Scale for Objects of Class Vegsoup*
}
\description{
Constructs an object of class \code{\link[vegsoup:Coverscale-class]{Coverscale}} containg a definition for an ordinal scale, or alternatively setting up a \code{Coverscale} object representing species abundances on a contious scale (counts, frequencies or percentages). Objects of this class are mandatory for all classes extending \code{Vegsoup}.
}
\usage{
Coverscale(name, codes, lims)

coverscale(obj)

coverscale(obj) <- value
}
\arguments{
  \item{name}{
A character giving a name of a built in abundance scale, or a name to be refered to a user supplied defition.
}
  \item{codes}{
A \code{character} vector giving the level codes (ordinal scales).
}
  \item{lims}{
A \code{numeric} vector defining the mean percentage cover for each entry in \code{codes}.
}
  \item{value}{
A \code{Vegsoup*} object.
}
  \item{value}{
An object of class \code{Coverscale}.
}
}
\details{
\code{Coverscale} creates an object of class \code{Coverscale}, whereas \code{coverscale} (lower case letters!) queries the abundance scale set up for an \code{Vegsoup*} object. The corresponding replacements method (\code{"coverscale<-"}) can be used to assign a different scale for an obejt weher it has already been defined. 
Most of the time it is convenient to use one of the built in definitions. Call \code{Coverscale()} without arguments to see which ones are available. Note a continous scale can be defined by not having a value of \code{NULL} for \code{codes} and \code{lims}. If the later are defined abundance values are  broken into intervals. In this case \code{lims} corresponds to classes upper limits.
}
\value{
An object of class \code{Coverscale}.
}
\references{
Shimwell, D.W. (1971). The Description and Classification of Vegetation. Sidgwick & Jackson.

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
