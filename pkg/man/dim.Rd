\name{dim}
\alias{dim}

\title{
Dimensions of Species Matrix
}
\description{
Retrieves the dimension of the species matrix of an object inheriting from class \code{VegsoupData}.
}
\usage{
dim(x)
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of mode \code{\link{integer}} giving number of row (plots) and species (each occurrence of a species in layer). 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
dim(dta)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

