\name{head}
\alias{head}
\alias{tail}

\title{
Return the First or Last Part of the Species Matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
head(x, choice, mode, ...) 
tail(x, ...)
}
% n = 6L
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object inheriting from class \code{VegsoupData}.
}
  \item{choice}{
Display first row of species matrix (the default) or sites data frame.
}
  \item{mode}{
If choice is \dQuote{species} of which mode should the returned matrix be reported. If missing the presence/absence mode of the species matrix is printed. See \code{\link{as.binary}}, \code{\link{as.numeric}}, and \code{\link{as.character}}, respectively.
}
  \item{\dots}{
not used
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}

\seealso{
See \code{\link{head}} and \code{\link{tail}} in package \pkg{utils}.
}
\examples{
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))
head(dta)
}
\keyword{methods}

