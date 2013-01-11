\name{seriation}
\docType{methods}
\alias{seriation}
\alias{seriation-methods}


\title{ Modify an Objects Internal Order as to Reflect a Vegetation Table }
\description{
The method arranges the species matrix according to an ordination, cluster analysis or by species frequencies. The method modifies the supplied object as  to reflect the order of plots and/or species as returned by the selected method. Currently there is no way to pass down arguments to functions.
}
\usage{
seriation(object, method = c("dca", "hclust", "ward", "flexible", "pam", "packed"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object inheriting from class \code{VegsoupData}.
}
  \item{methods}{
methods applied species matrix to obtain new order of data set, defaults to \code{"dca"}. See \sQuote{Details}.
}
  \item{dist}{
A similarity/disimilarity measure for any method that demands an object of class \code{dist} as input, defaults to \code{"bray"}.
} 
  \item{\dots}{
Not implemented yet. See \sQuote{Note}.
}
}
\details{
The default method \code{"dca"} uses function \code{\link{decorana}} in \pkg{vegan} without any additional arguments. Method \code{\dQuote{hclust}} uses \code{\link{hclust}} with argument \code{method = "ward"}, \code{\dQuote{hclust}}, \code{"ward"} calls \code{\link{agnes}} with argument \code{method = "ward"}, and \code{"flexible"} invokes \code{agnes} with arguments \code{method = "flexible"} and \code{par.method = c(0.625, 0.625, 1 - 2 * 0.625, 0)}. \code{"pam"} applies \code{\link{pam}}. Finally, method \code{"packed"} simply performs ordering by row and col sums.

Ordering by explicit indexing is available by using the extract method \code{\link{'['}} for obejcts of class \code{Vegsoup}.
}
\value{
A object of class depending of the input class.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\note{
Currently only presence/absencse data is used (\code{as.logical(obj)}) and default options of all methods apply.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{VegsoupDataPartition}} and \code{\link{vegemite}} in package \pkg{vegan} for the ancestor of all vegetation table functions. \code{\link{ordtaxa}} for another implemantation
}

\examples{
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))	
seriation(dta)
seriation(dta, "packed")	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

