\name{SpeciesWide2SpeciesLong}
\alias{SpeciesWide2SpeciesLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stacks data frames for species in wide format 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SpeciesWide2SpeciesLong(obj, file = TRUE, csv2 = TRUE, vervose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ a data frame}
\item{file}{ path to a csv-file}
\item{csv2}{ use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
\item{verbose}{ if \code{TRUE} prints \code{table{cov}} and \code{table{layer}}}   
}
\details{
The supported data frame, either read from file, or passed as \code{R} object must have columns exactly named as 'abbr', 'layer' and 'comment'.
The column 'comment' can have \code{NA} or \code{NULL}. This field is used to store taxonomic or other relvant information on a particular observation. The species matrix is assumed to have species in rows and plots in columns. If argument file is supplied, all columns are imported using \code{(read.csv(…, colClasses = "character")}.
}
\value{
\code{SpeciesWide2SpeciesLong} returns a five-column data frame; the first column has the plot names (\code{plot}), the second species abbreviations (\code{abbr}), the third the respective layer (\code{layer}), the fourth the abundance values (\code{cove}) and the last columns represents a comment field.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}
\note{
%%  ~~further notes~~
}

\examples{
library(vegan)
data(dune)
#	there are two moss species in the dune data set
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groom plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

res <- SpeciesWide2SpeciesLong(x)

}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Vegsoup-class}},
\code{\link{SitesWide2SitesLong}} which performs something similar for sites data,
\code{\link{read.csv}} in package \pkg{utils}.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{import}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
