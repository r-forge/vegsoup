\name{SitesWide2SitesLong}
\alias{SitesWide2SitesLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stacks data frames for sites in wide format 
}
\description{
Stack a plot by variable matrix to long format, with columns 'plot', 'variable' and 'value'.
}
\usage{
SitesWide2SitesLong(obj, file, csv2 = TRUE, verbose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ a data frame}
\item{file}{ path to a csv-file stored on disk}
\item{csv2}{ use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
\item{verbose}{ if \code{TRUE} prints \code{unique{variable}}}   
}
\details{
The supported data frame, either read from file, or passed as \code{R} object must have a column named exactly named as 'plot'. 
}
\value{
\code{SitesWide2SitesLong} returns a four-column data.frame; the first column has the plot names (\code{plot}), the second the measured variable (\code{variable}), the third the value of the respective variable (\code{value}). Column 'value' is coerced to \code{as.character}.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoup-class},
\link{SpeciesWide2SpeciesLong} which performs something similar for sites data,
\link{read.csv} in package \code{stills}.
}
\examples{
library(vegan)
data(dune.env)

x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")

res <- SitesWide2SitesLong(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{import}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
