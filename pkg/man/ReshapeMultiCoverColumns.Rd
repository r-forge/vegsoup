\name{ReshapeMultiCoverColumns}
\alias{ReshapeMultiCoverColumns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stacks data frames where abundance values for layers are in separate columns
}
\description{
Utility function to translate some commonly used data models to store species data.
}
\usage{
ReshapeMultiCoverColumns(x, file, layers, csv2 = TRUE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ a data frame}
\item{file}{ path to a csv-file}
\item{file}{ a string converted to \code{as.character} naming the columns in x respectively file to tabe observations for layers}
\item{csv2}{ use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
\item{verbose}{ if \code{TRUE} prints \code{names{x}} and \code{table{layer}}}   
}
\details{
Currently there is limited user interface to select.
}
\value{
\code{ReshapeMultiCoverColumns} returns a four-column \code{data.frame}. See \link{Vegsoup-class} or \link{SpeciesWide2SitesLong} for details.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoupa-class}, \link{SpeciesWide2SitesLong} which performs something similar for species data in wide format (species by plot matrix) ,
}

\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
