\name{testdata}
\alias{testdata}
\docType{data}
\title{Test data set with objects suitable for Vegsoup}
\description{
	Simple data structures for testing. A species and corresponding taxonomy data.frame plus a sites data.
}
\format{
The three objects \code{"species"}, \code{"sites"} and \code{"taxonomy"} each contain the following columns:
\describe{
species:
	\item{plot}{plot identifier, mode \code{"character"}}
	\item{abbr}{species names, mode \code{"character"}}
	\item{layer}{layer code, mode \code{"character"}}
	\item{cov}{species abundance, mode \code{"character"}}
sites:	
	\item{plot}{plot identifier, mode \code{"character"}}
	\item{variable}{name of the variable, mode \code{"character"}}
	\item{value}{value for each variable, mode \code{"character"}}
taxonomy:	
	\item{abbr}{species identifier, mode \code{"character"}}
	\item{taxon}{scientific species name, mode \code{"character"}}
}
}
\usage{
data(testdata)
}	
\examples{
data(testdata)
}