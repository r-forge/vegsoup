\name{Extract}
%% was $-methods
%%\docType{methods}
\alias{Extract}
\alias{$-methods}
\alias{$,VegsoupData-method}
\title{Extract or Replace Parts of the Sites Data Slot of Objects inheriting VegsoupData}
\description{
Extract or replace parts of the sites data.
}

\usage{
x[i, j, ... , drop = FALSE]

x$name
x$i <– value
}
\arguments{

\item{ x }{
An object ingeriting from class \code{VegsoupData}.
}                                                 

\item{ i, j }{
Indices specifying elements to extract or replace.
}                                                 
\item{name}{
A character string or a name.
}
}
\details{
The \code{"$"} operator can be used to access columns of the sites slot which is of class \code{data.frame} and has also functions to get or set the names of the object. Indexing of the species matrix can be done using indices specifying elements to extract. See \sQuote{Examples}.

If only one plot is requested for indexing (e.g. \code{obj[1,]}) the \code{summary} and \code{show} methods additionally will print a species list.
}
\note{
When objects of class \code{"VegsoupDataPartition"} are subsetted it is likely that the partitioning vector (\code{Partitioning(obj)}) has to be subsetted too. If this is the case, the number of partitions \code{"k"} (\code{getK(obj)}) is changed (lowered) accordingly.
}
\examples{
	qry <- Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet"))
	dta <- VegsoupData(qry)
	dta$rock
    
	#	summary returns a species list
	dta[1,]
	(dta$rock <- sqrt(dta$rock))
#	do.call("$", list(dta, "variable"))   
} 

%%}}
