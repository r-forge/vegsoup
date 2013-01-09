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
x$name
x$i <– value
}
\arguments{

\item{x}{
An object ingeriting from class \code{VegsoupData}.
}                                                 

\item{i}{
Indices specifying elements to extract or replace.
}                                                 
\item{name}{
A character string or a name.
}
}
\details{
The "$" operator can be used to access columns of the sites slot which is of class \code{data.frame} and has also functions to get or set the names of the object. Indexing of the species matrix can be done using indices specifying elements to extract. See examples.
}
\examples{
	qry <- Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet"))
	dta <- VegsoupData(qry)
	dta$rock
	
	(dta$rock <- sqrt(dta$rock))
#	do.call("$", list(dta, "variable"))   
} 

%%}}
