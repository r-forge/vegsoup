\name{import}
\alias{stack.species}
\title{
Stacks Species Matrices in Wide format
}
\description{
Converts input object and derives an object of class \code{Species}.
}
\usage{
stack.species(obj, file = TRUE, csv2 = TRUE, schema = c("abbr", "layer", "comment"),
              absence = ".", verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ An object converted to data frame}
\item{file}{ Path to a csv-file}
\item{csv2}{ Use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
\item{schema}{ Names of columns to be searched for. See \squote(Details)}
\item{absence}{ Character used to code absences. See \squote(Details)}
\item{verbose}{ if \code{TRUE} prints some diagnostic messages}   
}
\details{
The supported data frame, either read from file, or passed as \code{R} object must have columns corresponding to argument schema. The default schema is 'abbr', 'layer' and 'comment' (see class \code{\link{class?Species}}). The column 'comment' can have \code{NA} or \code{NULL}. This field can be used to store taxonomic or other relvant information (accuracy of determination) on a particular observation, but is not mandatory. The species matrix is assumed to have species in rows and plots in columns. Plot names are derived form \code{names(x)}. If argument file is supplied, all columns are imported using \code{(read.csv(..., colClasses = "character")}. This matters if species abundances were recorded on a non-ordinal scale and schoud be kept as numeric. Internal checking of conversion is performed using \code{type.convert} to test for appropriate mode of the spcies abundances vector; setting \code{verbose = TRUE} will report results of this test.
}
\value{
\code{stack.species} returns a five-column data frame; the first column has the plot names (\code{plot}), the second species abbreviations (\code{abbr}), the third the respective layer (\code{layer}), the fourth the abundance values (\code{cove}) and the last columns represents a comment field.
}

\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
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

res <- stack.species(x, absences = "0", verbose = TRUE)

}

\seealso{
\code{\link{Vegsoup-class}},
\code{\link{SitesWide2SitesLong}} which performs something similar for sites data,
\code{\link{read.csv}} in package \pkg{utils}.
}

\keyword{import}

