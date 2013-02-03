\name{stack}
\alias{stack.species}
\alias{stack.sites}
\title{
Stack Species Matrix or Sites Dataframe
}
\description{
Function \code{stack.species} converts a matrix like input object or file into a representation in long format, where each observation on a species consits of one row in a resulting data frame that has as many rows as there are single species occurences in the data set. In a similar fashion, function \code{stack.sites} can be used to derive a plot by variable matrix in long format, with columns 'plot', 'variable' and 'value'.
}
\usage{
stack.species(x, file, csv2 = TRUE, schema = c("abbr", "layer", "comment"),
              absence, verbose = FALSE)

stack.sites(x, file, csv2 = TRUE, schema = "plot", verbose = FALSE)               
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ An object converted to a data frame. }
\item{file}{ Path to a csv-file}
\item{csv2}{ Use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}. }
\item{schema}{ Names of columns to be searched for. See \sQuote{Details}. }
\item{absence}{ Character used to code absences. See \sQuote{Details}. }
\item{verbose}{ If \code{TRUE} prints some diagnostic messages. }   
}
\details{

For \code{stack.species} the supported data frame, either read from file, or passed as \code{R} object must have columns corresponding to argument schema. The default schema is 'abbr', 'layer' and 'comment' (see class \code{\link{class?Species}}). The column 'comment' can have \code{NA} or \code{NULL}. This field can be used to store taxonomic or other relvant information (accuracy of determination) on a particular observation, but is not mandatory. The species matrix is assumed to have species in rows and plots in columns. Plot names are derived form \code{names(x)}. If argument file is supplied, all columns are imported using \code{read.csv(..., colClasses = "character")}. This matters if species abundances were recorded on a non-ordinal scale and schoud be kept as numeric. Internal checking of conversion is performed using \code{type.convert} to test for appropriate mode of the species abundances vector; setting \code{verbose = TRUE} will report results of this test. If argument \code{absences} is missing an almost save guess is obtained from the data and derived as the most frequent value in the species data matrix. These are usually the species absences.
\cr
For \code{stack.sites} theres are no speciefic requirements about the form of the object, except that one column must be specified which will then be used as the plot identifier. The default is to search for a column named \code{'plot'}, but other columns can be specified by argument \code{schema}. 
}
\value{
\code{stack.species} returns a five-column data frame; the first column has the plot names (\code{'plot'}), the second species abbreviations (\code{'abbr'}), the third the respective layer (\code{'layer'}), the fourth the abundance values (\code{'cov'}) and the last columns represents a comment field (\code{'comment'}).
\cr
\code{stack.sites} returns a three-column data frame; the first column has the plot names (\code{'plot'}), the second the measured variable (\code{'variable'}), and the last the respective value (\code{'value'}) for each variable measured on each plot. Colomn 'value' is coerced to \code{as.character}.
}

\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}

\examples{
library(vegan)
data(dune)

## species matrix
#  there are two moss species in the dune data set
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groom plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

spc <- stack.species(x, verbose = TRUE)

## sites data frame
data(dune.env)
x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")


sts <- stack.sites(x, verbose = TRUE)

}

\seealso{
\code{\link{Vegsoup-class}},
\code{\link{SitesWide2SitesLong}} which performs something similar for sites data,
\code{\link{read.csv}} in package \pkg{utils}.
}

\keyword{import}

