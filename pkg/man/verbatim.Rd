\name{verbatim}
\alias{verbatim}
\alias{verbatim.append}
\alias{verbatim.append}
\title{
Read Monospaced Community Tables From Text Files
}
\description{
This function tries to read data from text files as frequntly encountered in published tables typesetted with monospaced fonts. It's main purpose is to help digitizing literture data.
}

\usage{
  verbatim(file, colnames)
  
  verbatim.append(x, file, abundance = "+")
}

\arguments{
\item{file}{
A character string. A path to a text file.
}
\item{colnames}{
A character string to be searched for in the header section and used to assign plot names. See \sQuote{Details}.
}
\item{x}{
An object of class \code{VegsoupVerbatim} as created by calls to function \code{verbatim}
}
\item{abundance}{
A vector to be coerced to \code{as.character}. Defaults to \code{"+"} and applied to all observations.
}
}

\details{
The data format of the input file equals in its fundamentals the output format of traditional software such as \sQuote{TWINSPAN} and users are assumed to be familiar with such data structeres. Also editing plain text files where matrix like data structures are achieved using monospaced fonts is presupposed. In order to create a fully valid input file a text editor allowing hidden characters to be displayed is essential. At least the editor must be capable of displaying line end special characters (see below).

In general, the data layout consits of species abundances measured on plots where species performance is coded nothing but as a single character. Species or varibales are in rows and plots are columns. As a consequence, using a monospaced font each observation on a plot aligns to a column composed of single characters. Only in the header section (see 1. below) values of width longer than one are supported. Adopting the previous logic, these figures have to be aligned vertically. For example, a value of \sQuote{1000} needs for lines, one line for each digit. See \sQuote{Examples} for a valid input file.

The function \code{verbatim()} makes the following additional assumptions about the input file format.

\enumerate{
\item The table header and table body must be enclosed within a pair of keywords. Lines giving the keywords \sQuote{BEGIN HEAD} and \sQuote{END HEAD} must be present at the beginning and end, respectively, of the table header. This block of data has to followd by \sQuote{'BEGIN TABLE'} and \sQuote{END TABLE} to identify the main table structure.
\item The data blocks (header and table) can have empty lines and/or columns of spaces seperating plots.
\item The width of the table (number of monospaced font characters) should align at right side. It is crucial to ensure that all line end charcters align vertically!
\item All species absences have to be coded with a dot (\sQuote{.}). This is the de-facto standard in published tables.
\item Given that absences are given as \sQuote{.} each cell of the community table has a value. This ensures that the left margin of the community table can be filtered automatically.
\item The width (number of characters) of the header and the table body data blocks has to perfectly match. The values found in the header (possibly of length longer than one and aligned vertically) exactly corresponds the species abundances on the same plot. In other words, they align to the same column in a monospaced font layout.
}
Plot identifiers can be assigned using one of the attributes on the resulting object or by supplying an argument \code{colnames}.

It is often the case that rare species are only given in table footer but not in the main table. The function \code{verbatim.append()} takes an object created by calls to \code{verbatim()} and adds species given in argument \code{file}.

The function \code{verbatim.append()} requires a simple text file, where each row corresponds to a unique species, follwod by a colon (\sQuote{:}) and subsequent strings matching \code{colnames(x)} and seperated with commas   (\sQuote{,}). All spaces found after the colon will be discared. See \sQuote{Examples} for a valid input file.
}

\value{
An S3 object of class \code{VegsoupVerbatim}. Basically a matrix of mode \code{character} with attributes giving the data in the header section of the input file. Species are rownames. If argument \code{colnames} is supplied the returned matrix will hold meaningful \code{colnames}.
}

\note{

The print method for objects of class \code{VegsoupVerbatim} uses \code{as.data.frame(x)} as a means to get rid of quotes and to provide clean screen output
}
\author{
Roland Kaiser
}
\references{
The demonstration data set used in the example is taken from:

Erschbamer, B. (1992). Zwei neue Gesellschaften mit Krummseggen (Carex curvula ssp. rosae, Carex curvula ssp. curvula) aus den Alpen - ein Beitrag zur Klärung eines alten ökologischen Rätsels. Phytocoenologia, 21:91–116.

The are three text files. \code{"Erschbammer1992Tab4.txt"} has the main table. Species names were taxonomically interpretetd to a match a reference list.   \code{"Erschbammer1992Tab4Tablefooter.txt"} has a list of rare species, those with frequnency lower than 4. \code{"Erschbammer1992Tab4Locations.txt"} has geographic coordinates interpreted from the appendix in the data source.
}
\examples{
library(vegsoup)
require(stringr)

file <- system.file("extdata", "Erschbammer1992Tab4.txt",
	package = "vegsoup")
x <- verbatim(file)
class(x)
head(x)

# assign plot ids using attributes
colnames(x) <- attributes(x)$"Aufnahme Nr." #German for relevee number
head(x)  
dim(x)

# or in one step
x <- verbatim(file, "Aufnahme Nr.")
head(x)

# add species from table footer
file <- system.file("extdata", "Erschbammer1992Tab4Tablefooter.txt",
package = "vegsoup")
x <- verbatim.append(x, file)
tail(x)
# 68 species were added
dim(x)

# turn into long format suitable as argument 'x' in function Vegsoup()
x.df <- data.frame(abbr = rownames(x), layer = "hl", comment = NA, x,
	check.names = FALSE)
X <- SpeciesWide2SpeciesLong(x.df)
head(X)

#	sites data also including coordinates
file <- system.file("extdata", "Erschbammer1992Tab4Locations.txt",
package = "vegsoup")
Y <- read.delim(file, colClasses = "character")
names(Y)[1] <- "plot"

Y <- SitesWide2SitesLong(Y)

# taxonomy reference list
file <- system.file("extdata", "Erschbammer1992Taxonomy.txt",
package = "vegsoup")
Z <- read.delim(file, colClasses = "character")

# groome abundance scale codes to fit the standard
X$cov <- gsub("m", "2m", X$cov)
X$cov <- gsub("a", "2a", X$cov)
X$cov <- gsub("b", "2b", X$cov)

dta <- VegsoupData(Vegsoup(X, Y, Z))

# assign header data stored as attributes in
# imported original community table
# omit dimnames, class and plot id
df.attr <- as.data.frame(attributes(x)[-c(1:4)])
rownames(df.attr) <- colnames(x)
# reorder by plot
df.attr <- df.attr[match(rownames(dta), rownames(df.attr)), ] 

#	give name and assign
dta$block <- df.attr$Block
dta$altitude <- df.attr$Seehöhe
dta$expo <- df.attr$Exposition
dta$cov <- df.attr$"Deckung...."
dta$ph <- df.attr$"ph...10.2."

names(Sites(dta))

# see if we can reproduce the grouping in the original table
prt.flx <- VegsoupDataPartition(dta, 3, "wards")
prt.org <- VegsoupDataPartition(dta, 3, clustering = "block")

# block/group 3 is ambigously assigned
Confus(prt.flx, prt.org)
}

\seealso{
\code{\link{SpeciesWide2SpeciesLong}}, 
}
\keyword{import}
