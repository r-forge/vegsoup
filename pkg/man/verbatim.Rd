\name{verbatim}
\alias{verbatim}
\alias{read.verbatim}
\alias{read.verbatim.append}
\alias{write.verbatim}
\alias{write.verbatim-method}


\title{
Read or Write Monospaced Community Tables From or To Plain Text Files
}
\description{
The base S3 function \code{read.verbatim(file)} reads data from text files as frequntly encountered in published tables typesetted with monospaced fonts. It's main purpose is to help digitizing literture data. The corresponding \code{write.verbatim(obj)} S4-method performs the reverse on objects inheriting from class \code{VegsoupData}.
}

\usage{
  read.verbatim(file, colnames, layers, replace = c("|", "-", "–", "_"),
           species.only = FALSE, verbose = TRUE)
  
  read.verbatim.append(x, file, abundance = "+")

  write.verbatim(obj, file, select, absence = ".", collapse = " ", pad = 1,
           abbreviate = TRUE, short.names = FALSE, add.lines = FALSE)
}

\arguments{
## Arguments for S3 function \code{read.verbatim}
\item{file}{
A character string. A path to a text file.
}
\item{colnames}{
A character string to be searched for in the header section and used to assign plot names. See \sQuote{Details}.
}
\item{layers}{
A named list giving start and end indices of to assign a layer to each species, or a vector of characters of length according to the number of species in the data set. See \sQuote{Examples}.
}
\item{replace}{
A vector of characters to be replaced by blanks. Note, both the hypen and dash charcters are defined in default set. See \sQuote{Details}.
}
\item{species.only}{
Just return a vector of species extrated from the data set.
}
\cr
## Arguments for S3 function \code{read.verbatim.append}
\item{x}{
An object of class \code{VegsoupVerbatim} as created by calls to function \code{read.verbatim}
}
\item{abundance}{
A vector to be coerced to \code{as.character}. Defaults to \code{"+"} and applied to all observations.
}
\cr
## Arguments for S4 method \code{write.verbatim}
\item{obj}{
An object of class \code{VegsoupData}.
}
\item{file}{
A file name to output results.
}
\item{select}{
Indices to columns in Sites(obj) to be incorporated in the header section. Beware, this only makes sense for numeric or short string variables (e.g. slope aspect)!
}
\item{absence}{
Character to code absences, defaults to \code{"."}.
}
\item{collapse}{
Character to be used as seperator in columns of abundances (plots).
}
\item{pad}{
Integer specifing the number of blanks to add to taxon names (right side) and layers string (both sides), defaults to 1.
}
\item{abbreviate}{
Truncate abundance values to width 1 using abbreviate(x, minlength = 1, strict = TRUE).
}
\item{short.names}{
Use taxon abbreviation instead of long scientific names. Dots are converted to blanks.
}
\item{add.lines}{
Add blank lines sperating header section.
}
\item{latex.input}{
Warp output in LaTex verbatim environment.
}
}

\details{
The data format of the input file equals in its fundamentals the output format of traditional software such as \sQuote{TWINSPAN} and users are assumed to be familiar with such data structeres. Also editing plain text files where matrix like data structures are achieved using monospaced fonts is presupposed. In order to create a fully valid input file a text editor allowing hidden characters to be displayed is essential. At least the editor must be capable of displaying line end special characters (see below).

In general, the data layout consits of species abundances measured on plots where species performance is coded nothing but as a single character. Species or varibales are in rows and plots are columns. As a consequence, using a monospaced font each observation on a plot aligns to a column composed of single characters. Only in the header section (see 1. below) values of width longer than one are supported. Adopting the previous logic, these figures have to be aligned vertically. For example, a value of \sQuote{1000} needs for lines, one line for each digit. See \sQuote{Examples} for a valid input file. 

Horizontal and vertical table rules are often found in printed tables. When digitizing such a table the it is often handy to use some special characters to align with the original table strokes. Because of their non informative properties these characters are discarded when parsing the input file and all characters supplied with \code{replace} are replaced with blanks. Care has to be taken with hypens and dashes (see defaults of \code{replace}). R treats the minus sign as a dash (\code{"\uad"}). Warning, problems might arise on non UTF-8 plattforms.

The S3 function \code{read.verbatim()} makes the following additional assumptions about the input file format (not a file format in the strict sense, but a set of conventions).

\enumerate{
\item The table header and table body must be enclosed within a pair of keywords. Lines giving the keywords \sQuote{BEGIN HEAD} and \sQuote{END HEAD} must be present at the beginning and end, respectively, of the table header. This block of data has to followd by \sQuote{'BEGIN TABLE'} and \sQuote{END TABLE} to identify the main table structure.
\item The data blocks (header and table) can have empty lines and/or columns of spaces seperating plots.
\item The width of the table (number of monospaced font characters) should align at right side. It is crucial to ensure that all line end charcters align vertically!
\item All species absences have to be coded with a dot (\sQuote{.}). This is the de-facto standard in published tables.
\item Given that absences are given as \sQuote{.} each cell of the community table has a value. This ensures that the left margin of the community table can be filtered automatically.
\item The width (number of characters) of the header and the table body data blocks has to perfectly match. The values found in the header (possibly of length longer than one and aligned vertically) exactly corresponds the species abundances on the same plot. In other words, they align to the same column in a monospaced font layout.
\item Tab characters (\code{"\t"}) are not allowed in the input file!
}

Plot identifiers can be assigned using one of the attributes on the resulting object or by supplying an argument \code{colnames}.

It is often the case that rare species are only given in table footer but not in the main table. The function \code{read.verbatim.append()} takes an object created by calls to \code{read.verbatim()} and adds species given in argument \code{file}.

The function \code{read.verbatim.append()} requires a simple text file, where each row corresponds to a unique species, followed by a colon (\sQuote{:}) and subsequent strings matching \code{colnames(x)} and seperated with commas   (\sQuote{,}). All spaces found after the colon will be discared. See \sQuote{Examples} for a valid input file.

Finally the S4 method \code{write.verbatim()} creates output honorig the set of definitions given above. See \sQuote{Arguments} for possible customization of the output file. Changes of the order
}

\value{
\code{read.verbatim} and \code{read.verbatim.append} return an S3 object of class \code{VegsoupVerbatim}. Basically a matrix of mode \code{character} with attributes giving the data in the header section of the input file. Species are rownames. If argument \code{colnames} is supplied the returned matrix will hold meaningful \code{colnames}.

\code{write.verbatim} writes a file to disk and invisibly returns the vector of characters written to the file.
}

\note{
It is hard to avoid typos when editing mono spaced table structes, especially, when the are a lot of rows and columns. The human eye easily gets impaired by the overwhelming number of values. The \code{read.verbatim} function will report any instance that is not valid and diagnsotic messages are printed to the console to aid the user in correcting the input file. The probability of typos increases if there are columns of spaces seperating columns of data.

The print method for objects of class \code{VegsoupVerbatim} uses \code{as.data.frame(x)} as a means to get rid of quotes and to provide clean screen output.
}

\author{
Roland Kaiser
}
\references{
The demonstration data set used in the example is taken from:

Erschbamer, B. (1992). Zwei neue Gesellschaften mit Krummseggen (Carex curvula ssp. rosae, Carex curvula ssp. curvula) aus den Alpen - ein Beitrag zur Klärung eines alten ökologischen Rätsels. Phytocoenologia, 21:91–116.

The are three text files. \code{"Erschbammer1992Tab4.txt"} has the main table. Species names were taxonomically interpretetd to a match a reference list supplied as \code{"Erschbammer1992Taxonomy.txt"}.   \code{"Erschbammer1992Tab4Tablefooter.txt"} has a list of rare species, those with frequnency lower than 4. \code{"Erschbammer1992Tab4Locations.txt"} has geographic coordinates interpreted from the appendix in the data source.
}
\examples{
library(vegsoup)
require(stringr)

file <- system.file("extdata", "Erschbammer1992Tab4.txt",
	package = "vegsoup")
x <- read.verbatim(file)
class(x)
head(x)

# assign plot ids using attributes
colnames(x) <- attributes(x)$"Aufnahme Nr." #German for relevee number
head(x)  
dim(x)

# or in one step
x <- read.verbatim(file, "Aufnahme Nr.")
head(x)

# add species from table footer
file <- system.file("extdata", "Erschbammer1992Tab4Tablefooter.txt",
package = "vegsoup")
x <- read.verbatim.append(x, file)
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

# give name and assign
dta$block <- df.attr$Block
dta$altitude <- df.attr$Seehöhe
dta$expo <- df.attr$Exposition
dta$cov <- df.attr$"Deckung...."
dta$ph <- df.attr$"ph...10.2."

names(Sites(dta))

#	plot of the dissimilarity matrix
coldiss(dta, diag = T)

# see if we can reproduce the grouping in the original table
prt.flx <- VegsoupDataPartition(dta, 3, "wards")
prt.org <- VegsoupDataPartition(dta, 3, clustering = "block")

# block/group 3 is ambigously assigned
Confus(prt.flx, prt.org)

# write object of class VegsoupData
foo <- write.verbatim(seriation(dta), file = tempfile(), select = c(10,7,8,9,11))
foo[1:30] # resize console window
}

\seealso{
\code{\link{SpeciesWide2SpeciesLong}}, 
}
\keyword{import}
