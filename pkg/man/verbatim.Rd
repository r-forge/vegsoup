\name{verbatim}
\docType{methods}

\alias{verbatim}

\alias{read.verbatim}

\alias{read.verbatim.append}

\alias{castFooter}
\alias{header}

\alias{write.verbatim}
\alias{write.verbatim,Vegsoup-method}
\alias{write.verbatim,VegsoupPartition-method}

\title{ Read or Write Fixed-Column-Width Community Tables From or To Plain Text Files }

\description{
  The base S3 function \code{read.verbatim(file)} reads data from text files
  as  encountered in published tables typesetted with monospaced fonts. It's
  main purpose is to help digitizing literature data. The corresponding
  \code{write.verbatim(obj)} method performs the reverse on objects
  inheriting from class \code{'Vegsoup'}.
}

\usage{
read.verbatim(file, colnames, layers, replace = c("|", "-", "_"),
         species.only = FALSE, verbose = TRUE)

read.verbatim.append(x, file, mode = c("plots", "species", "layers"),
         collapse = ",", abundance)

castFooter(file, schema = c(":", "," , " "), first = TRUE)

header(x)

\S4method{write.verbatim}{Vegsoup}(obj, file, select, absence = ".",
          sep = " ", pad = 1, abbreviate = TRUE, short.names = FALSE,
		  rule, add.lines = FALSE, latex.input = FALSE)

\S4method{write.verbatim}{VegsoupPartition}(obj, file, select, absence = ".",
          sep = " ", pad = 1, abbreviate = TRUE, short.names = FALSE,
  		  rule, add.lines = FALSE, latex.input = FALSE)
}

\arguments{
## Arguments to functions
  \item{ file }{
    A character string. A path to a plain text file.
}
## Arguments to function \code{read.verbatim}
  \item{ colnames }{
    A character string to be searched for in the header section and used to
    assign plot names. Note, do not use any characters defined for argument
    replace in the header section! See \sQuote{Details}.
  }
  \item{ layers }{
    A named list giving start and end indices of to assign a layer to each
    species, or a vector of characters of length according to the number of
    species in the data set. See \sQuote{Examples}.
  }
  \item{ replace }{
    A vector of characters to be replaced by blanks. Note, both the hyphen and
    dash characters are defined in default set. See \sQuote{Details}.
  }
  \item{ schema }{
    A vector of characters of length 3 to be used to split character strings.
    Element one gives the separator to prune the plot identifier.
    Element two is the separator in the taxon and abundance value part.
    Element three is used to prune abundance values associated with each taxon.
    See \sQuote{Details}.
  }
  \item{ first }{
    If \code{TRUE} abundance value precede taxon names, if \code{FALSE} abundance
    values are given after taxon names.
  }
  \item{ species.only }{
    Just return a vector of species extracted from the data set.
  }
  \item{ verbose }{
    Print diagnostic messages.
  }
  \cr
## Arguments to function \code{read.verbatim.append}
  \item{ x }{
    An object of class \code{VegsoupVerbatim} as created by calls to function
    \code{read.verbatim}.
  }
  \item{ mode }{
    Character, defining how the species occurrences in rows are organized.
    See \sQuote{Details}.
  }
  \item{ sep }{
    Character, the separator between successive species.
    Defaults to \code{","}.
  }
  \item{ abundance }{
    For \code{mode = "species"} a single \code{character} recycled for all
    observations and setting a dummy cover value if no abundance for a species
    is supplied in the data source. Defaults to \code{"+"}.

    For \code{mode = "plots"} either \code{TRUE} or \code{FALSE} and
    specifying if the character found after the species name should be treated
    as a cover value. See \sQuote{Details}. If \code{abundance = FALSE} the
    defaults of \code{mode = "species"} apply with no option to override it,
    but setting \code{abundance} to any value (e.g. \code{"+"}) will use that
    value.
}
\cr
## Arguments for S4 method \code{write.verbatim} for signature \code{'Vegsoup'} and \code{'VegsoupPartition'}
  \item{ obj }{
    An object of class \code{VegsoupData}.
  }
  \item{ select }{
    Character vector matching \code{names(obj)} or indices to columns in
    Sites(obj) to select information to be incorporated in the header section.
    Beware, this only makes sense for numeric (e.g. elevation) or short string
    variables (e.g. slope aspect)!
  }
  \item{ absence }{
    Character to code absences, defaults to \code{"."}.
  }
  \item{ collapse }{
    Character to be used as separator in columns of abundances (plots).
  }
  \item{ pad }{
    Integer specifying the number of blanks to add to taxon names (right side)
    and layers string (both sides), defaults to 1.
  }
  \item{ abbreviate }{
    Truncate abundance values to width 1 using \code{abbreviate(x, minlength =
    1, strict = TRUE)}.
  }
  \item{ rule }{
    Integer vector of length equal to the number of plots \code{nrow(obj)}.
    The lengths of runs of equal values in this vector are used to insert
    vertical rule mimicked by the pipe gylph (\code{'|'}).
  }
  \item{ short.names }{
    Use taxon abbreviation instead of long scientific names. Dots are
    converted to blanks.
  }
  \item{ add.lines }{
    Add blank lines separating header section.
  }
  \item{ latex.input }{
    Warp output in LaTex verbatim environment.
  }
}

\details{
The data format of the input file equals in its fundamentals the output format of traditional software such as \sQuote{TWINSPAN} and users are assumed to be familiar with such data structures. Also editing plain text files where matrix like data structures are achieved using monospaced fonts is presupposed. In order to create a fully valid input file a text editor allowing hidden characters to be displayed is essential. At least the editor must be capable of displaying line end special characters (see below).

In general, the data layout consists of species abundances measured on plots where species performance is coded nothing but as a single character. Species or variables are in rows and plots are columns. As a consequence, using a monospaced font each observation on a plot aligns to a column composed of single characters. Only in the header section (see 1. below) values of width longer than one are supported. Adopting the previous logic, these figures have to be aligned vertically. For example, a value of \sQuote{1000} needs for lines, one line for each digit. See \sQuote{Examples} for a valid input file. 

Horizontal and vertical table rules are often found in printed tables. When digitizing such a table the it is often handy to use some special characters to align with the original table strokes. Because of their non informative properties these characters are discarded when parsing the input file and all characters supplied with \code{replace} are replaced with blanks. Care has to be taken with hyphens and dashes (see defaults of \code{replace}). R treats the minus sign as a dash (\code{"\uad"}). Warning, problems might arise on non UTF-8 platforms.

The S3 function \code{read.verbatim()} makes the following additional assumptions about the input file format (not a file format in the strict sense, but a set of conventions).

\enumerate{
\item The table header and table body must be enclosed within a pair of keywords. Lines giving the keywords \sQuote{BEGIN HEAD} and \sQuote{END HEAD} must be present at the beginning and end, respectively, of the table header. This block of data has to by enclosed with \sQuote{'BEGIN TABLE'} and \sQuote{END TABLE} to identify the main table structure.
\item The data blocks (HEADER and TABLE) can have empty lines and/or columns of spaces separating plots.
\item The width of the table (number of monospaced font characters) should align at right side. It is crucial to ensure that all line end characters align vertically!
\item All species absences have to be coded with a dot (\sQuote{.}). This is often found in published tables. At least when digitizing using dots for absences helps in aligning columns.
\item Given that absences are coded as \sQuote{.} each cell of the community table has a value. This also ensures that the left margin of the community table can be filtered automatically.
\item The width (number of characters) of the header and the table body data blocks has to perfectly match. The values found in the header (possibly of length longer than one and aligned vertically) exactly corresponds the species abundances on the same plot. In other words, they align to the same column in a monospaced font layout.
\item Tab characters (\code{"\t"}) are not allowed in the input file!
}

Plot identifiers can be assigned using one of the attributes on the resulting object or by supplying an argument \code{colnames}.
Function \code{header} can be used to retrieve the header part stared as additional object matrix attributes.

It is often the case that rare species are only given in the table footer but not in the main table. The function \code{read.verbatim.append()} takes an object created by calls to \code{read.verbatim()} and adds species given in argument \code{file}. The function currently accepts two formats specified by argument \code{mode}. Input files in \code{mode = "species"} requires a simple text file, where each row corresponds to a unique species, followed by a colon (\sQuote{:}) and subsequent strings matching \code{colnames(x)} and separated with commas (see argument \code{collapse}). All spaces found after the colon will be discarded. See \sQuote{Examples} for a valid input file in species mode. Files applicable to \code{mode = "plots"} have plots (must match \code{colnames(x)}) in each row, followed by a colon. Note, if the plot identifier can be interpreted as numeric leading zeros are stripped of. This is also the behavior of \code{read.verbatim}. The part after the colon has to be an enumeration of taxa for a respective plot; again separated by argument \code{collapse}. If argument \code{abundance} is \code{TRUE} the single last character after the species name is treated as an abundance value. \code{mode = "layers"} allows also a layer to be parsed.

Function \code{stackFooter} is a stand-alone utility function to cast a three column matrix from plain text consisting of (1) plot, (2) species and (3) abundance values character strings given on a single row of the input file. Suppose the following example: \code{"30: 1 Empetrum nigrum ssp. hermaphroditum, + Arabis ciliata, + Luzula campestris"}. In this case, \code{30:} gives the plot identifier, it is separated with a colon from the listing of species with their associated abundance values. Each species and cover combination is further separated with a comma. Finally, the abundance value can be unscrambled by searching for the first space. The \code{schema} argument must be used to define the particular patterning of the input file. For this particular case it is \code{c(":", ",", " ")}. Unfortunately there are no standards and there are plenty of variants in the literature. The main practical effect of the \code{stackFooter} function is to transpose the data to a matrix representation for further manipulation.

Finally the S4 method \code{write.verbatim()} creates output honoring the set of definitions given above. See \sQuote{Arguments} for possible customization of the output file. If called on an object of class \code{"VegsoupPartition"} argument \code{rule} is taken form \code{Partitioning(obj)}. To get partition aligned properly it might be necessary to apply \code{seriation(obj)} beforehand.

The print method for objects of class \code{VegsoupVerbatim} uses \code{as.data.frame(x)} as a means to get rid of quotes and to provide clean screen output.
}

\value{
\code{read.verbatim} and \code{read.verbatim.append} return an S3 object of class \code{VegsoupVerbatim}. Basically a matrix of mode \code{character} with attributes giving the data in the header section of the input file. Species are rownames. If argument \code{colnames} is supplied the returned matrix will hold meaningful \code{colnames}.

\code{write.verbatim} writes a file to disk and invisibly returns the vector of characters written to the file.

\code{castFooter} returns a matrix with three columns.
}

\note{
It is hard to avoid typos when editing mono spaced table structures, especially, when the are a lot of rows and columns. The human eye easily gets impaired by the overwhelming number of values. The \code{read.verbatim} function will report any instance that is not valid and diagnostic messages are printed to the console to aid the user in correcting the input file. The probability of typos increases if there are columns of spaces separating columns of data.

Leading zeros in \code{ colnames } if it can be coerced to \code{as.numeric}, as well as in the header block as a whole, are not preserved but are subject to a call to \code{\link{type.convert}}!
}

\author{
Roland Kaiser
}
\references{
The demonstration data set used in the example is taken from:

Erschbamer, B. (1992). Zwei neue Gesellschaften mit Krummseggen (\emph{Carex curvula} ssp. \emph{rosae}, \emph{Carex curvula} ssp. \emph{curvula}) aus den Alpen – ein Beitrag zur Kla"rung eines alten o"kologischen Ra"tsels. Phytocoenologia, 21:91–116.

The are three text files. \code{"Erschbammer1992Tab4.txt"} has the main vegetation table plus some header rows. Species names were taxonomically interpreted to a match a reference list supplied as \code{"Erschbammer1992Taxonomy.txt"}.   \code{"Erschbammer1992Tab4Tablefooter.txt"} has a list of rare species, those with frequency lower than 4. \code{"Erschbammer1992Tab4Locations.txt"} has geographic coordinates interpreted from the data source.
}
\examples{
# text encoding issue?
\dontrun{
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
x <- read.verbatim.append(x, file, mode = "species")
tail(x)
# 68 species were added
dim(x)

# turn into long format
x.df <- data.frame(abbr = rownames(x), layer = "hl", taxon = NA, x,
                   check.names = FALSE)
# promote to class "Species"
X <- stackSpecies(x.df)

#   sites data including coordinates
file <- system.file("extdata", "Erschbammer1992Tab4Locations.txt",
                    package = "vegsoup")
Y <- read.delim(file, colClasses = "character")
names(Y)[1] <- "plot"
# promote to class "Sites"
Y <- stackSites(Y)

# taxonomy reference list
file <- system.file("extdata", "Erschbammer1992Taxonomy.txt",
                    package = "vegsoup")
Z <- read.delim(file, colClasses = "character")
#   promote to class Taxonomy
Z <- taxonomy(Z)

# groome abundance scale codes to fit the standard
# of the extended Braun-Blanquet scale used in the origional publication
X$cov <- gsub("m", "2m", X$cov)
X$cov <- gsub("a", "2a", X$cov)
X$cov <- gsub("b", "2b", X$cov)

dta <- Vegsoup(X, Y, Z, "braun.blanquet")

# assign header data stored as attributes in
# imported original community table
# omit dimnames, class and plot id
df.attr <- as.data.frame(attributes(x)[-c(1:4)])
rownames(df.attr) <- colnames(x)
# reorder by plot
df.attr <- df.attr[match(rownames(dta), rownames(df.attr)), ] 

# give names and assign
dta$block <- df.attr$Block
dta$altitude <- df.attr$Seeho"he
dta$expo <- df.attr$Exposition
dta$cov <- df.attr$"Deckung...."
dta$ph <- df.attr$"ph...10.2."

names(Sites(dta))

# plot of the dissimilarity matrix
coldiss(dta, diag = T)

# see if we can reproduce the grouping in the original table
prt.flx <- VegsoupPartition(dta, 3, "wards")
prt.org <- VegsoupPartition(dta, 3, clustering = "block")

# block/group 3 is ambigously assigned
Confus(prt.flx, prt.org)

# write object of class Vegsoup
txt <- write.verbatim(seriation(dta), file = tempfile(),
    select = c(8,7,6,9))
txt[1:30] # resize console window
}
}
\seealso{
\code{\link{stackSpecies}}, \code{\link{stackSites}}
}
\keyword{import}
