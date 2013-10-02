\name{Latex}
\docType{methods}
\alias{Latex}
\alias{Latex-methods}
\alias{Latex,VegsoupPartition-method}
\alias{Latex,VegsoupPartitionFidelity-method}

\title{ Create Files for Typesetting Tables in LaTex }
\description{
  Creates meaningful table structres to be compiled by LaTex.
}
\usage{
## S4 method for signature 'VegsoupPartition' and 'VegsoupPartitionFidelity'
Latex(obj, choice, recursive, ...)

## S4 method for choice = "sites" and recursive = FALSE
Latex(obj, col.width, filename, verbose = FALSE, ...)

## S4 method for choice  = "sites" and recursive = TRUE
Latex(obj, path, ...)

## S4 method for choice = "species" and recursive = FALSE
Latex(obj, filename, mode = 1, p.max = .05, stat.min, constancy.treshold = 95,
     taxa.width = "60mm", col.width = "10mm", footer.treshold,
     molticols.footer, footer.width = "150mm", use.letters = FALSE,
     caption.text = NULL, fivenum.select, recode = FALSE, sep = "/",
     sites.columns, verbose = FALSE, ...)

## S4 method for choice = "species" and recursive = TRUE
Latex(obj, path, col.width, taxa.width, caption.text, verbose, ...)
}

\arguments{
  \item{ obj }{
    An object inheriting from class \code{"VegsoupPartition"}
  }
  \item{ choice }{
    character. Either \code{"species"} or \code{"sites"}.
  }
  \item{ recursive }{
    logical. If \code{TRUE} create tables for each partition. If \code{FALSE},
    the default, summarize information in one table.
  }
  \item{ path }{
    character. Path to a folder.
  }
  \item{ filename }{
    character. Output filename. File extension can be missing.
  }
  \item{ mode }{
    type of ouput. \code{mode = 1} will typeset a species by cluster fidelity
    table. \code{mode = 2} will typeset a summary table for each cluster.
    See \sQuote{Details}.
  }
  \item{ p.max }{
    numeric. Threshold value for Fisher test.
  }
  \item{ stat.min }{
    numeric. Threshold value of fidelity statistic.
  }
  \item{ constancy.treshold }{
    numeric. Threshold value of the constancy of species(-layer replicate).
  }
  \item{ taxa.width }{
    LaTex tag to set column width for taxon column.
    See \sQuote{Details}.
  }
  \item{ col.width }{
    LaTex tag to set column width of all other columns. Recycled as necessary.
    See \sQuote{Details}.
  }
  \item{ footer.treshold }{
    integer. Minimum number of occurences of a species to become excluded from
    the table, defaults to 2.
  }
  \item{ molticols.footer }{
    integer. Number of columns to typeset species meeting conditions implied
    by \code{footer.treshold}
  }
  \item{ footer.width }{
     LaTex tag to set column width for footer columns.
  }
  \item{ fivenum.select }{
    a character vector selecting dimensions returned by
    \code{\link{Fivenum}} to become pasted to the constancy value.
    Only applicable in \code{mode = 2}.
    See \sQuote{Details}.
  }
  \item{ caption.text }{
    character. Additional user supplied text pasted into automatically
    generated table caption.
    See \sQuote{Details}.
  }
  \item{ sites.columns }{
    character. Selceting columns from sites data frame.
  }
  \item{ use.letters }{
    logical. Use additional letters.
    Only applicable if \code{getK(object <= 26)}.
  }
  \item{ verbose }{
    logical. Print messages on screen.
  }
  \item{ sep }{
    character. Optional character string to separate the summary statistics of
    \code{Fivenum}, defaults to \code{"/"}.
  }
  \item{ recode }{
    logical. Recode result returend from \code{Fivenum} to ordinal scale if
    applicable, defaults to \code{FALSE}.
  }
  \item{ \dots }{
    arguments passed to \code{\link{Fidelity}} if object is not of class
    \code{"VegsoupPartitionFidelity"}.
  }
}
\details{
  \code{Latex} is a complex function to create input files for LaTex to
  typeset vegetation tables and related community summaries. It comes in four
  flavours. See \sQuote{Usage}. Some usages have many arguments, most of which
  have some sensible default values.

  The method accepts both a \code{"VegsoupPartition"} as well as a
  \code{"VegsoupPartitionFidelity"} object. The first class is promoted to the
  latter class by internally calling \code{\link{Fidelity}} with it's default
  arguments.

  The method currently provides two differnt outputs. \code{mode = 1} builds
  an ordered vegetation table based on a fidelity (indicator species)
  statistic.

  \code{mode = 2} creates descripive summaries for each partition (cluster) of
  an object. It embeds it's ouput using LaTex package subfloat.
}
\value{
  A \code{"list"} containing the lines written to the file:
  \item{ table }{
    The main table. For \code{mode = 2} a list of subtables for each partition
   (cluster).
  }
  \item{ footer.sites }{
    Additional lines for table footer.
  }
  \item{ footer.species }{
    Additional lines for table footer, giving species excluded from the table.
  }
}
\author{ Roland Kaiser }
\note{
%%Most of the time you will prefer the option rowname = NULL in latex().
  
  To prevent the LaTex file from being displayed on screen store the result of
  latex in an object.
}
\seealso{
\code{\link{write.verbatim}}
}
\examples{
require(vegsoup)

data(bigtestdata)
dta <- Vegsoup(spc.big, sts.big, txa.big, coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, k = 10)

ltx <- Latex(prt, filename = tempfile())

fid <- Fidelity(prt)
class(fid)

ltx <- Latex(fid, mode = 2, filename = tempfile())

head(ltx$table)
}
\keyword{ methods }

