\name{Latex}
\docType{methods}

\alias{Latex}
\alias{Latex,VegsoupPartition-method}
\alias{Latex,VegsoupPartitionFidelity-method}

\title{ Typesetting Tables in LaTeX }
\description{
  Creates meaningful table structres to be compiled by \acronym{LaTeX}.
}
\usage{
\S4method{Latex}{VegsoupPartitionFidelity}(obj, choice = "species", recursive = FALSE, file,
    mode = 1,
    p.max = .05,
    stat.min,
    constancy.min = 95,
    taxa.width = "60mm",
    col.width = "10mm",
    footer.width = "150mm",
    footer.treshold,
    molticols.footer,
    use.letters = FALSE,
    caption.text = NULL,
    quantile.select,
    coverscale = FALSE,
    sep = "/",
    sites.columns,
    newpage = TRUE,
    verbose, ...)
}

\arguments{
  \item{ obj }{
    \code{'VegsoupPartition'} object.
  }
  \item{ choice }{
    character. Either \code{"species"} or \code{"sites"}, partial matches allowed.
  }
  \item{ recursive }{
    logical. If \code{TRUE}, loop over partitions and create a standalone tables for each
    partition. If \code{FALSE}, the default, summarize information in single table.
  }
  \item{ file }{
    character. Output filename. If missing a default filename is choosen depending on
    arguments \code{choice} and \code{recursive}, otherwise file is tested for validity.
  }
  \item{ mode }{
    integer. With defaults values for choice and species (\sQuote{Details}), mode determines
    the type of ouput created. \code{mode = 1} will typeset a species by cluster fidelity table.
    \code{mode = 2} will typeset a summary table for each cluster.  See \sQuote{Details}.
  }
  \item{ p.max }{
    numeric. Threshold value for Fisher test.
  }
  \item{ stat.min }{
    numeric. Threshold value of fidelity statistic.
  }
  \item{ constancy.min }{
    numeric. Threshold value of the constancy of species(-layer replicate).
  }
  \item{ taxa.width }{
    \acronym{LaTeX} tag to set column width for taxon column.
    See \sQuote{Details}.
  }
  \item{ col.width }{
    \acronym{LaTeX} tag to set column width of all other columns. Recycled as necessary.
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
    \acronym{LaTeX} tag to set column width for footer columns.
  }
  \item{ quantile.select }{
    a character vector selecting dimensions returned by
    \code{\link{quantile}} to become pasted to the constancy value.
    Only applicable if \code{mode == 2}.
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
  \item{ coverscale }{
    logical. Recode result returend from \code{Fivenum} to ordinal scale if
    applicable, defaults to \code{FALSE}.
  }
  \item{ newpage }{
    logical. Insert LaTex newpage command.
  }  
  \item{ \dots }{
    arguments passed to \code{\link{Fidelity}} if object is not of class
    \code{"VegsoupPartitionFidelity"}.
  }
}
\details{
  Typical usages are
  \preformatted{Latex(obj)}
  \preformatted{Latex(obj, choice = "species", recursive = FALSE, file = "foo.tex", mode = 1)}
  \preformatted{Latex(obj, "species", FALSE, mode = 1, ...)}
  \preformatted{Latex(obj, "species", FALSE, mode = 2, ...)}
  \preformatted{Latex(obj, "species", TRUE,  ...)} 
  \preformatted{Latex(obj, "sites",   FALSE, ...)}
  \preformatted{Latex(obj, "sites",   TRUE,  ...)}
  If arguments \code{choice} and \code{recursive} are missing the default for both
  arguments apply (see \sQuote{Usage}).

  \code{Latex} is a complex function to create input files for \acronym{LaTeX} to
  typeset vegetation tables and related community summaries. It comes in four (five)
  flavours (see above). Some usages have many arguments, most of which
  have some sensible default values.

  The method accepts both a \code{"VegsoupPartition"} as well as a
  \code{"VegsoupPartitionFidelity"} object. The first class is promoted to the
  latter class by internally calling \code{\link{Fidelity}} with it's default
  arguments.

  The \code{choice = "species"} and \code{recursive = FALSE} form currently provides two
  differnt types of output. \code{mode = 1} builds an ordered vegetation table based on a
  fidelity (indicator species) statistic. \code{mode = 2} creates descripive summaries for each
  partition (cluster) of an object. It embeds it's ouput using \acronym{LaTeX} package subfloat
  (put usepackage{subflota} in the LaTex preamble).
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
  
  To prevent the \acronym{LaTeX} file from being displayed on screen store the result of
  Latex in an object.
}
\seealso{
\code{\link{write.verbatim}}
}
\examples{
require(vegsoup)

data(windsfeld)
x <- VegsoupPartition(windsfeld, k = 2, method = "flexible")

\dontrun{
# method currently broken
tex <- Latex(x, file = tempfile())

fid <- Fidelity(x)
class(fid)

tex <- Latex(fid, file = tempfile())

head(tex$table)
}

}
\keyword{ methods }

