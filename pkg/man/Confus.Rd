\encoding{UTF-8}

\name{confusion}
\docType{methods}

\alias{confusion}
\alias{confusion,VegsoupPartition-method}
\alias{confusion,VegsoupPartition,VegsoupPartition-method}

\title{ confusionion Matrix for Objects of class VegsoupPartition }

\description{
  Cross-classifies each partition (clustering) by actual class membership and
  predicted membership, computing overall accuracy, and the Kappa statistic of
  agreement.
}

\usage{
  \S4method{confusion}{VegsoupPartition,VegsoupPartition}(obj1, obj2)
}

\arguments{
	\item{ obj1, obj2 }{
	  \code{'VegsoupPartition'} object. \code{obj1} is be compared with \code{obj2}.
	}
}
\details{
  Method \code{confusion} takes two objects inheriting from class
  \code{\link{VegsoupPartition}} (\code{obj1} and \code{obj2}) and computes a
  cross-tabulation (contingency table) of reference (observed) (\code{obj1})
  vs. comparison (predicted) class memberships (\code{obj2}), and calculates
  the Kappa and Goodman-Kruskal lambda statistics.
}
\value{
  A list with the following components:
  \describe{
	\item{ confus }{
	  the cross tabulation of \code{obj1} (rows) and \code{obj2} (columns).
	  % (\code{table(partitioning(obj1), partitioning(obj1))}).
	}
	\item{ correct }{
	  the percentage of correctly predicted samples.
	}
	\item{ kappa }{
	  the value of the Kappa statistic
	}
	\item{ lambda }{
	  the value of the Lambda statistic
	}
  }
}
\references{
Goodman, L.A. and Kruskal, W.H. (1954). Measures of association for cross-classifications. Journal of the American Statistical Association, 49(268): 732-764.
}
\author{ Roland Kaiser }
\seealso{
\code{\link{VegsoupPartition}},
\code{\link{accuracy}} in package '\pkg{vegsoup}.
\code{\link{confus}} in package \pkg{optpart} and
\code{\link[polytomous:crosstable.statistics]{crosstable.statistics}} and \code{\link[polytomous:associations]{associations}} in package \pkg{polytomous}.
}
\examples{
require(vegsoup)

data(windsfeld)

prt1 <- VegsoupPartition(windsfeld, k = 10, method = "flexible")
prt2 <- VegsoupPartition(windsfeld, k = 10, method = "pam")

confusion(prt1, prt2)
}
\keyword{ methods }
\keyword{ multivariate }

