\encoding{UTF-8}

\name{Accuracy}

\alias{Accuracy}
\alias{Accuracy,VegsoupPartition-method}
\alias{Accuracy,VegsoupPartition,VegsoupPartition-method}

\title{ Accuracy Statistics for Objects of class VegsoupPartition }

\description{
  \code{Accuracy} takes two objects inheriting from class
  \code{'VegsoupPartition'} (\code{obj1} and \code{obj1}) and computes a
  cross-tabulation (contingency table) of reference (observed) (\code{obj1})
  vs. compoarison (predicted) class memberships (\code{obj2}), and calculates
  a range of statistics about prediction accuracy.
}

\usage{
  Accuracy(obj1, obj2)
}

\arguments{
	\item{ obj1 }{
	  an object of class \code{'VegsoupPartition'}.
	}

	\item{ obj2 }{
	  a second object of class \code{'VegsoupPartition'} to compare to \code{obj1}.
	}
}

\value{
A list with the following components:
\describe{
  \item{\code{accuracy}}{Overall prediction accuracy}
  \item{\code{recall.predicted}}{Recall of prediction for each outcome value}
  \item{\code{precision.predicted}}{Precision of prediction for each outcome value}
  \item{\code{lambda.prediction}}{lambda for prediction accuracy (improvement over baseline of always predicting mode)}
  \item{\code{tau.classification}}{tau for classification accuracy (improvement over baseline of homogeneous distribution of predicted outcomes)}
  \item{\code{d.lambda.prediction}}{d(lambda): used for calculating \code{P(lambda)}}
  \item{\code{d.tau.classification}}{d(tau): used for calculating \code{P(tau)}}
  \item{\code{p.lambda.prediction}}{P(lambda): probability of reaching \code{lambda} by chance}
  \item{\code{p.tau.classification}}{P(tau): probability of reaching \code{tau} by chance}
}
}

\references{
  Menard, Scott (1995). Applied Logistic Regression Analysis. Sage University Paper 
  Series on Quantitative Applications in the Social Sciences 07-106. Thousand 
  Oaks: Sage Publications. 
}

\author{
Adopted by Roland Kaiser for vegsoup from code by \emph{Antti Arppe}
}

\seealso{
\code{\link[polytomous:model.statistics]{model.statistics}} in package \pkg{polytomous}.
}

\examples{
require(vegsoup)
data(windsfeld)

prt1 <- VegsoupPartition(windsfeld, k = 10, method = "flexible")
prt2 <- VegsoupPartition(windsfeld, k = 10, method = "pam")

Accuracy(prt1, prt2)
}
\keyword{ methods }
\keyword{ multivariate }

