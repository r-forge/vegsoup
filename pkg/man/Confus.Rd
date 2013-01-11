\name{Confus}
\docType{methods}
\alias{Confus}
\alias{Confus-methods}
\title{ Confusion Matrix for Objects of class VegsoupDataPartition }
\description{
Cross-classifies each partitioning vector by actual class membership and predicted membership, computing overall accuracy, and the Kappa statistic of agreement.
}
\usage{
  Confus(obj1, obj2)
}
\arguments{
    \item{obj1}{an object of class \code{\link{VegsoupDataPartition}}. }
    \item{obj2}{a second object of class \code{\link{VegsoupDataPartition}} to compare to \code{obj1}. }
}
\details{
Method \code{Confus} takes two objects inheriting from class \code{\link{VegsoupDataPartition}} (\code{obj1} and \code{obj2}) and computes a cross-tabulation (contingency table) of reference (observed) (\code{obj1}) vs. comparison (predicted) class memberships (\code{obj2}), and calculates the Kappa and Goodman-Kruskal lambda statistics.
}
\value{
A list with the following components:
\describe{
  \item{ confus }{the cross tabulation of \code{obj1} and \code{obj2} (\code{table(Partitioning(obj1), Partitioning(obj1))}). }
  \item{ correct }{the percentage of correctly predicted samples}
  \item{ kappa }{the value of the Kappa statistic}
  \item{ lambda }{the value of the Lambda statistic}
}
}
\references{
Goodman, L.A. and Kruskal, W.H. (1954). Measures of association for cross-classifications. Journal of the American Statistical Association, 49(268): 732-764.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{VegsoupDataPartition}},\code{\link{confus}} in package \pkg{optpart} and \code{\link{crosstable.statistics}} and \code{\link{associations}} in package \pkg{polytomous}, \code{\link{Association}}, \code{\link{Accuracy}} and \code{\link{Confus}} in package 'Vegsoup'. 
}
\examples{

}
\keyword{ methods }

