\name{Association}

\alias{Association}
\alias{Association,VegsoupPartition-method}
\alias{Association,VegsoupPartition,VegsoupPartition-method}

\title{
  Measures of association for two partitonings
}

\description{
  \code{Association} takes two objects inheriting from class
  \code{\linkS4class{VegsoupPartition}} (\code{obj1} and \code{obj1}) and computes a
  cross-tabulation (contingency table) of reference (observed) (\code{obj1})
  vs. comparison (predicted) class memberships (\code{obj2}), and calculates a
  range of measures of association between the two partitions (clusters).
}

\usage{
  Association(obj1, obj2, alpha, p.zero.correction)
}

\arguments{
  \item{ obj1 }{
    an object of class \code{"VegsoupPartition"}.
  }
  \item{ obj2 }{
    a second object of class \code{"VegsoupPartition"} to be compared to
    \code{obj1}.
  }
  \item{ alpha }{
    the significance threshold (P-value) to be used in certain
    calculations; defaults to \code{alpha = 0.05}.
  }
  \item{ p.zero.correction }{
    a (very) small value to be substituted when \code{P=0} for the meaningful
    calculation of certain statistics based on logarithmic functions; if
    missing specified according to the sum frequency of the cross tabulation
    of \code{1 / sum(table(Partitioning(obj1), Partitioning(obj1))^2}.
  }
}

\value{
  A list with the following components:
    \describe{
      \item{\code{alpha.X2}}{
        \eqn{\alpha}: significance estimate of \emph{Pearson} chi-squared
        (\eqn{\chi^2}) test of independence (homogeneity); the probability of
        falsely rejecting the null hypothesis of independence when in actual
        fact it is true.
      }
      \item{\code{alpha.G2}}{
        \eqn{\alpha}: significance estimate of \emph{Log-likelihood} ratio
        (\eqn{G^2}) test of independence (homogeneity).
      }
      \item{\code{beta}}{
        \eqn{\beta}: the probability of falsely accepting, i.e. failing to
        reject, the null hypothesis of independence when in fact the
        alternative hypothesis of dependence is true, equal to
        \eqn{1 - power}.
      }
      \item{\code{power}}{
        Probability of (correctly) rejecting the null hypothesis of
        independence when it is indeed false and the alternative hypothesis of
        dependence is true, equal to \eqn{1 - \beta}.
      }
      \item{\code{effect.size}}{
        Cohen's \emph{Effect Size} (Cohen 1988).
      }
      \item{\code{likelihood.ratio}}{
        Log-likelihood ratio.
      }
      \item{\code{cramers.v}}{
        Cramer's \emph{V} (Cramer 1946).
      }
      \item{\code{lambda.RC}}{
        Goodman-Kruskall \eqn{\lambda(R|C)} indicating how much knowing the
        values of the independent \emph{Column} variable increases the
        prediction accuracy of the values of the dependent \emph{Row}
        variable, over always selecting the \emph{Row} mode value (Goodman and
        Kruskall 1954).
      }
      \item{\code{lambda.CR}}{
        Goodman-Kruskall \eqn{\lambda(C|R)} indicating how much knowing the
        values of the independent \emph{Row} variable increases the prediction
        accuarcy of the values of the dependent \emph{Column} variable, over
        always selecting the \emph{Column} mode value.
      }
      \item{\code{tau.RC}}{
        Goodman-Kruskall \eqn{\tau(R|C)} indicating how much knowing the
        values of the independent \emph{Column} variable increases the
        prediction accuracy of the probabilities of values of the dependent
        \emph{Row} variable, over a baseline of knowing only the overall
        probabilities of the classes of the dependent \emph{Row} variable
        (Liebetrau 1983).
      }
      \item{\code{tau.CR}}{
        Goodman-Kruskall \eqn{\tau(C|R)} indicating how much knowing the
        values of the independent \emph{Row} variable increases the prediction
        accuracy of the probabilities of values of the dependent \emph{Column}
        variable, over a baseline of knowing only the overall probabilities of
        the classes of the dependent \emph{Column} variable.
      }
      \item{\code{uc.RC}}{
        Theil's Uncertainty Coefficient \eqn{UC(R|C)}, indicating how much
        knowing the values of the independent \emph{Column} variable decreases
        uncertainty about the values of the dependent \emph{Row} variable
        (Theil 1970).
      }
      \item{\code{uc.CR}}{
        Theil's Uncertainty Coefficient \eqn{UC(C|R)}, indicating how much
        knowing the values of the independent \emph{Row} variable decreases
        uncertainty about the values of the dependent \emph{Column} variable.
      }
      \item{\code{uc.sym}}{
        Theil's symmetric Uncertainty Coefficient \eqn{UC}, indicating the
        aggregate of how much knowing the values of either the \emph{Row} or
        the \emph{Column} variables decreases uncertainty about the values of
        each other.
      }
      \item{\code{p.lambda.RC}}{
        Probability of observing \eqn{\lambda(R|C)} by chance, when the
        distribution in the underlying sampling population is in fact
        homogeneous.
      }
      \item{\code{p.lambda.CR}}{
        Probability of gaining \eqn{\lambda(C|R)} by chance.
      }
      \item{\code{p.tau.RC}}{
        Probability of gaining \eqn{\tau(R|C)} by chance.
      }
      \item{\code{p.tau.CR}}{
        Probability of gaining \eqn{\tau(C|R)} by chance.
      }
      \item{\code{p.uc.RC}}{
        Probability of gaining \eqn{UC(R|C)} by chance.
      }
      \item{\code{p.uc.CR}}{
        Probability of gaining \eqn{UC(C|R)} by chance.
      }
      \item{\code{var.lambda.RC}}{
        Variance of \eqn{\lambda(R|C)}.
      }
      \item{\code{var.lambda.CR}}{
        Variance of \eqn{\lambda(C|R)}.
      }
      \item{\code{var.tau.RC}}{
        Variance of \eqn{\tau(R|C)}.
      }
      \item{\code{var.tau.CR}}{
        Variance of \eqn{\tau(C|R)}.
      }
      \item{\code{var.uc.RC}}{
        Variance of \eqn{UC(R|C)}.
      }
      \item{\code{var.uc.CR}}{
        Variance of \eqn{UC(C|R)}.
      }
      \item{\code{ASE.lambda.RC}}{
        Asymptotic standard error of \eqn{\lambda(R|C)}.
      }
      \item{\code{ASE.lambda.CR}}{
        Asymptotic standard error of \eqn{\lambda(C|R)}.
      }
      \item{\code{ASE.tau.RC}}{
        Asymptotic standard error of \eqn{\tau(R|C)}.
      }
      \item{\code{ASE.tau.CR}}{
        Asymptotic standard error of \eqn{\tau(C|R)}.
      }
      \item{\code{ASE.uc.RC}}{
        Asymptotic standard error of \eqn{UC(R|C)}.
      }
      \item{\code{ASE.uc.CR}}{
        Asymptotic standard error of \eqn{UC(C|R)}.
      }
      \item{\code{noncentrality}}{
        Noncentrality parameter.
     }
  }
}
\references{
Agresti, A. (2002). Categorical Data Analysis (2nd edition). Hoboken: John Wiley & Sons, Hoboken.

Arppe, A. (2008). Univariate, bivariate and multivariate methods in corpus-based lexicography -- a study of synonymy. Publications of the Department of General Linguistics, University of Helsinki, No. 44. URN: http://urn.fi/URN:ISBN:978-952-10-5175-3.

Cohen, J. (1988). Statistical power analysis for the behavioral sciences, (2nd edition). Hillsdale: Lawrence Erlbaum Associates.

Cramer, H. (1946). Mathematical Methods in Statistics. Princeton: Princeton University Press.

Goodman, L.A. and Kruskal, W. H. (1954). Measures of Association for Cross-Classifications. Journal of the American Statistical Association, 49(268):732--764.

Liebetrau, A.M. (1983). Measures of Association. Sage University Paper series on Quantitative Applications in the Social Sciences, 07-032. Beverly Hills and London: Sage Publications.

Theil, H. (1970). On the Estimation of Relationships Involving Qualitative Variables. The American Journal of Sociology: 76(1):103--154.
}
\author{
Antti Arppe, modified for package \code{vegsoup} by Roland Kaiser
}
\seealso{
\code{\link{Accuracy}} and \code{\link{Confus}} in package \pkg{vegsoup},
\code{\link[polytomous:associations]{associations}} in package \pkg{polytomous}
}
\examples{
require(vegsoup)
data(windsfeld)

prt1 <- VegsoupPartition(windsfeld, k = 10, method = "flexible")
prt2 <- VegsoupPartition(windsfeld, k = 10, method = "pam")

Association(prt1, prt2)
}
\keyword{ methods }
\keyword{ multivariate }

