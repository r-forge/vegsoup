\name{Fidelity}
\docType{methods}
\alias{Fidelity}
\alias{Fidelity-methods}
\alias{FisherTest}
\alias{FisherTest-methods}
\alias{Phi}
\alias{Phi-methods}
\alias{Indval}
\alias{Indval-methods}

\title{ Calculate Fidelity Values }
\description{
Function \code{Fidelity} calculates fidelity values between two vectors for object inheriting \code{'VegsoupPartition'} and returns an extended object of class \code{\linkS4class{VegsoupPartitionFidelity}}. Methods \code{Phi} and \code{Indval} calculates the phi statistics acording to \code{\link[phi:optpart]{indval}} in package \pkg{optpart}. \code{IndVal} is a wrapper for \code{\link[phi:optpart]{indval}} of package \pkg{labdsv}. Method \code{FisherTest} is a bit faster alternative to \code{Fidelity(obj, method = "fisher")}. \code{Phi} and \code{Indval} are also slightly faster compared to \code{\link{Fidelity}}. All three do not return an \code{'VegsoupPartitionFidelity'} object.
}

\usage{
## S4 method for signature 'VegsoupPartition'
Fidelity(obj, method = "r.g", group = NULL, nboot = 0, alpha = 0.05, c = 1,
         fast = FALSE, verbose = TRUE, ...)
FisherTest(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ obj }{
    An object inheriting from class \code{'VegsoupPartition'}
   }
  \item{ method }{
    Indicator measure.
  }
  \item{ group }{
    A grouping variable.
  }
  \item{ nboot }{
    Number of boot straps, defaults to .
  }
  \item{ alpha }{
    Significance.
  }
  \item{ c }{
    Forgot what that is:)
  }
  \item{ fast }{
    Accelerate computations. Needs package \code{multicore}.
  }
  \item{ verbose }{
    Display compuation status bar.
  }
  \item{\dots}{
    Additional arguments for \code{method = "fisher.s"}.
  }
}
\details{
Methods currently implemented include the following:

\enumerate{
   \item \code{r:} phi (point-biserial correlation coefficient)
   \item \code{r.g:} phi, group equalised
   \item \code{cos:} cosine (Ochiai index)
   \item \code{cos.g:} cosine (Ochiai index), group equalised
   \item \code{r.ind:} phi individual based
   \item \code{r.ind.g:} phi individual based, group equalised
   \item \code{s.ind:} square root of Indval A times Indval B
   \item \code{s.ind.g:} square root of Indval A.g * Indval B, group equalised
   \item \code{IndVal.g:} Indval, group equalised
   \item \code{IndVal:} Indval
   \item \code{A.g:} Indval A, group equalised
   \item \code{A:} Indval A
   \item \code{B:} Indval B
   \item \code{u.hyp:} Bruelheide's corrected u value
   \item \code{u.binB:} Bruelheide's corrected u value uBinB
   \item \code{u.binA:} Bruelheide's corrected u value uBinA
   \item \code{g:} G statistic
   \item \code{chi:} chi-square statistic with Yates correction
   \item \code{Fisher:} Fisher test
   \item \code{CR:} Willner's constancy ratio
   \item \code{TCR:} Willner's total cover ratio
}
}
\value{
For \code{Fidelity} an object of class \code{VegsoupPartitionFidelity}

For \code{FisherTest} and \code{Phi} a \code{matrix} of values with species as rows and partitions as columns
}
\references{
Willner, W., Tichy, L., and Chytry, M. (2009). Effects of different fidelity measures and contexts on the determination of diagnostic species. Journal of Vegetation Science, 20(1):130–137.

Willner, W. (2006). The association concept revisited. Phytocoenologia, 36(1):67–76.

Tichy, L. and M. Chytry. 2006. Statistical determination of diagnostic species for site groups of unequal size. Journal of Vegetation Science 17:809-818.

Chytry ́, M., Tichy ́, L., Holt, J., and Botta-Duk ́at, Z. (2002). Determination of diag- nostic species with statistical fidelity measures. Journal of Vegetation Science, 13(1):79–90.

De Caceres, M., Font, X., and Oliva, F. (2008). Assessing species diagnostic value in large data sets: A comparison between phi-coefficient and ochiai index. Journal of Vegetation Science, 19(6):779–788.

De Caceres, M. and Legendre, P. (2009). Associations between species and groups of sites: indices and statistical inference. Ecology, 90(12):3566–3574.

De Caceres, M., Legendre, P., and Moretti, M. (2010). Improving indicator species analysis by combining groups of sites. Oikos, 119(10):1674–1684.

Bruelheide, H. (2000). A new measure of fidelity and its application to defining species groups. Journal of Vegetation Science, 11(2):167–178.
}
\author{
Roland Kaiser, based on code from Miquel De Cáceres Ainsa taken from function \code{\link{strassoc}} in package \pkg{indicspecies}.
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\linkS4class{VegsoupPartitionFidelity}}
}
\examples{
require(vegsoup)
data(testdata)
prt <- VegsoupPartition(Vegsoup(spc, sts, txa, "braun.blanquet"), k = 2)
fid <- Fidelity(prt)
}
\keyword{ method }
\keyword{ multivariate }
