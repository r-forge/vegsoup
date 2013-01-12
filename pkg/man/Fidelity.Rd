\name{Fidelity}
\docType{methods}
\alias{Fidelity}
\alias{Fidelity-methods}
\alias{Fidelity,VegsoupDataPartition-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fidelity(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{method}{
%%     ~~Describe \code{\dots} here~~
}
  \item{group}{
%%     ~~Describe \code{\dots} here~~
}
  \item{nboot}{
%%     ~~Describe \code{\dots} here~~
}
  \item{alpha}{
%%     ~~Describe \code{\dots} here~~
}
  \item{c}{
%%     ~~Describe \code{\dots} here~~
}
  \item{fast}{
Accelerate computations. Needs package \code{multicore}.
}

  \item{\dots}{
Additional arguments when method is "fisher.s" 
}
}
\details{
\code{r:} phi (point-biserial correlation coefficient)

\code{r.g:} phi, group equalised

\code{cos:} cosine (Ochiai index)

\code{cos.g:} cosine (Ochiai index), group equalised

\code{r.ind:} phi individual based

\code{r.ind.g:} phi individual based, group equalised

\code{s.ind:} square root of Indval A times Indval B

\code{s.ind.g:} square root of Indval A.g * Indval B, group equalised

\code{IndVal.g:} Indval, group equalised

\code{IndVal:} Indval

\code{A.g:} Indval A, group equalised

\code{A:} Indval A

\code{B:} Indval B

\code{u.hyp:} Bruelheide's corrected u value

\code{u.binB:} Bruelheide's corrected u value uBinB

\code{u.binA:} Bruelheide's corrected u value uBinA

\code{g:} G statistic

\code{chi:} chi-square statistic with Yates correction

\code{Fisher:} Fisher test

\code{CR:} constancy ratio

\code{TCR:} total cover ratio

}
\value{
an object of class \code{VegsoupDataPartitionFidelity}
}
\references{
Willner, W., Tichy, L., and Chytry, M. (2009). Effects of different fidelity measures and contexts on the determination of diagnostic species. Journal of Vegetation Science, 20(1):130–137.

Willner, W. (2006). The association concept revisited. Phytocoenologia, 36(1):67–76.

Chytry ́, M., Tichy ́, L., Holt, J., and Botta-Duk ́at, Z. (2002). Determination of diag- nostic species with statistical fidelity measures. Journal of Vegetation Science, 13(1):79–90.

De Caceres, M., Font, X., and Oliva, F. (2008). Assessing species diagnostic value in large data sets: A comparison between phi-coefficient and ochiai index. Journal of Vegetation Science, 19(6):779–788.

De Caceres, M. and Legendre, P. (2009). Associations between species and groups of sites: indices and statistical inference. Ecology, 90(12):3566–3574.

De Caceres, M., Legendre, P., and Moretti, M. (2010). Improving indicator species analysis by combining groups of sites. Oikos, 119(10):1674–1684.

Bruelheide, H. (2000). A new measure of fidelity and its application to defining species groups. Journal of Vegetation Science, 11(2):167–178.
}
\author{
Roland Kaiser, based on code from Miquel De Cáceres Ainsa taken from function strassoc.R in package \code{indicspecies}.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
