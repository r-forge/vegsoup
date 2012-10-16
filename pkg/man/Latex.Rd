\name{Latex}
\alias{Latex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Latex(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object inheriting from class \code{"VegsoupDataPartition"}}
  \item{col.width}{Latex tag to set column width.}
  \item{taxa.width}{Latex tag to set column width for taxa column.}
  \item{p.max}{Threshold value for Fisher test.}
  \item{stat.min}{Threshold value for fidelity statistic.}
  \item{filename}{output filename. File extension can be missing.}
  \item{footer.treshold}{Minimum number of occurences of a species to become excluded from the table, defaults to 2.}
  \item{molticols.footer}{Number of columns to typeset species meeting conditions implied by \code{footer.treshold}}
  \item{fivenum.select}{if \code{mode = 2} a character vector selecting dimensions returned by \code{\link{Fivenum}} to become a pasted to the constancy value (see details).}
  \item{caption.text}{additional user supplied text paste into automatically generated table caption (see details).}
  \item{use.letters}{use additional letters, only applicable if \code{getK(object <= 26)}}
  \item{mode}{type of ouput. \code{mode = 1} will typeset a species by cluster fidelity table. \code{mode = 2} will typeset a summary table for each cluster (see details).}
  \item{verbose}{print function ouput to screen.}	
  \item{sep}{an optional character string to separate the summary statistics of \code{Fivenum}, defaults to \code{"/"}.}
  \item{recode}{recode result returend from \code{Fivenum} to ordinal scale if applicable, defaults to \code{"FALSE"}.}
  \item{...}{arguments passed to \code{\link{Fidelity}} if object is not of \code{class "VegsoupDataPartitionFidelity"}.}
}
\details{
\code{mode = 2} needs LaTex package subfloat 
To prevent the latex file from being displayed on screen store the result of latex in an object. Most of the time you will prefer the option rowname = NULL in latex().
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
