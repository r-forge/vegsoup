\name{Vegsoup}
\alias{Vegsoup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Vegsoup(x, y, z, scale = c("Braun-Blanquet", "frequency", "binary"), group, sp.points, sp.polygons)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{sp.points}{
%%     ~~Describe \code{sp.points} here~~
}
  \item{sp.polygons}{
%%     ~~Describe \code{sp.polygons} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, z, scale = c("Braun-Blanquet", "frequency", "binary"), 
    group, sp.points, sp.polygons) 
{
    if (missing(x)) {
        x <- data.frame(NULL)
        stop("query on species is empty!\n")
    }
    if (missing(y)) {
        y <- data.frame(NULL)
        stop("query on sites is empty!\n")
    }
    if (missing(z)) {
        z <- data.frame(NULL)
        stop("query on taxonomy is empty!\n")
    }
    x$abbr <- make.names(x$abbr)
    z$abbr <- make.names(z$abbr)
    if (missing(scale)) {
        warning("No cover scale provided", immediate. = TRUE)
        if (is.character(x$cov)) {
            warning("Interpret abundance values as charcter")
            warning("\nset cover scale to default 9 point Braun-Blanquet scale")
            scale <- list(scale = "Braun-Blanquet", codes = c("r", 
                "+", "1", "2m", "2a", "2b", "3", "4", "5"), lims = c(1, 
                2, 3, 4, 8, 18, 38, 68, 88))
            cat("\n", scale$codes)
        }
        else {
            cat("cover seems to be numeric")
            cat("\nset abundance scale to frequency")
            scale <- c(scale = "frequency", list(codes = NULL), 
                list(lims = NULL))
        }
    }
    else {
        if (is.list(scale)) {
            if (length(scale) == 1) {
                if (scale[[1]] == "frequency") {
                  scale <- c(scale, list(codes = NULL), list(lims = NULL))
                  stopifnot(is.numeric(x$cov))
                }
                if (scale[[1]] == "binary") {
                  scale <- c(scale, list(codes = NULL), list(lims = NULL))
                  stopifnot(dim(table(x$cov)) > 2)
                }
                if (scale[[1]] == "Braun-Blanquet") {
                  scale <- list(scale = "Braun-Blanquet", codes = c("r", 
                    "+", "1", "2m", "2a", "2b", "3", "4", "5"), 
                    lims = c(1, 2, 3, 4, 8, 18, 38, 68, 88))
                }
                if (scale[[1]] == "Braun-Blanquet 2") {
                  scale <- list(scale = "Braun-Blanquet", codes = c("r", 
                    "+", as.character(1:5)), lims = c(1, 2, 3, 
                    13, 38, 68, 88))
                }
                if (scale[[1]] == "Domin") {
                  scale <- list(scale = "Domin", codes <- c("+", 
                    as.character(1:9), "X"), lims <- c(0, 0.01, 
                    0.1, 1, 5, 10, 25, 33, 50, 75, 90, 100))
                }
            }
        }
    }
    if (missing(group)) {
        group <- as.integer(rep(1, length(unique(x$plot))))
        names(group) <- unique(x$plot)
        cat("\nno grouping factor supplied,", "use single partition")
    }
    else {
        stopifnot(!is.null(names(group)))
        if (!is.integer(group)) {
            group.names <- names(group)
            group <- as.integer(group)
            names(group) <- group.names
        }
    }
    if (missing(sp.points)) {
        cat("\nSpatialPoints missing, use random pattern")
        sp.points <- .rpoisppSites(x)[[1]]
    }
    if (missing(sp.polygons)) {
        cat("\nSpatialPolygons missing, use random pattern")
        sp.polygons <- .rpoisppSites(x)[[2]]
    }
    if (any(sapply(y, is.factor))) {
        y <- as.data.frame(as.matrix(y), stringsAsFactors = FALSE)
    }
    res <- new("Vegsoup", species.long = x, sites.long = y, taxonomy = z, 
        scale = as.list(scale), layers = as.character(unique(x$layer)), 
        group = group, sp.points = sp.points, sp.polygons = sp.polygons)
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
