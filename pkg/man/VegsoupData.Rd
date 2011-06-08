\name{VegsoupData}
\alias{VegsoupData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VegsoupData(obj, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (obj, verbose = FALSE) 
{
    require(stats)
    if (!inherits(obj, "Vegsoup")) 
        stop("Need object of class Vegsoup")
    scale <- AbundanceScale(obj)
    lay <- Layers(obj)
    txa <- Taxonomy(obj)
    species.long <- SpeciesLong(obj)
    if (scale$scale == "Braun-Blanquet") {
        stopifnot(is.character(species.long$cov))
        if (length(lay) == 1) {
            if (verbose) 
                cat("\ndata is structered in only one layer")
        }
        else {
            if (verbose) 
                cat("\ndata is structered in layers: ", lay)
        }
        cpu.time <- system.time({
            tmp <- as.factor(species.long$cov)
            levels(tmp) <- scale$lims[match(levels(tmp), scale$codes)]
            species.long$cov <- as.numeric(as.character(tmp))
            xt <- xtabs(cov ~ plot + abbr + layer, data = species.long)
            if (dim(xt)[3] > 1) {
                res <- matrix(0, ncol = dim(xt)[2] * dim(xt)[3], 
                  nrow = dim(xt)[1], dimnames = list(plot = dimnames(xt)$plot, 
                    abbr = paste(rep(dimnames(xt)$abbr, dim(xt)[3]), 
                      rep(dimnames(xt)$layer, each = dim(xt)[2]), 
                      sep = "@")))
            }
            else {
                res <- matrix(0, ncol = dim(xt)[2], nrow = dim(xt)[1], 
                  dimnames = list(plot = dimnames(xt)$plot, abbr = paste(dimnames(xt)$abbr, 
                    dimnames(xt)$layer, sep = "@")))
            }
            for (i in 1:dim(xt)[3]) {
                sel <- grep(paste("", dimnames(xt)$layer[i], 
                  sep = "@"), dimnames(res)$abbr, fixed = TRUE)
                res[, sel] <- xt[, , i]
            }
            res <- res[, colSums(res) > 0]
            res <- as.data.frame(res)
            for (i in seq(along = scale$lims)) {
                res[res == scale$lims[i]] <- scale$codes[i]
            }
            species <- res
        })
    }
    cpu.time <- system.time({
        if (scale$scale == "frequency") {
            if (!is.numeric(species.long$cov)) {
                warning("changed mode to numeric", str(species.long$cov))
                mode(species.long$cov) <- "numeric"
            }
            xt <- xtabs(cov ~ plot + abbr + layer, data = species.long)
            if (dim(xt)[3] > 1) {
                res <- matrix(0, ncol = dim(xt)[2] * dim(xt)[3], 
                  nrow = dim(xt)[1], dimnames = list(plot = dimnames(xt)$plot, 
                    abbr = paste(rep(dimnames(xt)$abbr, dim(xt)[3]), 
                      rep(dimnames(xt)$layer, each = dim(xt)[2]), 
                      sep = "@")))
            }
            else {
                res <- matrix(0, ncol = dim(xt)[2], nrow = dim(xt)[1], 
                  dimnames = list(plot = dimnames(xt)$plot, abbr = paste(dimnames(xt)$abbr, 
                    dimnames(xt)$layer, sep = "@")))
            }
            for (i in 1:dim(xt)[3]) {
                sel <- grep(paste("", dimnames(xt)$layer[i], 
                  sep = "@"), dimnames(res)$abbr, fixed = TRUE)
                res[, sel] <- xt[, , i]
            }
            res <- res[, colSums(res) > 0]
            species <- as.data.frame(res)
        }
    })
    if (verbose) {
        cat("\ntime to cast species matrix", "of", prod(dim(res)), 
            "cells:", cpu.time[3], "sec\n")
    }
    if (any(SitesLong(obj)[, 1:3] == "")) {
        obj@sites.long[obj@sites.long == "", 1:3] <- 0
        warning("NAs and empty fields (\"\") in supplied sites", 
            " filled with zeros")
    }
    sites <- reshape(SitesLong(obj)[, 1:3], direction = "wide", 
        timevar = "variable", idvar = "plot")
    if (any(is.na(sites))) {
        sites[is.na(sites)] <- 0
        warning("NAs in casted sites data frame", " filled with zeros", 
            "\nslot @sites.long will no longer match for these entries")
    }
    options(warn = -1)
    sites <- as.data.frame(sapply(sites, function(x) {
        if (!any(is.na(as.numeric(x)))) {
            x <- as.numeric(x)
        }
        else {
            x <- as.character(x)
        }
    }, simplify = FALSE), stringsAsFactors = FALSE)
    options(warn = 0)
    names(sites) <- gsub("value.", "", names(sites), fixed = TRUE)
    rownames(sites) <- sites$plot
    sites <- sites[, -grep("plot", names(sites))]
    sites <- sites[match(rownames(species), rownames(sites)), 
        ]
    res <- new("VegsoupData", species = species, sites = sites, 
        taxonomy = obj@taxonomy, layers = obj@layers, species.long = obj@species.long, 
        group = obj@group, sites.long = obj@sites.long, scale = obj@scale, 
        sp.points = obj@sp.points, sp.polygons = obj@sp.polygons)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
