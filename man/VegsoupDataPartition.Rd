\name{VegsoupDataPartition}
\alias{VegsoupDataPartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VegsoupDataPartition(x, binary, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external"), dist = "bray", clustering, polish = FALSE, nitr = 999, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{binary}{
%%     ~~Describe \code{binary} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
  \item{clustering}{
%%     ~~Describe \code{clustering} here~~
}
  \item{polish}{
%%     ~~Describe \code{polish} here~~
}
  \item{nitr}{
%%     ~~Describe \code{nitr} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, binary, k, method = c("ward", "flexible", "pam", 
    "isopam", "kmeans", "optpart", "wards", "external"), dist = "bray", 
    clustering, polish = FALSE, nitr = 999, verbose = FALSE, 
    ...) 
{
    debug = FALSE
    if (debug) {
        x = dta
        binary = TRUE
        k = 3
        method = "isopam"
        dist = "bray"
        nitr = 99
        polish = TRUE
    }
    else {
        if (!inherits(x, "VegsoupData")) 
            stop("Need object of class VegsoupData")
        if (missing(k)) {
            k <- 1
            warning("argument k missing, set to ", k)
        }
        if (missing(binary) && method != "external") 
            if (verbose) 
                cat("... Set to binary")
        binary = TRUE
        if (missing(k) && missing(clustering)) 
            stop("Need a value of k or optional clustering vetcor")
        if (missing(method)) {
            part.meth <- "flexible"
            if (verbose) 
                cat("... Set default option", part.meth)
        }
        else {
            part.meth <- match.arg(method)
        }
        if (!missing(clustering) && method == "external") {
            if (length(clustering == nrow(x@species.pa))) {
                k <- length(unique(clustering))
                if (verbose) 
                  cat("... Use supplied vector,", iflese(is.intger(k), 
                    k, as.intger(k)), "for partitioning")
            }
            else {
                stop("... Length of clustering vector and matrix must match", 
                  dim(x), length(clustering))
            }
        }
        if (!binary) {
            dis <- vegdist(wisconsin(as.numeric(x)), dist)
        }
        else {
            dis <- vegdist(wisconsin(as.binary(x)), dist)
        }
    }
    switch(part.meth, ward = {
        part <- agnes(dis, method = "ward", ...)
    }, flexible = {
        alpha <- 0.625
        beta = 1 - 2 * alpha
        part <- agnes(dis, method = "flexible", par.meth = c(alpha, 
            alpha, beta, 0), ...)
    }, pam = {
        if (verbose) cat("\nrun pam")
        part <- pam(dis, k = k, diss = TRUE, ...)
    }, isopam = {
        if (verbose) cat("\nrun isopam, ignoring k=", k)
        if (verbose) cat("\nplease supply c.fix to restict to a specific number of partitions\n")
        if (binary) tmp <- as.binary(x) else tmp <- as.numeric(x)
        part <- isopam(tmp, distance = dist, ...)
    }, optpart = {
        if (verbose) cat("\nrun optpart from random starts ...")
        if (verbose) cat("\nset to", k, "partitions\n")
        part <- .VegsoupDataPartitionOptpartBestopt(dis, k, numitr = 100, 
            ...)
    }, kmeans = {
        part <- kmeans(as.binary(x), centers = k, nstart = 25)
    }, wards = {
        part <- hclust(dis, method = "ward", ...)
    }, external = {
        part <- clustering
    })
    if (inherits(part, "agnes") | inherits(part, "hclust")) {
        grp <- cutree(part, k)
        names(grp) <- rownames(x)
    }
    if (inherits(part, "kmeans")) {
        grp <- part$cluster
    }
    if (inherits(part, "pam")) {
        grp <- part$clustering
    }
    if (inherits(part, "isopam")) {
        if (is.null(part$hier)) {
            if (verbose) 
                cat("no hierarchy estimated by isopam")
            grp <- part$flat
            k <- length(unique(part$flat))
        }
        else {
            if (verbose) 
                cat("retieve lowest hierachy level")
            grp <- part$flat[[ncol(part$hier)]]
            k <- length(unique(grp))
        }
    }
    if (inherits(part, "partana")) {
        grp <- part$clustering
        names(grp) <- rownames(x)
    }
    if (is.vector(part)) {
        grp <- as.numeric(part)
        names(grp) <- rownames(x)
    }
    if (k != length(unique(grp)) && class(part) != "isopam") {
        warning("did not converge for", k, "partitions", "\nset k to", 
            length(unique(grp)))
    }
    out.grp <- any(as.vector(table(grp)) == 1)
    if (out.grp) {
        warning("single member groups detected!")
    }
    if (out.grp || polish) {
        if (verbose) 
            cat("\n... try to resolve using function optsil")
        grp.opt <- optsil(grp, dis, k^2)$clustering
        names(grp.opt) <- rownames(x)
        if (any(as.vector(table(grp.opt)) == 1)) {
            warning("\ndid not succeed in reallocation")
        }
        else {
            method <- c(method, "optsil")
            grp <- grp.opt
            cat("\n... successfully 'polished' clustering")
            if (k != length(unique(grp))) {
                cat("\n... reset intial k =", k, "to k =", length(unique(grp)))
            }
        }
    }
    res <- new("VegsoupDataPartition", part = grp, method = part.meth, 
        k = length(unique(grp)), dist = dist, binary = binary, 
        species = as.character(x), taxonomy = Taxonomy(x), species.long = SpeciesLong(x), 
        sites.long = SitesLong(x), sites = Sites(x), sp.points = SpatialPointsVegsoup(x), 
        sp.polygons = SpatialPolygonsVegsoup(x), group = AprioriGrouping(x), 
        scale = AbundanceScale(x), layers = Layers(x))
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
