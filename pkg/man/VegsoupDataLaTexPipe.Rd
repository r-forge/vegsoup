\name{VegsoupDataLaTexPipe}
\alias{VegsoupDataLaTexPipe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VegsoupDataLaTexPipe(x, part, file.name, verbose = TRUE, arrange.method = "dca", split.table = FALSE, abundant.2.top = FALSE, abundance.treshold = 0.5, tag.species = FALSE, order.layer = TRUE, add.rule = TRUE, drop.prefix = FALSE, rotate.labels = TRUE, output.scale = FALSE, txpwidth = 50, pwidth = 1, longtable = TRUE, add.syntaxon = FALSE, apply.coverscale = FALSE, check.out = TRUE, tag.treshold = 3, basic, basic.tex.names, type = "vegbase")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{part}{
%%     ~~Describe \code{part} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{arrange.method}{
%%     ~~Describe \code{arrange.method} here~~
}
  \item{split.table}{
%%     ~~Describe \code{split.table} here~~
}
  \item{abundant.2.top}{
%%     ~~Describe \code{abundant.2.top} here~~
}
  \item{abundance.treshold}{
%%     ~~Describe \code{abundance.treshold} here~~
}
  \item{tag.species}{
%%     ~~Describe \code{tag.species} here~~
}
  \item{order.layer}{
%%     ~~Describe \code{order.layer} here~~
}
  \item{add.rule}{
%%     ~~Describe \code{add.rule} here~~
}
  \item{drop.prefix}{
%%     ~~Describe \code{drop.prefix} here~~
}
  \item{rotate.labels}{
%%     ~~Describe \code{rotate.labels} here~~
}
  \item{output.scale}{
%%     ~~Describe \code{output.scale} here~~
}
  \item{txpwidth}{
%%     ~~Describe \code{txpwidth} here~~
}
  \item{pwidth}{
%%     ~~Describe \code{pwidth} here~~
}
  \item{longtable}{
%%     ~~Describe \code{longtable} here~~
}
  \item{add.syntaxon}{
%%     ~~Describe \code{add.syntaxon} here~~
}
  \item{apply.coverscale}{
%%     ~~Describe \code{apply.coverscale} here~~
}
  \item{check.out}{
%%     ~~Describe \code{check.out} here~~
}
  \item{tag.treshold}{
%%     ~~Describe \code{tag.treshold} here~~
}
  \item{basic}{
%%     ~~Describe \code{basic} here~~
}
  \item{basic.tex.names}{
%%     ~~Describe \code{basic.tex.names} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (x, part, file.name, verbose = TRUE, arrange.method = "dca", 
    split.table = FALSE, abundant.2.top = FALSE, abundance.treshold = 0.5, 
    tag.species = FALSE, order.layer = TRUE, add.rule = TRUE, 
    drop.prefix = FALSE, rotate.labels = TRUE, output.scale = FALSE, 
    txpwidth = 50, pwidth = 1, longtable = TRUE, add.syntaxon = FALSE, 
    apply.coverscale = FALSE, check.out = TRUE, tag.treshold = 3, 
    basic, basic.tex.names, type = "vegbase") 
{
    debug = FALSE
    if (debug) {
        x = prt
        part = 1
        file.name <- "part"
        abundant.2.top = FALSE
        add.rule = FALSE
        order.layer = FALSE
        arrange.method = "dca"
        verbose = TRUE
        tag.species = FALSE
        tag.treshold = 1
        txpwidth = 90
        pwidth = 2
        rotate.labels = TRUE
        abundance.treshold = 0
        type = "vegbase"
        basic = basic
        basic.tex.names = basic.tex.names
        output.scale = FALSE
    }
    else {
        if (missing(x)) 
            stop("\nplease support object of class VegsoupDataPartition")
        if (missing(part)) 
            stop("\nplease support a partition subset")
        if (missing(file.name) || is.null(file.name)) {
            cat("\nUse check out mode on")
            check.out <- TRUE
        }
        else {
            check.out <- FALSE
            file.name <- paste("./Tex/part", part, sep = "")
        }
        if (missing(abundance.treshold)) 
            abundance.treshold <- 0
        if (missing(tag.treshold)) 
            tag.treshold <- abundance.treshold
    }
    config <- .VegsoupDataLaTexConfigure(x, part, type = type, 
        order.layer = order.layer, abundance.treshold = abundance.treshold, 
        abundant.2.top = abundant.2.top, tabdev.table = x@tabdev, 
        spread.list = x@spread, file.name = file.name, basic = basic, 
        basic.tex.names = basic.tex.names)
    tex.drop <- .VegsoupDataLaTexDrop(config, add.rule = add.rule, 
        tag.species = tag.species, output.scale = output.scale, 
        order.layer = order.layer)
    tex.parse <- .VegsoupDataLaTexParse(tex.drop)
    tex.post <- .VegsoupDataLaTexPost(tex.parse, pwidth = pwidth, 
        txpwidth = txpwidth, rotate.labels = rotate.labels, add.rule = add.rule)
    con <- file(paste(tex.post@file.name, ".tex", sep = ""), 
        open = "w")
    writeLines(tex.post@LaTex$sites$sites, con)
    writeLines(tex.post@LaTex$species, con)
    close(con)
    return(invisible(tex.post))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
