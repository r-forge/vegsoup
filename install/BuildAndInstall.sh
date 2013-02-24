R CMD build  /Users/roli/Documents/vegsoup/pkg
R CMD INSTALL -d -l /Users/roli/Library/R/2.15/library vegsoup_0.1-7.tar.gz
#--no-vignettes
# --fake		do minimal install for testing purposes, this will not install extdata!

