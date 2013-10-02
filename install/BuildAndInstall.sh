R CMD build --no-vignettes /Users/roli/Documents/vegsoup/pkg
R CMD INSTALL -d -l /Users/roli/Library/R/3.0/library vegsoup_0.1-9.tar.gz
#
# --fake		do minimal install for testing purposes, this will not install extdata!

