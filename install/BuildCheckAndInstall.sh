R CMD build --no-vignettes /Users/roli/Documents/vegsoup/pkg
R CMD check vegsoup_0.1-7.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/2.15/library vegsoup_0.1-7.tar.gz
