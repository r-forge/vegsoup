R CMD build /Users/roli/Documents/vegsoup/pkg
R CMD check vegsoup_0.2-2.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.1/library vegsoup_0.2-2.tar.gz
