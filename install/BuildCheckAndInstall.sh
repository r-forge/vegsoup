R CMD build /Users/roli/Documents/vegsoup/pkg
R CMD check vegsoup_0.2-5.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.2/library vegsoup_0.2-5.tar.gz

#	tools::showNonASCII(readLines())
