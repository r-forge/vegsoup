R CMD build --no-build-vignettes /Users/roli/Documents/vegsoup/pkg
R CMD check vegsoup_0.2-6.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.3/library vegsoup_0.2-6.tar.gz

#	MacTex needs packages 'hanging', 'inconsolata' and 'helvetic'
#	tools::showNonASCII(readLines())
