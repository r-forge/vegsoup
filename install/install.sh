R CMD build /Users/roli/Documents/vegsoup/pkg
R CMD check vegsoup_0.1-1.tar.gz

R CMD INSTALL -l /Users/roli/Library/R/2.11/library /Users/roli/Documents/vegsoup/pkg

* Run R CMD build to build the package tarball.
* Run R CMD check to check the package tarball.