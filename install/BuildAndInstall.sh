R CMD build --no-vignettes /Users/roli/Documents/vegsoup/pkg
R CMD INSTALL --fake -d -l /Users/roli/Library/R/2.15/library vegsoup_0.1-6.tar.gz

# --fake		do minimal install for testing purposes

