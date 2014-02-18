R CMD build --no-build-vignettes /Users/roli/Documents/vegsoup/pkg
R CMD INSTALL -d -l /Users/roli/Library/R/3.0/library vegsoup_0.2-0.tar.gz
#
# --fake		do minimal install for testing purposes, this will not install extdata!
# for wiondows use:
# pkgs <- c("vegan", "cluster", "coenoflex", "optpart", "vegclust", "sp", "raster", "multitable",
# "labdsv", "pbapply", "Hmisc", "spatstat", "rgdal", "RColorBrewer", "stringr", "googleVis",
# "gclus", "misc3d", "multicore", "maptools", "isopam", "ggmap", "geonames", "mefa", "maps",
# "mapdata", "Taxonstand")

# install.packages(pkgs)

# at the time of this writing
# R CMD INSTALL coenoflex_2.0-1.tar.gz

# R CMD build --no-build-vignettes vegsoup\pkg
# R CMD INSTALL vegsoup_0.2-0.tar.gz

