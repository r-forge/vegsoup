setwd("~/Documents/vegsoup/pkg/R")

objs <- system("ls", intern = TRUE)

package.skeleton("vegsoup", code_files = objs,
	path = "/Users/roli/Desktop/vegsoup",
	namespace = TRUE, force = TRUE)