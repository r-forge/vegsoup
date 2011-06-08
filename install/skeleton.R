setwd("~/Documents/vegsoup/pkg/R")

objs <- system("ls", intern = TRUE)

package.skeleton("vegsoup", code_files = objs,
	path = "/Users/roli/Desktop/",
	namespace = TRUE, force = TRUE)