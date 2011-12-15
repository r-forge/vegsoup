kw <- read.csv2("/Users/roli/Desktop/wb/keywords.csv", stringsAsFactors = FALSE)

kw <- apply(kw, 1, function (x) paste("mv /Users/roli/Desktop/wb/", x[1], ".jpg ",
	"/Users/roli/Desktop/wb/", x[2], ".jpg", sep = ""))
	
sapply(kw, system)	