library(gdata)

kw <- read.csv2("/Users/roli/Documents/jpg/keywords.csv", stringsAsFactors = FALSE)
kw <- read.xls("/Users/roli/Documents/jpg/keywords.xls")

kw[,1] <- as.character(kw[,1])
kw[,2] <- as.character(kw[,2])
kw[,2] <- sapply(strsplit(kw[,2], ",", fixed = TRUE), function (x) x[2])

kw <- apply(kw, 1, function (x) paste("mv /Users/roli/Documents/jpg/",
	gsub(" ", "\\ ", x[1], fixed = TRUE), ".jpg ", # if there are blanks in filenames
	"/Users/roli/Documents/jpg/", x[2], ".jpg", sep = ""))
	
sapply(kw, system)	