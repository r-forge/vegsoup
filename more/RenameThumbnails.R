kw <- read.csv2("/Volumes/roli/Desktop/jpg/keywords.csv", stringsAsFactors = FALSE)

kw[,1] <- as.character(kw[,1])
kw[,2] <- as.character(kw[,2])
kw[,2] <- sapply(strsplit(kw[,2], ",", fixed = TRUE), function (x) x[3])

kw <- apply(kw, 1, function (x) paste("mv /Volumes/roli/Desktop/jpg/",
	gsub(" ", "\\ ", x[1], fixed = TRUE), ".jpg ", # if there are blanks in filenames
	"/Volumes/roli/Desktop/jpg/", x[2], ".jpg", sep = ""))
	
sapply(kw, system)	