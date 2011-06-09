#	reshape tables where layers are in seperate columns

ReshapeMultiCoverColumns <- function (filename) {

res <- read.csv2(filename, colClasses = "character")


res <- rbind(
	cbind("hl", as.matrix(res[,c(1,2,3)])),
	cbind("sl", as.matrix(res[,c(1,2,4)])),
	cbind("tl", as.matrix(res[,c(1,2,5)])),
	cbind("ml", as.matrix(res[,c(1,2,6)])))	


res <- as.data.frame(res,
	stringsAsFactors = FALSE)
res <- res[,c(2,3,1,4)]
names(res) <- c("plot", "abbr", "layer", "cov")

res <- res[res$cov != "0",]
res <- res[res$cov != "",]


}
#res <- ReshapeMultiCoverColumns("/Users/roli/Desktop/db/relevees/species.csv")
#write.csv2(res, "foo.csv")