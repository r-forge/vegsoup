ReshapeMultiCoverColumns <- function (filename) {
#	old colums style to new style
#	tschirgant
#	diploma
df <- read.csv2(filename, colClasses = "character")


df <- rbind(
	cbind("hl", as.matrix(df[,c(1,2,3)])),
	cbind("sl", as.matrix(df[,c(1,2,4)])),
	cbind("tl", as.matrix(df[,c(1,2,5)])),
	cbind("ml", as.matrix(df[,c(1,2,6)])))	
df <- df[df[,4] != "",]
}
write.csv2(df, "foo.csv")