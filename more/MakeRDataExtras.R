#	stack sites data frame to match database structure

stack.sites <- function (filename) {

sites <- read.csv2(filename, colClasses = "character")

sites.stack <- stack(sites)
sites.stack[,1] <- as.character(sites.stack[,1])
sites.stack[,2] <- as.character(sites.stack[,2])
plot <- sites.stack[sites.stack$ind == "plot",]$values
plot <- rep(plot, (nrow(sites.stack)/length(plot))- 1)
sites.stack <- sites.stack[!sites.stack$ind == "plot",]
sites.stack <- data.frame(plot,
	variable = sites.stack[,2],
	value = sites.stack[,1])
sites.stack <- sites.stack[order(sites.stack$plot),]
sites.stack[is.na(sites.stack)] <- ""
rownames(sites.stack) <- 1:nrow(sites.stack)
sites <- sites.stack
return(sites)
}

#sites <- stack.sites("/Users/roli/Dropbox/Rpackages/vegsoup/debug/testing/amadeus dta/sites.csv")

#write.csv2(sites, "/Users/roli/Dropbox/Rpackages/vegsoup/debug/testing/amadeus dta/sites2.csv")

#	function to construct abbreviation
#	from full taxon names, very basic!

#	old colums style to new style
#	tschirgant
#	diploma
df <- read.csv2("/Users/roli/Dropbox/Rpackages/vegsoup/debug/testing/bitzenberg dta/species.csv", colClasses = "character")


df <- rbind(
	cbind("hl", as.matrix(df[,c(1,2,3)])),
	cbind("sl", as.matrix(df[,c(1,2,4)])),
	cbind("tl", as.matrix(df[,c(1,2,5)])),
	cbind("ml", as.matrix(df[,c(1,2,6)])))	
df <- df[df[,4] != "",]

write.csv2(df, "foo.csv")	

MakeAbbr <- function (x) 
{
	 x = tb$taxon
    names <- make.names(x, unique = FALSE)
    names <- lapply(strsplit(names, "\\."),
    function(x) if (length(x) > 1)
        substring(x, 1, 4)
    else x)
    names <- unlist(lapply(names,
    	function (x) if (length(x) > 1) 
        paste(x[seq(1, length(x))], collapse = " ")
    else x))
    names <- gsub("ssp  ", "", names, fixed = TRUE)
    names <- gsub("var  ", "", names, fixed = TRUE)
    names <- gsub("  ", " ", names, fixed = TRUE)
    names <- abbreviate(names, 8)
   	names <- make.names(names, unique = TRUE)
    names
}