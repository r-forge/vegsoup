#	stack sites data frame to match database structure

SitesWide2SitesLong <- function (filename) {

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

sites <- stack.sites("/Users/roli/Documents/vegsoup/testing/bad bruck dta/sites.csv")

write.csv2(sites, "/Users/roli/Documents/vegsoup/testing/bad bruck dta/sites2.csv")