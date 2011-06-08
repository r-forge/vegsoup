setwd("~/Documents/Rpackages/vegbase/debug/testing/weilhartsforst add dta")

species <- read.csv2("species.csv", stringsAsFactors = FALSE)
species <- species[c("plot", "abbr","layer", "cov")]
taxonomy <- read.csv2("/Users/roli/Documents/vegbase standards/austrian standard list 2008/austrian standard list 2008.csv", stringsAsFactors = FALSE)
taxonomy <- taxonomy[c("abbr", "taxon")]
sites <- read.csv2("sites.csv", stringsAsFactors = FALSE)

sites[is.na(sites)] <- ""
test <- merge(species, taxonomy,
	by.x = "abbr", by.y = "abbr",
	all.x = TRUE)

test <- test[apply(is.na(test), 1, any),]
if (dim(test)[1] < 1)
{
	cat("\nabbreviations checked")
} else {
	print(test)
}

dupl <- dim(species)[1] - dim(unique(species[,c(1:3)]))[1]

if (dupl > 0)
{
	cat("\nspecies data not unique for", dupl, "sample(s)")
	cat("\nremoved duplicted sample:\n\n")
	print(species[duplicated(species[,c(1:3)]),])
	species <- species[!duplicated(species[,c(1:3)]),]
} else {
	cat("\nno duplicates found")
}
if (any(species$cov == "") || is.na(species$cov))
{
	cat("\nmissing abandance for observation(s):")
	species[species$cov == "",]
}
species.sites.match <- all.equal(sort(unique(species$plot)),
	sort(unique(sites$plot)))
if (species.sites.match)
{
	cat("\nspecies and sites plot names matching")
} else {
	cat("\nspecies and sites plot do not match")
}

#	stack sites data frame to match database structure

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

save(species, file = "species.Rdata")
save(sites, file = "sites.Rdata")
save(taxonomy, file = "taxonomy.Rdata")

system("cp species.Rdata ../../species.Rdata")
system("cp sites.Rdata ../../sites.Rdata")
system("cp taxonomy.Rdata ../../taxonomy.Rdata")