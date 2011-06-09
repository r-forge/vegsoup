setwd("~/Dropbox/Rpackages/vegsoup/debug/testing/klammstein dta")

species <- read.csv2("species.csv", stringsAsFactors = FALSE)
species <- species[c("plot", "abbr","layer", "cov")]
taxonomy <- read.csv2("/Users/roli/Dropbox/vegbase standards/austrian standard list 2008/austrian standard list 2008.csv", stringsAsFactors = FALSE)
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


#	check any remaining missing values
for (i in unique(sites$variable)) {
	tmp <- sites[sites$variable == i,]
	cat(i, "\n")
	print(table(tmp$value))
}

sites$value <- gsub(",", ".", sites$value, fixed = TRUE)

save(species, file = "species.Rdata")
save(sites, file = "sites.Rdata")
save(taxonomy, file = "taxonomy.Rdata")

system("cp species.Rdata ../../species.Rdata")
system("cp sites.Rdata ../../sites.Rdata")
system("cp taxonomy.Rdata ../../taxonomy.Rdata")