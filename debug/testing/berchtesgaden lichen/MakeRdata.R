setwd("~/Dropbox/Rpackages/vegsoup/debug/testing/berchtesgaden lichen dta")

species <- read.csv2("species.csv", stringsAsFactors = FALSE)
species <- species[c("plot", "abbr","layer", "cov")]
taxonomy <- read.csv2("taxonomy.csv", stringsAsFactors = FALSE)
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

#	recode charcter strings
#	bark
tmp <- sites[sites$variable == "bark",]
codes <- read.csv2("bark codes.csv",
	stringsAsFactors = FALSE)
#	empty code, maybe a missing value?
codes$code[is.na(codes$code)] <- ""

#	reorder to match default sorting of factor
codes <- codes[match(levels(factor(tmp$value)), codes$code), ]
tmp$value <- as.character(factor(tmp$value,
	labels = codes$label))
sites[sites$variable == "bark",] <- tmp

#	vegetation
tmp <- sites[sites$variable == "vegetation",]
codes <- read.csv2("vegetation codes.csv",
	stringsAsFactors = FALSE)
#	empty code, maybe a missing value?
codes$code[is.na(codes$code)] <- ""
#	reorder to match default sorting of factor
codes <- codes[match(levels(factor(tmp$value)), codes$code), ]

tmp$value <- as.character(factor(tmp$value,
	labels = codes$label))
sites[sites$variable == "vegetation",] <- tmp

#	forest type
tmp <- sites[sites$variable == "forest.type",]
codes <- read.csv2("foresttype codes.csv",
	stringsAsFactors = FALSE)
#	empty code, maybe a missing value?
codes$code[is.na(codes$code)] <- ""
#	reorder to match default sorting of factor
codes <- codes[match(levels(factor(tmp$value)), codes$code), ]

tmp$value <- as.character(factor(tmp$value,
	labels = codes$label))
sites[sites$variable == "forest.type",] <- tmp

#	geomorphology
tmp <- sites[sites$variable == "geomorphology",]
codes <- read.csv2("geomorphology codes.csv",
	stringsAsFactors = FALSE)
#	empty code, maybe a missing value?
codes$code[is.na(codes$code)] <- ""
#	reorder to match default sorting of factor
codes <- codes[match(levels(factor(tmp$value)), codes$code), ]

tmp$value <- as.character(factor(tmp$value,
	labels = codes$label))
sites[sites$variable == "geomorphology",] <- tmp

#	check any remaining missing values
#for (i in unique(sites$variable)) {
#	tmp <- sites[sites$variable == i,]
#	cat(i, "\n")
#	print(table(tmp$value))
#}

#	combine moos (ml) and lichen (ll) layer
#	species$layer <- "kl"

save(species, file = "species.Rdata")
save(sites, file = "sites.Rdata")
save(taxonomy, file = "taxonomy.Rdata")

system("cp species.Rdata ../../species.Rdata")
system("cp sites.Rdata ../../sites.Rdata")
system("cp taxonomy.Rdata ../../taxonomy.Rdata")