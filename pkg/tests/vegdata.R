library(vegdata)

lc.1 <- data.frame(LAYER = 0:9,
	COMB = c("hl", rep("tl", 3), rep("sl", 2), rep("hl", 4)),
	stringsAsFactors = FALSE)

vd <- tv.veg('taxatest',
	uncertain=list('DET_CERT', data.frame(0:2,c('pres','agg','agg'))),
	pseudo=list(lc.1,'LAYER'), genus = 'delete', spcnames = "numbers", sysPath=TRUE)
sc <- tv.coverperc("taxatest")

names(vd)


m <- t(vd)
tx <- strsplit(row.names(m), ".", fixed = TRUE)

ab.ly <- t(sapply(tx, function (x) c(abbr = x[1], layer = x[2])))

m <- data.frame(ab.ly, comment = NA, m,
	check.names = FALSE, stringsAsFactors = FALSE)
species <- SpeciesWide2SpeciesLong(m)

species$cov <- as.character(
	cut(as.numeric(as.character(species$cov)), right = TRUE,
	breaks = c(0, 1, 2, 3, 13, 38, 68, 88),
	labels = c("r", "+", "1", "2", "3", "4", "5"))
	)
	
sd <- tv.site('taxatest')
names(sd) <- tolower(names(sd))
names(sd)[1] <- "plot"
sites <- SitesWide2SitesLong(sd)

#	taxonomy
tx <- sapply(as.numeric(species$abbr), tax, simplify = FALSE)
names(tx) <- 1:length(tx)
tx <- data.frame(do.call(rbind, tx))
taxonomy <- tx[, c(1,3)]
names(taxonomy) <- c("abbr", "taxon")

#	
species$abbr <- paste("a", species$abbr, sep = "")
taxonomy$abbr <- paste("a", taxonomy$abbr, sep = "")

qry <- Vegsoup(species, sites, taxonomy, scale = list(scale = "Braun-Blanquet 2"))
dta <- VegsoupData(qry)
