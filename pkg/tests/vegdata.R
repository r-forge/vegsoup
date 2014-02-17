library(vegsoup)
library(vegdata)

lc.1 <- data.frame(LAYER = 0:9,
	COMB = c("hl", rep("tl", 3), rep("sl", 2), rep("hl", 4)),
	stringsAsFactors = FALSE)

vd <- tv.veg('taxatest',
	uncertain=list('DET_CERT', data.frame(0:2,c('pres','agg','agg'))),
	pseudo=list(lc.1,'LAYER'), genus = 'delete', spcnames = "short", sysPath=TRUE)


m <- t(vd)
tx <- strsplit(row.names(m), ".", fixed = TRUE)
ab.ly <- t(sapply(tx, function (x) c(abbr = x[1], layer = x[2])))
x <- data.frame(ab.ly, taxon = NA, m,
	check.names = FALSE, stringsAsFactors = FALSE)
x <- stackSpecies(x)
	
y <- tv.site('taxatest')
y <- stackSites(y, schema = "RELEVE_NR")

#	taxonomy
z <- sapply(unique(x$abbr), tax, simplify = FALSE)
z <- do.call(rbind, z)
z <- taxonomy(as.matrix(z[!z[,5], c(2,3)]))

x <- Vegsoup(x, y, z, coverscale = "percentage")
coordinates(x) <- ~ LONGITUDE + LATITUDE
coverscale(x) <- "braun.blanquet2"
