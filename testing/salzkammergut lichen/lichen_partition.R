#	take subset
spc <- species[cal,]
sts <- sites[cal,]

#	drop single species plots
drop <- apply(bin(spc), 1, sum) > 1
spc <- spc[drop,]
sts <- sts[drop,]

#	drop rare species
drop <- apply(bin(spc), 2, sum) > 2
spc <- spc[,drop]

drop <- apply(bin(spc), 1, sum) > 1
spc <- spc[drop,]

drop <- match(rownames(spc), rownames(sts))
sts <- sts[drop,]

all.equal(rownames(spc), rownames(sts))

k <- 1:30

spc.duleg <- vegbase.isamic.duleg(spc, max(k), dist = "bray",
	method = "agnes", indicator = "duleg")
spc.isamic <- vegbase.isamic.duleg(spc, max(k), dist = "bray",
	method = "agnes", indicator = "isamic")
	
spc.indpower <- indpower(spc@pa)
diag(spc.indpower) <- NA
spc.indpower <- rowMeans(spc.indpower, na.rm = TRUE)

partitions <- sapply(k, function (x) {
	vegbase.partition(spc, k = x,
	method = "agnes", binary = TRUE, dist = "bray")
	}, simplify = FALSE)

pdf("./pdf/cal_k25.pdf",
	width = 21/2.54, height = 29.7/2.54)
plot(spc.duleg)
plot(spc.isamic)
vegbasePartitionPlotTabdev(partitions)
hm <- vegbasePartitionSpreadHeatmap(partitions[[10]],
	margins = c(2, 4))
dev.off()

partition <- partitions[[25]]
#	cal 25 or 6, drop > 2
#	tree 5, drop > 3
#	twig 6, drop > 1
#	ground 7, drop > 1

#	create Latex tables
setwd("~/Documents/Rpackages/vegbase/debug")

table.method = "agnes"
abundant.2.top = TRUE
tag.species = TRUE
order.layer = FALSE
tag.treshold = 1
abundance.treshold = 0

source("lichen_tables.R")

