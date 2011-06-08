library(mvpart)
library(maptree)

#	take subset
spc <- species[tree,]
sts <- sites[tree,]

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

#	tree models

notrun = FALSE

dis <- gdist(spc@vdm, meth="bray", full = TRUE, sq = TRUE)
fit <- mvpart(dis ~ elevation+relief+vegetation+substrate+stones+tree.diameter+tree.twig+bark+expo+slope,
	sts@raw, method = "dist")

pdf("./pdf/rpart_trees.pdf",
	width = 29.7/2.54, height = 21/2.54)
#fit <- clip.rpart(fit, best = 30)
draw.tree(fit,
	nodeinfo = TRUE, digits = 2, cex = 0.5)
dev.off()

part <- fit$frame
part$var <- as.character(part$var)
part$var[grep("leaf", part$var)] <-
	seq(along = grep("leaf", part$var))

part <- as.numeric(part[fit$where,]$var)
names(part) <- names(fit$where)

partition <- vegbase.partition(spc,
	method = "external", clustering = part)

vegbasePartitionSpreadHeatmap(partition,
	margins = c(1, 4))
	
#	create Latex tables
setwd("~/Documents/Rpackages/vegbase/debug")

table.method = "dca"
abundant.2.top = TRUE
tag.species = TRUE
order.layer = FALSE
tag.treshold = 1
abundance.treshold = 0

source("lichen_tables.R")