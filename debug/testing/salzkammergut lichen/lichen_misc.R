tree.subs <- sites[tree,c("substrate", "richness")]@raw
tree.subs$richness <- as.numeric(tree.subs$richness)
library(lattice)

tree.subs <- aggregate(tree.subs$richness,
	by = list(tree.subs$substrate), mean)
names(tree.subs) <- c("substrate", "richness")

tree.subs <- tree.subs[order(tree.subs$richness),]
dotchart(tree.subs$richness,
	labels = tree.subs$substrate, 
	gdata = tree.subs$substrate)

