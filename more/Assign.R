function (obj, )

library(vegsoup)

#?vegclass
data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))
Layers(dta)
dta <- Layers(dta, aggregate = "layer",
	collapse = c("hl", NA, "sl", "tl", "tl"), verbose = TRUE)
Layers(dta)

require(vegclust)
#	classified samples
dta.c <- dta[dta$abbr != "0", ]

#	not classified samples
dta.n <- dta[dta$abbr == "0", ]

class <- as.factor(dta.c$abbr)

vc <- as.vegclust(as.logical(dta.c),
	cluster = as.numeric(class))

cls <- vegclass(vc, as.logical(dta.n))
res1 <- cls$memb
names(res1) <- levels(class)

res1 <- t(sapply(1:nrow(res1), function (x) {
	c(rownames(res1)[x], names(res1)[res1[x, ] == 1])
}))

#	based on distances
vc <- as.vegclust(vegdist(as.logical(dta.c)),
	cluster = as.numeric(class))

#	a data frame containing the distances between the new sites in rows
#	and the old (classified) sites in columns

n <- as.data.frame(as.matrix(vegdist(as.logical(dta))))
n <- n[rownames(n) %in% rownames(dta.n), ]
n <- n[, colnames(n) %in% rownames(dta.c)]

cls <- vegclass(vc, n)
res2 <- cls$memb
names(res2) <- levels(class)

res2 <- t(sapply(1:nrow(res2), function (x) {
	c(rownames(res2)[x], names(res2)[res2[x, ] == 1])
}))

