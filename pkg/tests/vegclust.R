library(vegsoup)

#?vegclass
data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))

if (FALSE) {
opt <- OptimStride(dta1, 10)
plot(opt)
}

if (FALSE) {
dta1 <- SampleVegsoup(dta, floor(nrow(dta) * 0.80))
dta2 <- dta[!rownames(dta) %in% rownames(dta1), ]
con <- conformveg(as.logical(dta1), as.logical(dta2))
cls1 <- vegclust(as.logical(dta1), mobileCenters=8, dnoise=0.75, method = "KM")
cls <- vegclass(cls1, as.logical(dta2))
cls$memb
defuzzify(cls$memb)$cluster
}

#	subset only classified samples
dta.c <- dta[dta$abbr != "0", ]
sel <- names(table(dta.c$abbr)[table(dta.c$abbr) >= 1])
dta.c <- dta.c[dta.c$abbr %in% sel, ]

#	take random subset
dta1 <- SampleVegsoup(dta.c, floor(nrow(dta.c) * 0.80))
table(dta1$abbr)
#	ensure that we have enough samples per type
#sel <- names(table(dta1$abbr)[table(dta1$abbr) >= 3])
#dta1 <- dta1[dta1$abbr %in% sel, ]
#table(dta1$abbr)

dta2 <- dta.c[!rownames(dta.c) %in% rownames(dta1), ]
table(dta2$abbr)


#	mobileMemb = as.numeric(dta1$abbr == "unc smp")

#vd <- vegclustdist(dist(as.logical(dta1)), method = "KM",
#	fixedMemb = mem, mobileMemb = 2)
#colSums(defuzzify(vd$memb)$memb)

#	existing classification
class <- as.factor(dta1$abbr)

vc <- as.vegclust(as.logical(dta1),
	cluster = as.numeric(class))
cls2 <- vegclass(vc, as.logical(dta2))
res1 <- cls2$memb
names(res1) <- levels(class)

res1 <- t(sapply(1:nrow(res1), function (x) {
	c(rownames(res1)[x], names(res1)[res1[x, ] == 1])
}))

cmp <- cbind(rownames(dta2), dta2$abbr)

cbind(classifed.to = res1[, 2], originally.was = cmp[, 2])




vc <- as.vegclust(vegdist(as.logical(dta1)),
	cluster = as.numeric(class))
cls2 <- vegclass(vc, as.data.frame(as.matrix(vegdist(as.logical(dta2)))) )
defuzzify(cls2$memb)




