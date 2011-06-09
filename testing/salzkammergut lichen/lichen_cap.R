library(vegan)

#	plotting fucntion
plot.grps <- function (x, y)
{
	grps <- as.numeric(factor
		(as.character(sts@raw[y][,1])))
	for (i in 1:length(unique(grps)))
	{
	ordiellipse(x, groups = grps,
		show.groups = i, col = i)
	}
	grps
}


#	take subset
spc <- species[cal,]
sts <- sites[cal,]

#	drop single species plots
drop <- apply(spc@pa, 1, sum) > 1
spc <- spc[drop,]
sts <- sts[drop,]

#	mds.mod <- metaMDS(spc@pa, zerodist ="add")
cap.mod.1 <- capscale(wisconsin(spc@pa) ~
	expo + relief + stones + elevation + slope,
	data = sts@raw,
	distance = "bray", add = TRUE)
cap.mod.2 <- capscale(wisconsin(spc@pa) ~
	expo + relief,
	data = sts@raw,
	distance = "bray", add = TRUE)
anova(cap.mod.1, by = "terms", permu = 200)
anova(cap.mod.2, by = "terms", permu = 200)

par(mfrow = c(2,2))

fig <- ordiplot(cap.mod, type = "n", choices = c(1,2),
	main = "1-2")
points(fig, what = "sites", select = other,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)

fig <- ordiplot(cap.mod, type = "n", choices = c(1,3),
		main = "2-3")
points(fig, what = "sites", select = other,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)

fig <- ordiplot(cap.mod, type = "n", choices = c(2,3),
		main = "2-3")
points(fig, what = "sites", select = other,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)

fig <- ordiplot3d(cap.mod, type = "n", main = "1-2-3")
xyz <- scores(cap.mod, choices =1:3, display = "sites")
fig$points3d(xyz[other,1],xyz[other,2],xyz[other,3],
	pch = 16, col = rgb(1,0,0,0.2))
fig$points3d(xyz[tree,1],xyz[tree,2],xyz[tree,3],
	pch = 16, col = rgb(0,0,0,0.2))
fig$points3d(xyz[twig,1],xyz[twig,2],xyz[twig,3],
	pch = 1)		
