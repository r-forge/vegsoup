cap.mod.0 <- capscale(wisconsin(species@pa) ~ 1,
	distance = "bray", add = TRUE)
	
#cap.mod.1 <- capscale(wisconsin(species@vdm) ~ tree.twig,
#	data = sites@raw,
#	distance = "bray", add = TRUE)	

cap.mod <- cap.mod.0.pa

pdf("./pdf/ord.pdf",
	width = 21/2.54, height = 29.7/2.54)
	
par(mfrow = c(2,2))

fig <- ordiplot(cap.mod, type = "n", choices = c(1,2),
	main = "1-2")
#plot.grps(fig, 6)
points(fig, what = "sites", select = ground,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)


fig <- ordiplot(cap.mod, type = "n", choices = c(1,3),
		main = "2-3")
#plot.grps(fig, 6)
points(fig, what = "sites", select = ground,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)

fig <- ordiplot(cap.mod, type = "n", choices = c(2,3),
		main = "2-3")
#plot.grps(fig, 6)
points(fig, what = "sites", select = ground,
	pch = 16, col = rgb(1,0,0,0.2))
points(fig, what = "sites", select = tree,
		pch = 16, col = rgb(0,0,0,0.2))
points(fig, what = "sites", select = twig,
		pch = 1)

fig <- ordiplot3d(cap.mod, type = "n", main = "1-2-3")
xyz <- scores(cap.mod, choices =1:3, display = "sites")
fig$points3d(xyz[ground,1],xyz[ground,2],xyz[ground,3],
	pch = 16, col = rgb(1,0,0,0.2))
fig$points3d(xyz[tree,1],xyz[tree,2],xyz[tree,3],
	pch = 16, col = rgb(0,0,0,0.2))
fig$points3d(xyz[twig,1],xyz[twig,2],xyz[twig,3],
	pch = 1)
	
dev.off()

#	not used
plot.grps <- function (x, y)
{
	grps <- as.numeric(factor
		(as.character(sites@raw[y][,1])))
	for (i in 1:length(unique(grps)))
	{
	ordiellipse(x, groups = grps,
		show.groups = i, col = i)
	}
	grps
}