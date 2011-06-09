k.range <- 1:3

part.seq <- sapply(k.range, function (x) {
	vegbase.partition(spc, k = x,
	method = "agnes", binary = TRUE, dist = "bray")
	}, simplify = FALSE)

n.sig.spc <- sapply(part.seq,
	function (x) nrow(summary(slot(x, "tabdev"))))
tot.dev <- 	sapply(part.seq,
	function (x) slot(x, "tabdev")$totdev)


par(mfrow = c(2,2))

plot(n.sig.spc ~ k.range, type = "b")
plot(tot.dev ~ k.range, type = "b")
plot(n.sig.spc ~ tot.dev, type = "b")
	