require(vegsoup)

data(barmstein)
x <- barmstein

plot(x)
plotPCO(x)
plot(VegsoupPartition(x, k = 2, method = "flexible"))

