library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

plot(dta)
plotPCO(dta)

prt <- VegsoupPartition(dta, k = 2, method = "flexible")
plot(prt)
