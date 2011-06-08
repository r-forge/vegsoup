k <- 1:10

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

pdf("./windsfeld_pdf/summary.pdf",
	width = 21/2.54, height = 29.7/2.54)
plot(spc.duleg)
plot(spc.isamic)
vegbasePartitionPlotTabdev(partitions)
hm <- vegbasePartitionSpreadHeatmap(partitions[[2]],
	margins = c(2, 4))
dev.off()

partition <- partitions[[1]]

setwd("~/Documents/Rpackages/vegbase/debug")

x = partition
y = sts
type = "vegbase"
table.method = "packed"
abundant.2.top = TRUE
txpwidth = 50
tag.species = TRUE
order.layer = FALSE
tag.treshold = 0.1
abundance.treshold = 0

source("windsfeld_tables.R")

