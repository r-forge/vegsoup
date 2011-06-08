#	format and arrange Fischer test table
.FisherTestTableVegsoupPartition <- function (obj, method = "r.g") {
	
if (getK(obj) == 1) {
	stop("meaningless with k = ", getK(obj))
}	

#	obj = prt
p.max = .05
cnti <- Contingency(obj)
cnst <- Constancy(obj)
nc <- ncol(cnst)
SP <- ncol(obj)
#	ft <- FisherTest(obj)	# depreciated
ft <- Fidelity(obj, "Fisher", alternative = "two.sided")@stat
N <- nrow(obj)
frq <- colSums(getBin(obj))
siz <- table(Partitioning(obj))  

#	automatic guess adapted from isopam()
phi.min <- round (0.483709 + nc * -0.003272 + N * -0.000489 + 
      SP * 0.000384 + sqrt (nc) * -0.01475, 2) 

symb <- ft
symb[ft > 0.05] <- ""
symb[ft <= 0.05] <- "*"
symb[ft <= 0.01] <- "**"
symb[ft <= 0.001] <- "***"

#	combine frequency table with significance symbols
frq.ft <- matrix(paste(cnst, symb, sep = ""), 
	nrow = nrow(cnst), ncol = ncol(cnst))
class(frq.ft)
head(frq.ft)
frq.ft <- data.frame(frq.ft)
colnames(frq.ft) <- unique(Partitioning(obj))
rownames(frq.ft) <- names(obj)
  
#	standardized phi
#	phi <- Phi(obj)	# depreciated
phi <- Fidelity(obj, method)@stat # method defaults to "r.g"

#	sort table
phi.idx <- apply(phi, 1, which.max)	# group association by phi 
frq.ord <- phi.idx

for (i in 1:length(frq.ord)) {
	frq.ord[i] <- cnst[i, phi.idx [i]]
}	

frq.top <- as.matrix(frq)[order (phi.idx, -frq.ord),] ## Sorting
ord.top <- names(frq.top)
frq.ft.top <- frq.ft[ord.top,]
ft <- ft[ord.top,]
phi <- phi[ord.top,]

#	Filter diagnostic species
filter1 <- apply(ft, 1, min) <= p.max                                          
filter2 <- apply(phi, 1, max) >= phi.min
dia <- which (filter1 [filter2 == TRUE] == TRUE) ## diagnostic species 
n.dia <- length (dia) ## how many diagnostic species
if (n.dia == 0) diag <- "No diagnostic species with given thresholds." 
if (n.dia > 0) diag <- frq.ft.top [names (dia),]

#	For later use in the bottom part of the tables
ord.bot <- names ( as.matrix(frq) [order (-frq),])
frq.ft.b <- frq.ft [ord.bot,]
    
#	move diagnostic species to top
if (n.dia > 0) {
	FRQ <- rbind (diag, frq.ft.b [rownames (frq.ft.b) %in%
		rownames (diag) == FALSE,])
} else {
	FRQ <- frq.ft.b
}

#	info about diagnostic species
dig1 <- phi.idx[names(phi.idx) %in% names (dia)]
dig2 <- dig1[rownames(diag)]
typ <- list ()
for (i in 1:nc) {
	if (length(names(dig2)[dig2 == i]) > 0) {
		typ [i] <- paste(names(dig2)[dig2 == i], collapse = ', ')
	} else {
		typ [i] <- 'Nothing particularly typical'
	}	
}
names (typ) <- colnames(cnst)

res <- list(tab = FRQ, typical = typ)

return(res)
}

setGeneric("FisherTestTable",
	function (obj, ...)
		standardGeneric("FisherTestTable")
)
setMethod("FisherTestTable",
	signature(obj = "VegsoupDataPartition"),
	.FisherTestTableVegsoupPartition
) 