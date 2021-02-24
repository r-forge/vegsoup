#	compare results to original implementation
#	http://www.sci.muni.cz/botany/juice/R/classification%20stability.r

require(vegsoup)

k <- 5
nitr <- 200
nitr.lambda <- 10
seed <- 1234

x <- VegsoupPartition(coenoflex(seed = 1234), method = "wards", k = k)
JUICE.table <- as.numeric(x)

set.seed(seed)

#	original implementation
#   ---------------------------------------------------------
#   ---         Function Classification Stability         ---
#   ---------------------------------------------------------

   library(vegan)

#   ---------------------------------------------------------

   no.cycles<-nitr
   no.clusters<-k

   pb <- txtProgressBar (min = 1, max = no.cycles, char = '.',
        width = 45, style = 3, title = 'Calculation progress')

#  ..........................................................
#  ---                  Wards method                      ---

   sih<-vegdist(JUICE.table,method='euclidean')	
   prvni<-cutree(hclust(sih,method='ward.D'),no.clusters)
	#	test
	#	all(prvni == partitioning(x))
	
	prv<-as.matrix(data.frame(prvni))
	plot.no<-dim(JUICE.table)[1]
	spec.no<-dim(JUICE.table)[2]
	lambda <- 0
	rnd_lambda<-0
	lb<-vector (mode = 'numeric', length = no.cycles)

	for (cycle in 1:no.cycles) {

#   ---------------------------------------------------------
#   ---  Subset selection without replacement             ---
#   ---------------------------------------------------------

		q<-c(1:plot.no)
   		r<-sample(q,replace = T)
   		s<-sort(r)
   		t<-unique(s)
   		JUICE.subtable <- JUICE.table[t,]

#   ---------------------------------------------------------
#   ---         Removal of empty species                  ---
#   ---------------------------------------------------------

   		JUICE.sstable <- JUICE.subtable[, colSums(JUICE.subtable) > 0]
   		subplot.no <- dim(JUICE.sstable)[1]
   		subspec.no <- dim(JUICE.sstable)[2]

#  ..........................................................
#  ---                  Wards method                      ---
#  ..........................................................

   		sih<-vegdist(JUICE.sstable,method='euclidean')	
   		druha<-cutree(hclust(sih,method='ward.D'),no.clusters)

   		dru<-as.matrix(data.frame(druha))

#   ---------------------------------------------------------
#   ---         Crosstabulation                           ---
#   ---------------------------------------------------------

   		crosstab<-array(0,dim=c(no.clusters,no.clusters))
   		crossran<-array(0,dim=c(no.clusters,no.clusters))
   		prv.temp <- prv[rownames (prv) %in% rownames (dru),]
   		crosstab <- table (prv.temp, dru)


#   ---------------------------------------------------------
#   ---         Raw lambda calculation                    ---
#   ---------------------------------------------------------

   		crossR<-apply(crosstab,1,function(x) max(x))
   		crossC<-apply(crosstab,2,function(x) max(x))
   		F1<-sum(crossR)
   		F2<-sum(crossC)
   		F5<-2 * sum(crosstab)
   		F3<-max(rowSums(crosstab))
   		F4<-max(colSums(crosstab))
   		lb[cycle]=(F1+F2-F3-F4)/(F5-F3-F4)

#   ---------------------------------------------------------
#   ---         Random lambda calculation (10 times more) ---
#   ---------------------------------------------------------

		for (duna in 1:nitr.lambda) # was 10
			{
   			a<-factor(sample(1:no.clusters,subplot.no, replace=T),levels=1:no.clusters)
   			b<-factor(sample(1:no.clusters,subplot.no, replace=T),levels=1:no.clusters)
   			tab=table(a,b)
   			crossR<-apply(tab,1,function(x) max(x))
   			crossC<-apply(tab,2,function(x) max(x))
   			F1<-sum(crossR)
   			F2<-sum(crossC)
   			F5<-2 * sum(tab)
   			F3<-max(rowSums(tab))
   			F4<-max(colSums(tab))
   			rnd_lambda=rnd_lambda+(F1+F2-F3-F4)/(F5-F3-F4)
			}
		}


#   ---------------------------------------------------------
#   ---         Final modified lambda calculation         ---
#   ---------------------------------------------------------

   	lambda=sum(lb)/no.cycles
   	rnd_lambda=rnd_lambda/(no.cycles*nitr.lambda) # was 10
   	mod_lambda=(lambda-rnd_lambda)/(1-rnd_lambda)
	r1 <- list(lambda = round(lambda, 3),
		modified.lambda = round(mod_lambda, 3),
		random.lambda = round(rnd_lambda, 3))
#   ---------------------------------------------------------

#	vegsoup implementation
r2 <- stable(x, nitr = nitr, nitr.lambda = nitr.lambda, seed = seed)

test <- all.equal(r1, r2)

test