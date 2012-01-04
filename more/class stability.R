#   ---------------------------------------------------------
#   ---         Function Classification Stability         ---
#   ---------------------------------------------------------

   library(vegan)
   library(isopam)
   library (tcltk)

#   ---------------------------------------------------------

   no.cycles<-200
   no.clusters<-8

   pb <- txtProgressBar (min = 1, max = no.cycles, char = '.',
        width = 45, style = 3, title = 'Calculation progress')

   setTxtProgressBar (pb, 1)

#  ----------------------------------------------------------
#  ---                                                    ---
#  ---              Table Classification                  ---
#  ---                (Alternatives)                      ---
#  ---                                                    ---
#  ----------------------------------------------------------

#  ..........................................................
#  ---                K-means clustering                  ---

#   prvni<-kmeans(JUICE.table,no.clusters,nstart=25)[1]
#  ..........................................................

#  ..........................................................
#  ---                  Wards method                      ---

   sih<-vegdist(JUICE.table,method='euclidean')	
   prvni<-cutree(hclust(sih,method='ward'),no.clusters)
#  ..........................................................

#  ..........................................................
#  ---             Flexible beta clustering               ---

#    alpha <- 0.625  #    beta = 1-2*alpha
#   sih<-agnes(vegdist(JUICE.table,method='bray'), method='flexible', par.meth=c(alpha,alpha,1-2*alpha,0))
#   prvni<-cutree(as.hclust(sih),no.clusters)
#  ..........................................................

#  ..........................................................
#  ---                        Isopam                     ---

#   prvni<-isopam(JUICE.table,c.fix=no.clusters)[3]
#  ..........................................................

	prv<-as.matrix(data.frame(prvni))
	plot.no <- dim(JUICE.table)[1]
	spec.no <- dim(JUICE.table)[2]
	lambda <- 0
	rnd_lambda <- 0
	lb <- vector (mode = 'numeric', length = no.cycles)

	for (cycle in 1:no.cycles)
	{



	if (round(cycle/10,0) == cycle/10)
		{
		setTxtProgressBar (pb, cycle)

		}

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

#  ----------------------------------------------------------
#  ---                                                    ---
#  ---              Subset classification                 ---
#  ---                (Alternatives)                      ---
#  ---                                                    ---
#  ----------------------------------------------------------

#  ..........................................................
#  ---                K-means clustering                  ---

#   druha<-kmeans(JUICE.sstable,no.clusters,nstart=10)[1]
#  ..........................................................

#  ..........................................................
#  ---                  Wards method                      ---

   sih<-vegdist(JUICE.sstable,method='euclidean')	
   druha<-cutree(hclust(sih,method='ward'),no.clusters)
#  ..........................................................

#  ..........................................................
#  ---             Flexible beta clustering               ---

#   sih<-agnes(vegdist(JUICE.sstable,method='bray'), method='flexible', par.meth=c(alpha,alpha,1-2*alpha,0))
#   druha<-cutree(as.hclust(sih),no.clusters)
#  ..........................................................

#  ..........................................................
#  ---                        Isopam                     ---

#   druha<-isopam(JUICE.sstable,c.fix=no.clusters)[3]
#  ..........................................................


   		dru<-as.matrix(data.frame(druha))

#   ---------------------------------------------------------
#   ---         Crosstabulation                           ---
#   ---------------------------------------------------------

   		crosstab<-array(0,dim=c(no.clusters,no.clusters))
   		crossran<-array(0,dim=c(no.clusters,no.clusters))
   		prv.temp <- prv[rownames (prv) %in% rownames (dru),] #tohle vybere z prv jenom ty radky se snimky obsazenymi take v dru (na to slouzi funkce A %in% B, coz znamena ze A bude TRUE pokud se vyskytuje i v B, jinak bude FALSE; hranaty zavorky nakonec z prv vyseknou jenom ty radky, ktere jsou TRUE)
   		crosstab <- table (prv.temp, dru) # vytvori kontingencni tabulku


#   ---------------------------------------------------------
#   ---         Raw lambda calculation                        ---
#   ---------------------------------------------------------

   		crossR<-apply(crosstab,1,function(x) max(x))
   		crossC<-apply(crosstab,2,function(x) max(x))
   		F1<- sum(crossR)
   		F2<- sum(crossC)
   		F5<- 2 * sum(crosstab)
   		F3<- max(rowSums(crosstab))
   		F4<- max(colSums(crosstab))
   		lb[cycle]=(F1+F2-F3-F4)/(F5-F3-F4)

#   ---------------------------------------------------------
#   ---         Random lambda calculation (10 times more) ---
#   ---------------------------------------------------------

		for (duna in 1:10)
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

	sort (lb)
   	lambda=sum(lb)
   	lambda=lambda/no.cycles
   	rnd_lambda=rnd_lambda/(no.cycles*10)
   	mod_lambda=(lambda-rnd_lambda)/(1-rnd_lambda)
   	mod_lambda

#   ---------------------------------------------------------

