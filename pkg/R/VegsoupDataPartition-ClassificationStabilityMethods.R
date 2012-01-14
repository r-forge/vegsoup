#	

   no.cycles<-200
   no.clusters<-8

   pb <- txtProgressBar (min = 1, max = no.cycles, char = '.',
        width = 45, style = 3, title = 'Calculation progress')

   setTxtProgressBar (pb, 1)