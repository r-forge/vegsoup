#	init vegbase
source("/Users/roli/Documents/Rpackages/vegbase/debug/init.R")

#	load data	
setwd("~/Documents/Rpackages/vegbase/debug")

load("species.Rdata")
load("taxonomy.Rdata")
load("sites.Rdata")

query <- vegbase.query(species, sites, taxonomy)
spc <- vegbase.species(query, scale = "Braun.Blanquet")
sts <- vegbase.sites(query)

