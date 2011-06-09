#	init vegbase
source("/Users/roli/Documents/Rpackages/vegbase/debug/init.R")

#	load data	
setwd("~/Documents/Rpackages/vegbase/debug")

load("species.Rdata")
load("taxonomy.Rdata")
load("sites.Rdata")

#	sites[sites$substrate == "FG",]$substrate <- "Fg"
#	sites[sites$relief == "O",]$relief <- "B"
#	save(sites, file = "sites.Rdata")


query <- vegbase.query(species, sites, taxonomy)
species <- vegbase.species(query, scale = "frequency")
sites <- vegbase.sites(query)

#	remove substrate type 'mö', various anthropogenic substrates
drop <- sites@raw$substrate != "mö"

species <- species[drop,]
species <- species[apply(species@pa, 1, sum) > 0,]
sites <- sites[drop,]

#	subsets

#	rocks, ground, trees and twigs 
sts.tree.twig <- unique(sites@raw$tree.twig)
ground <- match(sites@raw$tree.twig, sts.tree.twig) == 1
tree <- match(sites@raw$tree.twig, sts.tree.twig) == 2
twig <- match(sites@raw$tree.twig, sts.tree.twig) == 3

#	calcareous substrate (rocks)
cal <- sites@raw$substrate == "cal"