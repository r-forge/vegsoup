#	not working!
setwd("/Users/roli//Dropbox/Rpackages/vegsoup")

require(methods)
require(vegan)
require(optpart)
require(Hmisc)
library(misc3d)
library(sp)
library(cluster)
library(labdsv)
library(optpart)
library(isopam)
library(indicspecies)

objs <- list.files(pattern = ".R")

source = FALSE

if (source) {	
	sapply(objs, source)
} else {
	package.skeleton("vegsoup", code_files = objs,
	path = "/Users/roli//Dropbox/Rpackages/vegsoup",
	namespace = TRUE, force = TRUE)
}

rm(objs, source)