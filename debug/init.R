rm(list = ls())

setwd("~/dropbox/Rpackages/vegsoup")

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

#	import classes
setOldClass("duleg")
setOldClass("tabdev")
setOldClass("disdiam")
setOldClass("partana")
setOldClass("silhouette")

#	VegsoupDataPartition methods
setOldClass("agnes")
setOldClass("pam")
setOldClass("optpart")
setOldClass("isopam")
setOldClass("twins")
setOldClass("partition")

#	Collate
source("Class-Vegsoup.R")
source("Vegsoup-internal.r")
source("Vegsoup-Methods.R")
#showClass("Vegsoup")

source("Class-VegsoupData.R")
source("VegsoupData-Methods.R")
#showClass("VegsoupData")

source("Class-VegsoupDataPartition.R")
source("VegsoupDataPartition-Methods.R")
source("VegsoupDataPartition-MoreMethods.R")
source("VegsoupDataPartition-EvenMoreMethods.R")
#showClass("VegsoupDataPartition")

source("Class-VegsoupDataPartitionFidelity.R")
source("VegsoupDataPartitionFidelity-Methods.R")
#showClass("VegsoupDataPartitionFidelity")

#source("VegsoupPartitionSpreadHeatmap.R")

#source("Class-VegsoupDataLaTexSpecies.R")
#showClass("VegsoupDataLaTexSpecies")

#source("Class-VegsoupDataLaTexSites.R")
#showClass("VegsoupDataLaTexSites")

#source("Class-VegsoupDataLaTex.R")
#source("VegsoupDataLaTex-Methods.R")
#showClass("VegsoupDataLaTex")

#showMethods("VegsoupPartition")

