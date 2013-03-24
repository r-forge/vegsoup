#	private class not exposed to the user
#	used to allow slots 'codes' and 'lims' to be NULL 
setClassUnion("codes", c("character", "NULL"))
setClassUnion("lims", c("numeric", "NULL"))

#	!any(is.na(factor(x$cov, levels = scale$codes, labels = scale$lims)))
setClass("Coverscale",
	representation(
	name = "character",
	codes = "codes",
	lims = "lims")
	#,
#	validity = function (object) {
#		if (length(codes) != length(lims)) {
#			cat("length of codes and lims must")
#			FALSE
#		} else {
#			TRUE
#		}
#	}
)

#	cover scale definitions
#	validity for neagtive entries in class Vegsoup*!
#	continous scales 
.percentage <- .counts <- .frequency <- list(
	name = "percentage", 
	codes = NULL,
	lims = NULL)
.counts$name <- "counts"
.frequency$name <- "frequency"

#	ordinal scales	
#	Braun-Blanquet new
.braun.blanquet <- list(
	name = "Braun-Blanquet", 
	codes = c("r", "+", "1", "2m", "2a", "2b", "3", "4", "5"),
	lims = c(  0.1, 1,   3,   4,    8,   18,   38,  68,  88))

#	Braun-Blanquet old
.braun.blanquet2 <- list(
	name = "Braun-Blanquet 2", 
	codes = c("r", "+", "1", "2", "3", "4", "5"),
	lims = c(  0.1, 1,   3,  13,  38,  68,  88))

#	Hult, Sernander, Du Rietz
#	reference
.hult <- list(
	name = "Hult, Sernander, Du Rietz",
	codes = c("1", "2", "3", "4", "5"),
	lims =  c( 3,   9,  19,  37.5,75)) 

#	ordinal
.ordinal <- list(
	name = "ordinal",
	codes = c("1", "2", "3", "4", "5", "6",  "7", "8", "9"),
	lims =  c( 1,   2,   3,   4,   8,  18,   38,  68,   88)) 

#	Domin (sensu curral 1987)
#	Currall, J. (1987). A transformation of the Domin scale. Vegetatio, 72(2):81–87.
.domin <- list(
	name = "Domin",
	codes = c("+", "1", "2", "3", "4", "5", "6", "7", "8", "9", "X"),
	lims =  c( 0.1, 0.25,0.5, 2.5, 7,  17,  28.5,41,  62,  84.5,97.5)) 

#	North-Carolina
.carolina <- list(
	name = "Carolina",
	codes = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "X"),
	lims =  c( 0.1, 0.5, 1.5, 3.5, 7.5,17.5,37.5,62.5,85,  97.5)) 

#	Doing
.doing <- list(
	name = "Doing",
	codes = c("r", "p", "a", "m", "01", "02",  "03", "04", "05", "06", "07", "08", "09", "10"),
	lims =  c( 1,   1,   2,   4,   10,   20,    30,   40,   50,   60,   70,   80,   90,   97)) 

#	Barkman Doing & Segal
.doing2 <- list(
	name = "Barkman Doing et Segal",
	codes = c("r", "+r", "+p", "+a", "+b", "1p",  "1a", "1b", "2m", "2a", "2b", "3a", "3b", "4a", "4b", "5a", "5b"),
	lims =  c( 1,   1,    1,    1,    2,    1,     2,    3,    4,    8,   18,   31,   43,   56,   68,   81,   93)) 

#	Londo
.londo <- list(
	name = "Londo",
	codes = c(".1", "r1", "p1", "a1", "m1", "*2", "r2", "p2", "a2", "m2", "*4", "r4", "p4", "a4", "m4", "1-", "1", "1+", "2-", "2", "2+", "3-", "3", "3+", "4-", "4", "4+", "5-", "5", "5+", "6-", "6", "6+", "7-", "7", "7+", "8-", "8", "8+", "9-", "9", "9+", "10"),
	lims =  c( 0.5,  1,    1,    1,    1,    2,    2,    2,    2,    2,    4,    4,    4,    4,    4,    7,   10,  12,   17,   20,  22,   27,   30,  32,   37,   40,  42,   47,   50,  52,   57,   60,  62,   67,   70,  72,   77,   80,   82,  87,   90,   92,   97)) 

#	Londo2 !tabs!
.londo2 <- list(
	name = "Londo 2",
	codes = c(".1", ".1r", ".1p", ".1a", ".1m", ".2", ".2r", ".2p", ".2a", ".2m", "2", ".4", ".4r", ".4p", ".4a", ".4m", "1", "2", "3",  "4", "5-", "5", "5+", "6", "7", "8", "9", "10"),
	lims =  c( 0.5,  0.5,   0.5,   0.5,   0.5,   2,    2,     2,     2,     2,     2,   4,    4,     4,     4,     4,    10,  20,  30,   40,  47.5, 50,  52.5, 60,  70,  80,  90,   97.5)) 

#	based on tvscale.dbf taken from package vegdata
#	Dengler 2003
#	reference
.dengler <- list(
	name = "Dengler",
	codes = c("r", "+", "1", "A", "B", "3", "4", "5"),
	lims =  c(0.5,  1.8, 3.8, 7.5,17.5,37.5,62.5,87.5))

#	Pfadenhauer et al 1986
#	Pfadenhauer, J., Poschlod, P., and Buchwald, R. (1986). Überlegungen zu einem Konzept geobotanischer Dauerbeobachtungsflächen für bayern, Teil I. Berichte der ANL, 10:41–60.
.pfadenhauer <- list(
	name = "Pfadenhauer",
	codes = c("+", "1a", "1b", "2a", "2b", "3", "4", "5"),
	lims =  c( 1,   2,    4,   10,   20,   38,  63,  88)) 

#	Ebert Klopfer et Pötsch
#ebert.klopfer.poetsch <- list(
#	name = "Ebert Klopfer et Pötsch",
#	codes = c("8", "7", "6", "2", "5", "3", "2", "1"),
#	lims =  c( 1,   2,   4,   6,   8,  38,  68,  88)) 
#with(ebert.klopfer.poetsch, length(codes) == length(lims))	
#	Mirschel
#mirschel <- list(name = "Mirschel",
#	codes = c("1", "2", "3", "4", "5", "6",  "7", "8", "9", "10"),
#	lims =  c( 0.1, 0.6, 1.5, 3.5, 7.5, 17.5,37.5,62,  85,   97.5)) 
#with(mirschel, length(codes) == length(lims))	
		
#	used in coverscale()
.COVERSCALES <- list(
	braun.blanquet = .braun.blanquet,
	braun.blanquet2 = .braun.blanquet2,
	hult = .hult,
	ordinal = .ordinal,
	domin = .domin,
	carolina = .carolina,
	doing = .doing,
	doing2 = .doing2,
	londo = .londo,
	londo2 = .londo2,
	dengler = .dengler,
	pfadenhauer = .pfadenhauer,
	percentage = .percentage,
	frequency = .frequency,
	counts = .counts)
	
#.builtin <- c(
#	"Braun-Blanquet", "Braun-Blanquet 2",
#	"Hult", "ordinal", "Domin", "North-Carolina",
#	"Doing", "Barkman Doing et Segal",
#	"Londo", "Londo 2",
#	"Dengler", "Pfadenhauer",
#	"braun.blanquet", "braun.blanquet2",
#	"hult", "ordinal", "domin", "north.carolina",
#	"doing", "doing2",
#	"londo", "londo2",
#	"dengler", "pfadenhauer"
#	)
