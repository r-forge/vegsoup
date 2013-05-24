coenoflex <- function (numplt = 10, numspc = 10, ...) {	
	dta <- coenoflex::coenoflex(numgrd = 2, numplt = numplt, numspc = numspc,
	                 grdtyp = c("e","e"), grdlen = c(1,1), width = c(1/4,1/2),
	                 variab = c(2,1), grdprd = c(0,0), alphad = c(2,1.8),
	                 pdist = "r", sdist = "r", skew = 1.5, aacorr = 0.0,
	                 cmpasy = 0.5, cmpphy = 0, maxtot = 100,
	                 noise = 5, slack = 0.2, autlin = "irm(1,2)")
	
	as(dta, "Vegsoup")
}