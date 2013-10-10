require(vegsoup)

Rprof(tmp <- tempfile())
dta <- coenoflex(2000, 1000)
Rprof()
summaryRprof(tmp)
unlink(tmp)