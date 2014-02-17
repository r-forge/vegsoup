require(vegsoup)

Rprof(tmp <- tempfile())
dta <- coenoflex(100, 200)
Rprof()
summaryRprof(tmp)
unlink(tmp)