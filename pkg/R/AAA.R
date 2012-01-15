.onLoad <- function(lib, pkg) {
	require(methods)
}


.onUnload <- function(libpath)
    library.dynam.unload("Vegsoup", libpath)