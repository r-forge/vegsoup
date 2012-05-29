.onLoad <- function(lib, pkg) {

}


.onUnload <- function(libpath) {
    library.dynam.unload("Vegsoup", libpath)
}