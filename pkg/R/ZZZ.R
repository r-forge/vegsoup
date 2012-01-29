.onAttach <- function (lib, pkg) {
    packageStartupMessage("This is vegsoup ",
                          utils::packageDescription("vegsoup",
                                                    field = "Version"),
                          appendLF = TRUE)
}

