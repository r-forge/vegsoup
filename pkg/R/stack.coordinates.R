#	read OGR data source
stackCoordinates <- function (dsn, layer, schema, round = TRUE, verbose = TRUE, ...) {

    # read data source via terra
    pt <- terra::vect(dsn, layer = layer, ...)

    # check for Z dimension
	g <- terra::geom(pt, geom = TRUE)
	withz <- "z" %in% colnames(g)

    if (missing(schema)) {
        if (verbose) {
            print(pt)
            cat("\nAvailable attribute columns:\n")
            print(names(pt))
        }
        stop("please supply a column name in data source indicating plot ids")
    }

    # check column names
    pt.names <- names(pt)
    test <- match(schema, pt.names)

    if (any(is.na(test))) {
        cat("\nterra::vect returns these fields:\n")
        print(pt.names)
        cat("you supplied schema: ", paste(schema, collapse = " "), "\n")
        cat("found: ",
            ifelse(length(pt.names[test[!is.na(test)]]) == 0,
                   "... nothing?",
                   paste0(pt.names[test[!is.na(test)]], collapse = " ")))
        cat("\n")
        stop("\nif specified both elements have to match")
    }

    # reproject to WGS84
    pt <- terra::project(pt, "EPSG:4326")

    # extract coordinates (XY or XYZ)
    coords <- terra::crds(pt, df = FALSE)
    coords <- as.data.frame(coords)

    # build df depending on presence of Z and schema length
    if (!withz && length(schema) == 1) {
        df <- data.frame(
            coords,
            plot = as.character(pt[[schema[1]]]),
            stringsAsFactors = FALSE
        )
    } else if (!withz && length(schema) == 2) {
        df <- data.frame(
            coords,
            elevation = as.numeric(as.character(pt[[schema[2]]])),
            plot      = as.character(pt[[schema[1]]]),
            stringsAsFactors = FALSE
        )
    } else if (withz && length(schema) == 1) {
        df <- data.frame(
            coords[, 1:2, drop = FALSE],
            elevation = coords[, 3],
            plot      = as.character(pt[[schema[1]]]),
            stringsAsFactors = FALSE
        )
    } else { # withz & length(schema) == 2
        warning("Data source supports 3D",
                " but use attribute \"", schema[2], "\" to obtain heights from attributes")
        df <- data.frame(
            coords,
            elevation = as.numeric(as.character(pt[[schema[2]]])),
            plot      = as.character(pt[[schema[1]]]),
            stringsAsFactors = FALSE
        )
    }

    names(df)[1:2] <- c("longitude", "latitude")

    if (!withz) {
        res <- data.frame(
            as.character(df$plot),
            stack(df, select = 1:2),
            stringsAsFactors = FALSE
        )
        res <- res[, c(1, 3, 2)]
    }

    if ((!withz && length(schema) == 2) || withz) {
        res <- data.frame(
            as.character(df$plot),
            stack(df, select = 1:3),
            stringsAsFactors = FALSE
        )
        res <- res[, c(1, 3, 2)]
        res[, 3] <- as.numeric(as.character(res[, 3])) # for safety
    }

    names(res) <- c("plot", "variable", "value")

    if (round) {
        res$value[res$variable == "longitude"] <-
            round(res$value[res$variable == "longitude"], 6)
        res$value[res$variable == "latitude"]  <-
            round(res$value[res$variable == "latitude"], 6)
        res$value[res$variable == "altitude"]  <-
            round(res$value[res$variable == "altitude"], 0)
    }

    res <- new("Sites", data = res)
    return(invisible(res))
}
