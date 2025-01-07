#' Compute spatial auto correlation using spdep
#'
#' @param gobject Input a Giotto object.
#' @param method Specify a method name to compute auto correlation.
#' Available methods include
#' \code{"geary.test", "lee.test", "lm.morantest","moran.test"}.
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use, default = normalized
#' @param spatial_network_to_use spatial network to use,
#' default = spatial_network
#' @param verbose be verbose
#' @param return_gobject if FALSE, results are returned as data.table.
#' If TRUE, values will be appended to feature metadata
#' @returns A data table with computed values for each feature.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' spdepAutoCorr(g)
#' @export
spdepAutoCorr <- function(
        gobject,
        method = c("geary.test", "lee.test", "lm.morantest", "moran.test"),
        feats,
        spat_unit = NULL,
        feat_type = NULL,
        expression_values = NULL,
        spatial_network_to_use = "spatial_network",
        wm_name = "spat_weights",
        method_params = list(),
        return_gobject = FALSE,
        verbose = FALSE,
        ...) {
    # Check and match the specified method argument
    method <- match.arg(method)

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    
    has_y <- is.list(feats)
    if (has_y) {
        x <- feats[[1]]
        y <- feats[[2]]
    } else {
        x <- feats
    }
    
    # get spatial weight matrix
    sn <- getSpatialNetwork(gobject,
        spat_unit = spat_unit[1L],
        name = spatial_network_to_use,
        verbose = verbose,
        output = "spatialNetworkObj"
    )
    wm <- slot(sn, "misc")$weight_matrix[[wm_name]]
    if (is.null(wm)) {
        sprintf(
            "weight matrix \"%s\" for spatial network \"%s\" does not exist.\n Create one first with `createSpatialWeightMatrix()`",
            wm_name, spatial_network_to_use
        ) |>
            stop(call. = FALSE)
    }
    
    # values should be ordered the same as the weight matrix
    id_order <- colnames(wm)
    
    # spatValues params
     p <- list(...)
    .get_sv_param <- function(i) {
        p$spat_unit <- spat_unit[1L] # must be same spat_unit
        p$feat_type <- feat_type[i]
        p$expression_values <- p$expression_values[i] %na% NULL
        p$spat_loc_name <- p$spat_loc_name[i] %na% NULL
        p$spat_enr_name <- p$spat_enr_name[i] %na% NULL
        # skip poly_info since "poly_ID" colname will throw error
        p$dim_reduction_to_use <- p$dim_reduction_to_use[i] %na% NULL
        p$dim_reduction_name <- p$dim_reduction_name[i] %na% NULL
        p$verbose <- verbose
    }
    svx_params <- .get_sv_param(1L)
    svx_params$feats <- x
    xvals <- do.call(spatValues, svx_params)
    xvals <- xvals[match(id_order), xvals$cell_ID, -"cell_ID"]
    if (has_y) {
        svy_params <- .get_sv_param(2L)
        svy_params$feats <- y
        yvals <- do.call(spatValues, svy_params)
        yvals <- yvals[match(id_order), xvals$cell_ID, -"cell_ID"]
    }
    
    a <- method_params
    a$method <- method
    a$wm <- spdep::mat2listw(wm, style = "W")
    
    if (!is.null(y)) {
        checkmate::assert_character(y)
        if (length(y) != length(x) &&
            length(y) != 1L) {
            stop("spdepAutoCorr: number of `y` features may only be:\n - 1 (paired against all `x` values)\n - same length as `x` (paired 1 to 1 with the `x` values)")
        }
    }

    # progressr
    nfeats <- ncol(xvals)
    if (nfeats > 20) {
        step_size <- ceiling(nfeats / 10L)
    } else {
        step_size <- 1
    }

    with_pbar({
        pb <- pbar(steps = nfeats / step_size)
        result_list <- lapply_flex(seq_len(nfeats),
            future.packages = c("data.table", "spdep"),
            function(feat_i) {
                a$x <- xvals[[feat_i]]
                if (has_y) {
                    if (ncol(yvals) > 1L) {
                        a$y <- yvals[[feat_i]]
                    } else {
                        a$y <- yvals[[1L]]
                    }
                }
                
                res <- do.call(callSpdep, a)
                
                if (inherits(res, "localG")) {
                    if (attr(res, "gstari")) gitype <- "Z(Gi*)"
                    else gitype <- "Z(Gi)"
                    res <- attr(res, "internals")
                    res <- res[, c("Z(Gi)", "Pr(z != E(Gi))")]
                    colnames(res) <- c(gitype, "p-value")
                    res <- apply(res, 2, function(x) {
                        data.table::as.data.table(t(x))
                    })
                } else if (inherits(res, "list")) {
                    res <- data.table::as.data.table(res)
                } else if (inherits(res, "boot")) {
                    res <- data.table::data.table(res$t0)
                    names(res) <- a$method
                } else if (inherits(res, "matrix")) {
                    res <- apply(res, 2, function(x) {
                        data.table::as.data.table(t(x))
                    })
                } else if (inherits(res, "data.frame") {
                    res <- lapply(res, t)
                })
                
                
                
                # Extract the estimated value from the result
                result_value <- res$estimate[1]
                temp_dt <- data.table(
                    feat_ID = feat[feat_i], value = result_value
                )
                # increment progress
                if (exists("pb")) if (feat_i %% step_size == 0) pb()
                return(temp_dt)
            }
        )
    })
    # combine results
    result_dt <- data.table::rbindlist(result_list)

    # Return the resulting datatable

    if (isTRUE(return_gobject)) {
        if (isTRUE(verbose)) {
            wrap_msg(
                "Appending", method,
                "results to feature metadata: fDataDT()"
            )
        }
        gobject <- addFeatMetadata(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            new_metadata = result_dt,
            by_column = TRUE,
            column_feat_ID = "feat_ID"
        )

        return(gobject)
    } else {
        return(result_dt)
    }
}






#' Call the spdep function with required parameters
#' @description
#' Call an spdep spatial statistic function. These functions typically require
#' a value `x` for providing numeric values to test (and a `y` for bivariate
#' cases) alongside an object that will provide spatial relationship information.
#' @param method Specify method name to call from spdep with its required
#' parameters.
#' @param x numeric vector. Values to test. Number of elements should be the
#' same as the number of cols/dims of the weight matrix
#' @param wm matrix. Weight matrix encoding spatial relationships to use.
#' Accepts a `matrix`, `Matrix`, or spdep `listw` object.
#' @param ... Additional parameters for the function. See spdep documentation
#' for relevant parameters.
#' @returns Computed statistics from the specified method.
#' @export
#' @seealso \url{https://cran.r-project.org/web/packages/spdep/index.html}
callSpdep <- function(method, x, wm = NULL, ...) {
    # Load the 'spdep' package if not already installed
    package_check(pkg_name = "spdep", repository = "CRAN", optional = FALSE)

    # Check if 'method' argument is NULL, if so, stop with an error
    if (is.null(method)) {
        stop("The 'method' argument has not been provided. Please specify a
            valid method.")
    }

    # Check if 'method' exists in the 'spdep' package, if not, stop with an
    # error
    fun <- try(eval(get(method, envir = loadNamespace("spdep"))),
        silent = TRUE
    )
    if (inherits(fun, "try-error")) {
        stop(paste(
            "Invalid method name. Method", method,
            "is not available in the spdep package."
        ))
    }

    # Fetch the arguments of the spdep function
    all_args <- args(fun) %>%
        as.list() %>%
        names()

    # Capture arguments provided by the user
    methodparam <- list(...)
    mp_names <- names(method_param)

    # Check if the user provided the listw argument
    listw_arg <- wm
    if ("listw" %in% names(methodparam)) {
        listw_arg <- listw_arg %null% methodparam$listw

        # Check if listw_arg is a matrix
        if (is.matrix(listw_arg)) {
            # Convert the matrix to a listw object
            listw_arg <- spdep::mat2listw(listw_arg, style = "W")
        } else if (!inherits(listw_arg, "listw")) {
            stop("`wm` must be either a `matrix` or a `listw` object.")
        }

        # Update the listw argument in methodparam
        methodparam$listw <- listw_arg
    }


    # Warn if not all user-provided arguments are used
    unused_args <- mp_names[!mp_names %in% all_args]
    if (length(unused_args) > 0L) {
        sprintf(
            "callSpdep: These args are not used with {spdep} `%s()`:\n %s",
            method, paste(unused_args, collapse = ", ")
        ) |>
            warning(call. = FALSE)
    }
    # A vector of commonly used constants in spdep
    spw_constants <- c("n", "n1", "n2", "n3", "nn", "S0", "S1", "S2")

    # Check if any of the constants are required by the spdep method
    if (any(spw_constants %in% all_args)) {
        # Obtain arguments from 'spweights.constants'
        spwc <- spdep::spweights.constants(listw = methodparam$listw)
        # Combine user-provided arguments and `spwc`, checking only against
        # 'feats' value
        combinedParams <- append(methodparam, spwc)
    } else {
        combinedParams <- methodparam
    }


    # Identify common parameters between user and 'spdep'
    commonParams <- intersect(names(combinedParams), all_args)

    # Create a named list of common parameters
    combinedParams <- combinedParams[commonParams]

    # Call the function with its parameters
    do.call(fun, combinedParams)
}
