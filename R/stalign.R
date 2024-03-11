
# import library
.stalign <- function() {

  STA <- getOption("giotto.stalign_ptr", NULL)

  if (is.null(STA)) {
    package_check(
      "STalign",
      repository = "pip",
      custom_msg = sprintf(
        "%s\n\n %s\n%s\n%s",
        "package 'STalign' is not yet installed",
        "To install:",
        "# instructions for python pkg install to default Giotto miniconda environment",
        "reticulate::conda_install(envname = 'giotto_env',packages = 'git+https://github.com/JEFworks-Lab/STalign.git', pip = TRUE)"
      )
    )

    STA <- reticulate::import(
      "STalign.STalign",
      convert = TRUE,
      delay_load = TRUE
    )
    options(giotto.stalign_ptr = STA) # cache the ptr for future calls
  }

  return(STA)
}


setGeneric("doSTalign", function(x, y, ...) standardGeneric("doSTalign"))



#' @param dx,dy rasterization resolution for vector coords type x,y inputs.
#' default is 1/250 of the major axis distance of the image
setMethod(
  "doSTalign", signature(x = "spatLocsObj", y = "ANY"),
  function(
    x, y, dx = max(range(ext(x))) / 250, dy = max(range(ext(y))) / 250, ...
  ) {
    res_x <- .sta_prep_rasterize_spatlocs(x, dx = dx, ...)
    res_y <- .sta_prep_rasterize_spatlocs(y, dx = dy, ...)
    class(res_x) <- class(res_y) <- "stalign_rast"

    doSTalign(x = res_x, y = res_y, ...)
  }
)


setMethod(
  "doSTalign", signature(x = "spatLocsObj", y = "ANY"),
  function(
    x, y, dx = max(range(ext(x))) / 250, ...
  ) {
    res <- .sta_prep_rasterize_spatlocs(x, dx = dx, ...)
    class(res) <- "stalign_rast"

    doSTalign(x = res, y = y, ...)
  })

setMethod("doSTalign", signature(x = "ANY", y = "spatLocsObj"), function(x, y, ...) {
  doSTalign(x = y, y = x, ...)
})



# manual annotation steps ####

setMethod(
  "doSTalign", signature(x = "stalign_rast", y = "ANY"),
  function(x, y, ...) {
    # use terra to select points
    # TODO
  })





setMethod("doSTalign", signature("SpatRaster", "SpatRaster"), function(x, y, ...) {
  # TODO
  # core functionality
})



# internals ####

# downsamples to a resolution defined by dx.
# Not doing this through terra since this function also applies a gaussian blur
.sta_prep_rasterize_spatlocs <- function(
    x, dx = max(range(ext(x))) / 250, verbose = FALSE
) {
  stalign <- .stalign()

  xI <- reticulate::np_array(x$sdimx)
  yI <- reticulate::np_array(x$sdimy)

  out <- if (!verbose) {
    .quiet_py(stalign$rasterize(xI, yI, dx = dx))
  } else {
    stalign$rasterize(xI, yI, dx = dx)
  }

  names(out) <- c("XI", "YI", "I", "fig")
  out$I <- aperm(out$I, perm = c(2, 3, 1))

  return(out)
}


.quiet_py <- function(expr) {
  .out <- NULL
  reticulate::py_capture_output({.out <- eval(substitute(expr))})
  return(.out)
}


