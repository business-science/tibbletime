# ------------------------------------------------------------------------------
# Registration function
# Copied from googledrive r package, dplyr-compat.R

## function is called in .onLoad()

# nocov start

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg))
  envir <- asNamespace(pkg)

  stopifnot(is.character(generic))
  stopifnot(is.character(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}

tidyr_at_least_1.0.0 <- NULL

.onLoad <- function(libname, pkgname) {

  # If tidyr is available, library() it and register these methods implemented
  # in tibbletime.
  # This is done because tidyr is not imported because it is not used
  # anywhere else in the package.
  if (requireNamespace("tidyr", quietly = TRUE)) {
    register_s3_method("tidyr", "gather", "tbl_time")
    register_s3_method("tidyr", "spread", "tbl_time")
    register_s3_method("tidyr", "nest",   "tbl_time")
    register_s3_method("tidyr", "unnest", "tbl_time")
    register_s3_method("tidyr", "unnest", "tbl_df")
    tidyr_at_least_1.0.0 <<- utils::packageVersion("tidyr") >= "1.0.0"
  }

  invisible()
}

# nocov end
