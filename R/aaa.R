.onLoad <- function(libname, pkgname) {

  # If tidyr is available, library() it and register these methods implemented
  # in tibbletime.
  # This is done because tidyr is not imported because it is not used
  # anywhere else in the package.
  if (requireNamespace("tidyr", quietly = TRUE)) {
    register_s3_method("tidyr", "nest",   "tbl_time")
    register_s3_method("tidyr", "unnest", "tbl_time")
    register_s3_method("tidyr", "unnest", "tbl_df")
  }

  invisible()
}
