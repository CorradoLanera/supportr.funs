.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to proxynator."
  )
}


.onLoad <- function(libname, pkgname) {

  if (Sys.getenv("DBPROXY_SNP_EXPOSURE_varname") == "") {
    message(glue::glue(
      "Environmental variable 'DBPROXY_SNP_EXPOSURE_varname' is not set.
      Default to 'snp_exposure'"
    ))
    Sys.setenv(
      DBPROXY_SNP_EXPOSURE_varname = "snp_exposure"
    )
  }

  # op <- options()
  #
  # op.depigner <- list(
  #   depigner.dev.test_telegram_bot = FALSE,
  #
  #   depigner.bot_name = NULL,
  #   depigner.chat_id = NULL
  # )
  #
  # toset <- !(names(op.depigner) %in% names(op))
  #
  # if (any(toset)) options(op.depigner[toset])
  #
  invisible(TRUE)
}
