#' Configuration
#'
#' Various functions for loading, caching and performing configured behaviors
#' using a user-supplied configuration file.
#'
#' @param path A file path to use when searching for a config file. Either the
#'   file path to a `DESCRIPTION` or the root path of a package, depending on
#'   the context of the function.
#'
#' @name config
.registered <- new.env(parent = baseenv())



#' @describeIn config
#' Load the contents of a config into an environment
#'
#' @keywords internal
config_load <- function(path = getwd(), cache = TRUE) {
  if (!is.null(roxylint_config <- roxygen2::roxy_meta_get("roxylint")))
    return(roxylint_config)

  roxylint <- new.env(parent = baseenv())
  local_config <- config_find_from(path)

  # config linters
  for (tag in names(local_config$linters)) {
    add_linters(
      roxylint,
      tag,
      local_config$linters[[tag]],
      overwrite = TRUE
    )
  }

  # add non-linter config
  local_config$linters <- NULL
  for (n in names(local_config)) {
    roxylint[[n]] <- local_config[[n]]
  }

  # add any registered linters
  for (n in names(.registered)) {
    regconfig <- .registered[[n]]
    for (tag in names(regconfig$linters)) {
      overwrite <- isTRUE(regconfig$overwrite)
      new_linters <- regconfig$linters[[tag]]
      add_linters(roxylint, tag, new_linters, overwrite = overwrite)
    }
  }

  # store roxylint in roxygen2 environment
  roxy_meta_set <- getNamespace("roxygen2")[["roxy_meta_set"]]
  if (cache) roxy_meta_set("roxylint", roxylint)

  roxylint
}


#' @describeIn config
#' Load a configuration from a path
#'
#' @keywords internal
config_find_from <- function(path) {
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) break
    if (dirname(path) == path) return(list())
    path <- dirname(path)
  }

  config_desc <- config_from_desc(path)
  config_file <- config_from_file(path)

  if (!is.null(config_desc) && !is.null(config_file))
    stop(errors$redundant_config)

  config_desc %||% config_file
}


#' @describeIn config
#' Load a configuration from a DESCRIPTION file
#'
#' @importFrom utils packageName
#' @keywords internal
config_from_desc <- function(path) {
  path <- file.path(path, "DESCRIPTION")

  field <- paste0("Config/", utils::packageName())
  config_desc <- read.dcf(path, fields = field)[1, field]

  result <- tryCatch(
    eval(parse(text = config_desc)),
    error = function(e) stop(errors$description_parse_failure(e$message))
  )

  if (length(result) == 0 || is.na(result)) return(NULL)
  result
}


#' @describeIn config
#' Load a configuration from a dotfile
#'
#' @importFrom utils packageName
#' @keywords internal
config_from_file <- function(path) {
  pattern <- "^meta\\.[rR]"

  path <- file.path(path, "man", utils::packageName())
  config_files <- list.files(
    path,
    pattern = pattern,
    all.files = TRUE,
    full.names = TRUE
  )

  if (length(config_files) == 0)
    return(NULL)

  res <- new.env()
  source(config_files[[1]], local = res)$value
}
