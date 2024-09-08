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

  # discover citations
  if (length(list.files(path, pattern = "^CITATION")) > 0) {
    roxylint$citations <- config_citations(path)
  }

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

config_citations <- function(path) {
  UseMethod("config_citations")
}

#' @export
config_citations.default <- function(path) {
  if (file.info(path)$isdir) {
    config_files <- list.files(
      path,
      pattern = "^CITATION",
      full.names = TRUE,
      recursive = TRUE
    )

    return(unlist(recursive = FALSE, lapply(config_files, function(f) {
      config_citations(f)
    })))
  }

  if (nchar(ext <- tools::file_ext(path)) > 0) {
    class(path) <- c(tolower(ext), class(path))
    config_citations(path)
  }
}

#' @export
config_citations.cff <- function(path) {
  if (!requireNamespace("cffr", quietly = TRUE)) {
    cli::cli_alert_info(
      "Package {.pkg cffr} is required to use citations to inform lints."
    )
    return()
  }

  derive_citation_data(cffr::cff_read(as.character(path)))
}

derive_citation_data <- function(x) {
  data <- list(cff = x)

  # derive proper nouns from authors lists
  ref_author_names <- function(ref) {
    lapply(ref$authors, function(author) {
      author[c("family-names", "given-names")]
    })
  }

  data$names <- unique(c(
    ref_author_names(x),
    unlist(recursive = FALSE, lapply(x$references, ref_author_names))
  ))

  data
}
