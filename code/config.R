if (!exists('req_env')) req_env <- new.env(parent = emptyenv())

#' Retrieve an item of site-specific configuration
#'
#' @param label The name of the configuration item
#' @param value Optionally, a value to which to set the configuration
#'     item.
#'
#' @return The (possibly new) configuration value, or `NULL` if it
#'     does not exist.
#' @md
config <- function(label, value) {
  if (! missing(value)) assign(label, value, pos = req_env)
  invisible(get0(label, envir = req_env))
}

#' Determine whether a configuration item is set
#'
#' @param label The name of the configuration item
#'
#' @return TRUE if the item has been set, and FALSE if not
#' @md
config_exists <- function(label) {
  exists(label, where = req_env)
}

#' Remove a configuration item
#'
#' If the configuration item exists, deletes it.  If the item did
#' not exist, no action is taken.
#'
#' @param label The name of the configuration item
#'
#' @return NULL
#' @md
config_rm <- function(label) {
  if (config_exists(label)) rm(list = label, pos = req_env)
}
#' Append to a configuration item
#'
#' Append new value(s) to a configuration item if the item already exists.  The
#'   form of the result depends on the existing value of the item, following the
#'   rules for the core [c()] function.
#'
#' If the configuration item does not exist, then it is created with the given
#'   value as its contents.
#'
#' @param label The name of the configuration item
#' @param value The value to be appended
#'
#' @return The revised value of the configuration item
#' @md
config_append <- function(label, value) {
  if (config_exists(label)) {
    config(label, c(config(label), value))
  } else {
    config(label, value)
  }
}

#' Retrieve a handle for accessing configuration items
#'
#' If you prefer to interact with the configuration system as an
#' R object rather than via function calls, you can retrieve a value
#' via this function that behaves in a manner similar to a list:
#' configuration values can be retrieved or set using the `$` or `[[`
#' indexing operators.
#'
#' Note that the handle should be treated as an opaque object, and no functions
#' than the indexing operators have defined behavior.
#'
#' @return The configuration handle
#' @md
config_handle <- function() { x <- req_env; class(x) <- '_co_req'; x }

`$<-._co_req` <- function(x, elt, value) {
  config({{ elt }}, value)
  req_env
}
