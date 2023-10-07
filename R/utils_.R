#' @title Not empty
#' 
#' @describeIn utils_ 
#' Check if a field is not empty.
#' @param rc Row data.
#' @keywords internal
#' @returns boolean
not_empty <- function(rc){
  !(is.na(rc) || rc=="")
}


#' @title Print messages 
#' 
#' @describeIn utils_ 
#' Conditionally print messages.
#'  Allows developers to easily control verbosity of functions, 
#'  and meet Bioconductor requirements that dictate the message 
#'  must first be stored to a variable before passing to \link[base]{message}. 
#'  
#' 
#' @param v Whether to print messages or not.
#' @param parallel Whether to enable message print when wrapped 
#' in parallelised functions.
#' 
#' @return Null 
#' @keywords internal 
messager <- function(..., v = TRUE, parallel = FALSE) {
  
  message_parallel <- function(...) {
    system(sprintf('echo "%s"', paste0(..., collapse = "")))
  }
  if(isTRUE(parallel)){
    if(v) try({message_parallel(...)})
  } else {
    msg <- paste(...)
    if (v) try({message(msg)})
  }
}


#' @title Stop messages
#' 
#' @describeIn utils_ 
#'  Conditionally print stop messages.
#' Allows developers to easily control verbosity of functions, 
#'  and meet Bioconductor requirements that dictate the stop message 
#'  must first be stored to a variable before passing to \link[base]{stop}. 
#' @param v Whether to print messages or not.
#' 
#' @return Null 
#' @keywords internal 
stopper <- function(..., v = TRUE) {
  msg <- paste(...)
  if (v) {
    stop(msg)
  } else {
    stop()
  }
}
