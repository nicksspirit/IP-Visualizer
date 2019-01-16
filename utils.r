library("magrittr")
library("docstring")

is_int <- function(n) {
#' Checks if n is an integer
#' @param n should be a numeric object

  return(as.integer(n) == n)
}

get_dvars <- function(bounded_sol) {
#' @title Get descision variables
#' @description Get the descision variables from the
#' solution to a linear relaxed problem

  num_dvars <- length(bounded_sol) - 1

  return(tail(bounded_sol, num_dvars))
}

branch_on <- function(dvars) {
#' Finds the integer solution value that has the greatest fractional part
#' @param dvars vector of numeric objects
#' @returns returns the max integer with a fractional part

  non_ints <- dvars %>% subset(!is_int(dvars))

  if (length(non_ints) == 0) {
      return(0)
  }

  return(max(non_ints))
}

get_varname <- function(dvars, branch_val) {
#' Get the name of the optimal integer solution value
#' @param dvars vector of descision variables of the linear relaxed solution
#' @param branch_val optimal integer solution value

  dvar_index <- match(branch_val, dvars)

  return(
      dvars %>%
      names() %>%
      extract(dvar_index)
  )
}

constr_coeffs <- function(dvars, branch_val) {
#' @title Create numeric vector containing the constraint coefficients
#' @description The vector is the same length and the length of
#' the descision varialbles dvars.
#' The vector mostly consists of zeros expect for the index where branch_val
#' exists and the value there is one
#' @param dvars vector of descision variables of the linear relaxed solution
#' @param branch_val optimal integer solution value

  coeff_vec <- rep(0, times = length(dvars))
  dvar_index <- match(branch_val, dvars)
  coeff_vec[dvar_index] <- 1

  return(coeff_vec)
}