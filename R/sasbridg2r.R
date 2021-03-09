
#' @title "Not In"
#' @name %notin%
#' @description Negation of the \code{%in%} operator.
#' @param x Value(s) to be checked for in y.
#' @param y Value(s) to be checked against.
#' @examples
#' 1 %notin% c(1,2,3)
#' 1 %notin% c(2,3)
#' @export
`%notin%` <- function(x, y) {

  !`%in%`(x, y)

}

#' @name %notin%
#' @param x Value(s) to be checked for in y.
#' @param y Value(s) to be checked against.
#' @examples
#' 1 %!in% c(1,2,3)
#' 1 %!in% c(2,3)
#' @export
`%!in%` <- function(x, y) {

  !`%in%`(x, y)

}


#' @title "Not Equal To"
#' @name %_not=_%
#' @description Special NA != Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 1 %_!=_% 2
#' 1 %_!=_% NA
#' NA %_!=_% NA
#' @export
`%_!=_%` <- function(lhs, rhs) {

  (lhs != rhs | (is.na(lhs) & !is.na(rhs)) | (is.na(rhs) & !is.na(lhs))) & !(is.na(lhs) & is.na(rhs))

}


#' @title "Equal To"
#' @name %_==_%
#' @description Special NA == Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 1 %_==_% 2
#' 1 %_==_% NA
#' NA %_==_% NA
#' @export
`%_==_%` <- function(lhs, rhs) {

  !`%_!=_%`(lhs, rhs)

}


#' @title "Greater Than"
#' @name %_>_%
#' @description Special NA > Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 2 %_>_% 1
#' 1 %_>_% NA
#' NA %_>_% NA
#' @export
`%_>_%` <- function(lhs, rhs) {

  (lhs > rhs | (is.na(rhs) & !is.na(lhs))) & !((is.na(lhs) & !is.na(rhs)) | (is.na(lhs) & is.na(rhs)))

}


#' @title "Less Than"
#' @name %_<_%
#' @description Special NA < Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 1 %_<_% 2
#' NA %_<_% 1
#' NA %_<_% NA
#' @export
`%_<_%` <- function(lhs, rhs) {

  (lhs < rhs | (is.na(lhs) & !is.na(rhs))) & !((is.na(rhs) & !is.na(lhs)) | (is.na(lhs) & is.na(rhs)))

}


#' @title "Greater Than Or Equal To"
#' @name %_>=_%
#' @description Special NA >= Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 2 %_>=_% 1
#' 1 %_>=_% NA
#' NA %_>=_% NA
#' @export
`%_>=_%` <- function(lhs, rhs) {

  !`%_<_%`(lhs, rhs)

}

#' @title "Less Than Or Equal To"
#' @name %_<=_%
#' @description Special NA <= Operator.
#' @param lhs Left-hand side of operator.
#' @param rhs Right-hand side of operator.
#' @examples
#' 1 %_<=_% 2
#' NA %_<=_% 1
#' NA %_<=_% NA
#' @export
`%_<=_%` <- function(lhs, rhs) {

  !`%_>_%`(lhs, rhs)

}


#' @title "SAS Rounding in R"
#' @name round_sas
#' @description Round in R Like SAS: "Banker's Rounding"
#' @param x Numeric variable / value to round.
#' @param digits Number of digits to display to the right of the decimal place.
#' @examples
#' round_sas(2.5)
#' round_sas(2.55, digits = 1)
#' @export
round_sas <- function(x, digits = 0) {

  posneg <- sign(x)

  z <- abs(x) * 10^digits

  z <- z + 0.5 + sqrt(.Machine$double.eps)

  z <- trunc(z)

  z <- z/10^digits

  z * posneg

}


#' @title "Display Rounded Percentages Like SAS"
#' @name round_sas_pct
#' @description Round percentages like SAS and display the % sign.
#' @param x Numeric variable / value to round.
#' @param digits Number of digits to display to the right of the decimal place.
#' @param by100 Multiply the numeric variable / value by 100.
#' @examples
#' round_sas_pct(0.55)
#' round_sas_pct(0.555, digits = 1)
#' round_sas_pct(55.5, by100 = FALSE)
#' @export
round_sas_pct <- function(x, digits = 0, by100 = TRUE) {

  if (by100 == TRUE) {

    y <- x * 100

  } else {

    y <- x

  }

  paste0(round_sas(x = y, digits = digits), "%")

}


#' @title "Derive Quantiles Like SAS"
#' @name quantile_sas
#' @descripion Mirror the quantile algorithm employed by SAS.
#' @param x Same as x parameter for stats::quantile.
#' @param probs Same as probs parameter for stats::quantile.
#' @param na.rm Same as na.rm parameter for stats::quantile.
#' @examples
#' quantile_sas(stats::runif(10), probs = 0.5)
#' @export
quantile_sas <- function(x, probs, na.rm = TRUE) {

  stats::quantile(x = x, probs = probs, na.rm = na.rm, type = 2)

}
