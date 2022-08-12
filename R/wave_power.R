#' Wave Power calculation
#'
#' @param d water density (kg / m^3)
#' @param g acceleration due to gravity (m / s ^ 2)
#' @param pi pi number (3.1416)
#' @param h wave height (m)
#' @param t wave period
#' #'
#' @return
#' @export
#'
#' @examples
#'
wave_power <- function(d, g, pi, h, t) {
  p = ((d * g ^ 2 ) / (64 * pi )) * (h ^ 2) * (t)
  print(p)
}


