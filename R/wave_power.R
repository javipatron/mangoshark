#' Wave Power calculation
#'
#' @param density water density (kg / m^3)
#' @param gravity acceleration due to gravity (m / s ^ 2)
#' @param pi pi number (3.1416)
#' @param height wave height (m)
#' @param time wave period
#' #'
#' @return
#' @export
#'
#' @examples
#'
wave_power <- function(density, gravity, pi, height, time) {
  p = ((density * gravity ^ 2 ) / (64 * pi )) * (height ^ 2) * (time())
  print(p)
}

