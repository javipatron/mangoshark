#' Calculate my snacks
#'
#' @param grapes mass of grapes (grams)
#' @param apples number in apples in the bucket (pieces)
#' @param burritos how many burritos could be produced (complete pieces)
#'
#' @return
#' @export
#'
#' @examples
#' snacks ( grapes =2.5, apples = 6, burritos = 1)
snacks <- function(grapes, apples, burritos) {
  (grapes + apples) / burritos
}


