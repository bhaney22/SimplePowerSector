#
# This file contains helper functions for the BuildScenarios script.
# 

#' Create a price matrix
#'
#' @param P1 The price for product 1
#' @param P2 The price for product 2
#' @param F1 The price of commodity purchased by F1
#' @param F2 The price of commodity purchased by F2
#'
#' @return a matrix of prices, with zeroes in all the right places
#' @export
#'
create_price_matrix <- function(P1, P2, F1, F2){
  lenP1 <- length(P1)
  if (lenP1 > 1 & length(P2) == lenP1 & length(F1) == lenP1 & length(F2) == lenP1){
    return(Map(create_price_matrix, P1, P2, F1, F2))
  }
  sum_byname(
    # Sub-matrix of prices for P1 and P2
    matrix(rep(c(P1, P2), Ind.n),
           nrow = 2, ncol = Ind.n) %>%
      setrownames_byname(paste0("P", 1:2)) %>% setcolnames_byname(industry.names),
    # Sub-matrix of prices for F1 and F2
    matrix(rep(c(F1, F2), Fin.n),
           byrow = TRUE, nrow = Prod.n - Fin.n, ncol = Fin.n) %>%
      setrownames_byname(paste0("P", (Fin.n+1):Prod.n)) %>%
      setcolnames_byname(c("F1", "F2"))
  ) %>%
    sort_rows_cols(margin = 2, colorder = c(industry.names, fin.names))
}
