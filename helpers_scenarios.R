#
# This file contains helper functions for the BuildScenarios script.
# 


#' Create an F.split matrix
#' 
#' Also works when f1 is a list of f1 values.
#'
#' @param f1 the fraction for Industry 1
#'
#' @return a 1x2 matrix consisting of f1 and 1-f1
#' @export
#' 
#' @examples 
#' create_F.split_matrix(0.2)
create_F.split_matrix <- function(f1){
  if (length(f1) > 1) {
    return(lapply(f1, create_F.split_matrix))
  }
  matrix(c(f1, 1 - f1),
         nrow = 1, ncol = 2, byrow = TRUE) %>%
    setrownames_byname("Product") %>% setcolnames_byname(c("F1", "F2")) %>%
    setrowtype("Products") %>% setcoltype("Industries") %>% 
    make_list(n = 1, lenx = 1)
}


#' Create an F.product.coeffs matrix
#' 
#' Also works with lists for \code{fpc31} ... \code{pfc52}.
#'
#' fpcxy --> x is output good (fin.1 or fin.2), y is industry (ind.3, ind.4, ind.5, ind.6)
#'
#' @param fpc31 the entry in the 3rd row and 1st column of the F.produce.coeffs matrix
#' @param fpc32 the entry in the 3rd row and 2nd column of the F.produce.coeffs matrix
#' @param fpc41 the entry in the 4th row and 1st column of the F.produce.coeffs matrix
#' @param fpc42 the entry in the 4th row and 2nd column of the F.produce.coeffs matrix
#' @param fpc51 the entry in the 5th row and 1st column of the F.produce.coeffs matrix
#' @param fpc52 the entry in the 5th row and 2nd column of the F.produce.coeffs matrix
#'
#' @return an F.product.coeffs matrix
#' @export
#' 
#' @examples 
#' create_F.product.coeffs_matrix(fpc13 = 0.2, fpc23 = 0.3, 
#'                                fpc14 = 0.1, fpc24 = 0.25,
#'                                fpc15 = 0.2, fpc25 = 0.1, 
#'                                fpc16 = 0.5, fpc26 = 0.35)
create_F.product.coeffs_matrix <- function(fpc13, fpc23,
                                           fpc14, fpc24,
                                           fpc15, fpc25, 
                                           fpc16, fpc26){
  # Ensure that we have fpcs that sum to 1.
  # fpc1x is output goods
  # fpc2x is industries
  stopifnot(fpc13 + fpc14 + fpc15 + fpc16 == 1)
  stopifnot(fpc23 + fpc24 + fpc25 + fpc26 == 1)
  
  # Test to ensure lengths are all same.
  lenfpc13 <- length(fpc13)
  if (lenfpc13 > 1 & length(fpc23) == lenfpc13 & 
      length(fpc14) == lenfpc13 & length(fpc24) == lenfpc13 & 
      length(fpc15) == lenfpc13 & length(fpc25) == lenfpc13 & 
      length(fpc16) == lenfpc13 & length(fpc26) == lenfpc13) {
    return(Map(create_F.product.coeffs_matrix, fpc13, fpc23, fpc14, fpc24, fpc15, fpc25, fpc16, fpc26))
  }
  # Add additional items to the matrix
  # fpc11, fpc21, fpc12, and fpc22 are 0, because industries 1 and 2 are the primary extraction industries.
  # No primary extraction resource goes straight to a final output good.
  fpc11 <- 0
  fpc21 <- 0
  fpc12 <- 0
  fpc22 <- 0
  matrix(c(fpc11, fpc21,
           fpc12, fpc22, 
           fpc13, fpc23,
           fpc14, fpc24,
           fpc15, fpc25,
           fpc16, fpc26),
         nrow = 6, ncol = 2, byrow = TRUE) %>% 
    setrownames_byname(c("P1", "P2", "P3", "P4", "P5", "P6")) %>% 
    setcolnames_byname(c("F1", "F2")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
}

#' Create a matrix of manufacturing efficiencies (etas)
#' 
#' Also works when \code{I1}, \code{I2}, \code{I3}, \code{I4}, \code{I5}, and \code{I6}
#' are lists.
#'
#' @param I1 the efficiency for the 1st industry
#' @param I2 the efficiency for the 2nd industry
#' @param I3 the efficiency for the 3rd industry
#' @param I4 the efficiency for the 4th industry
#' @param I5 the efficiency for the 5th industry
#' @param I6 the efficiency for the 6th industry
#'
#' @return a matrix of manufacturing efficiencies
#' @export
#'
#' @examples
#' create_mfg.etas_matrix(I1 = 0.2, I2 = 0.3, I3 = 0.25, I4 = 0.5, I5 = 0.15, I6 = 0.75)
create_mfg.etas_matrix <- function(I1, I2, I3, I4, I5, I6){
  lenI1 <- length(I1)
  if (lenI1 > 1 & length(I2) == lenI1 &
      length(I3) == lenI1 & length(I4) == lenI1 &
      length(I5) == lenI1 & length(I6) == lenI1){
    return(Map(create_mfg.etas_matrix, I1, I2, I3, I4, I5, I6))
  }
  r <- c(I1, I2, I3, I4, I5, I6)
  matrix(rep.int(r, 6), 
         nrow = 6, ncol = 6, byrow = TRUE) %>% 
    setrownames_byname(paste0("P", 1:6)) %>% setcolnames_byname(paste0("I", 1:6)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
}

#' Create a price matrix
#' 
#' Also works when \code{P1}, \code{P2}, \code{F1}, and \code{F2} are lists of prices.
#'
#' @param P1 the price for product 1
#' @param P2 the price for product 2
#' @param F1 the price of commodity purchased by F1
#' @param F2 the price of commodity purchased by F2
#'
#' @return a matrix of prices, with zeroes in all the right places
#' @export
#'
#' @examples
#' create_price_matrix(P1 = 0.05595097, P2 = 0.08967107, F1 = 0.876, F2 = 1.314)
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
