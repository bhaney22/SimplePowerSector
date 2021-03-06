#
# R script to calculate the IO Metrics 
#

##############################################################################################
# Function: calc.IO.metrics
# Calculates Information Matrix Metrics: H, X, Psi, TST, and alpha
# for an input matrix.
#
# BRH: 2017.11.22
#
# Requires 1 argument: rxc matrix
# Returns 1 vector: H,X,Psi,TST,alpha,F
###########################################################################################################################################################################################

# Put your cursor somewhere in the function.
# Then, from the Code menu, choose "Insert Roxygen Skeleton".
# You'll get an R-style comment stub. 
# I suggest using that for comments for functions.
# If these functions every make their way into a package, 
# R will create the nice help files automatically
# during the build process.
# See my attempt below at documenting this function below.
# See http://r-pkgs.had.co.nz/man.html for additional details on the formatting for Roxygen comments.
 

#' Input-Output information metrics
#' 
#' Calculates information metrics for an input-output matrix.
#' All of 
#' \enumerate{
#'   \item \code{TST} (total system throughput)
#'   \item \code{H} (more here),  
#'   \item \code{X} (more here), 
#'   \item \code{Psi} (more here), 
#'   \item \code{alpha} (more here), and 
#'   \item F (more here)
#' }
#' are returned in a list. 
#'
#' @param IO an input-output matrix (more here)
#'
#' @return a named list containing \code{TST}, \code{H}, \code{X}, \code{Psi}, \code{alpha}, and \code{F}.
#' @export 
#'
#' @examples
#' Put any examples here.
calc.IO.metrics <-function(IO) {
	TST=sum(IO)
	N=nrow(IO)
	R<-as.vector(rowSums(IO))
	C<-as.vector(colSums(IO))
	D=1/(R%*%t(C))
	D[is.infinite(D)] <- 0
		temp1 <- as.matrix(log(IO*(TST*D)))
		temp1[is.infinite(temp1)] <- 0
	X=sum(IO*temp1)
		temp1 <- log((IO*IO)*D)
		temp1[is.infinite(temp1)] <- 0
	Psi= -sum(IO*(temp1))
		temp1 <- log(IO/TST)
		temp1[is.infinite(temp1)] <- 0
	H <- -sum(IO*temp1)
	alpha <- X/H
	F	<- -exp(1)*alpha*log(alpha)
	# Return everything as a named list.  
	# Named lists can be cbound into a data frame, 
	# with the list's names used as column titles. 
	list(TST = TST, H = H, X = X, Psi = Psi, alpha = alpha, F = F)
}

calc.TST <- function(IO) {
  return(calc.IO.metrics(IO)["TST"] %>% as.numeric) }
  
calc.alpha <- function(IO) {
    return(calc.IO.metrics(IO)["alpha"] %>% as.numeric) }
    
calc.F <- function(IO) {
      return(calc.IO.metrics(IO)["F"] %>% as.numeric)  }

