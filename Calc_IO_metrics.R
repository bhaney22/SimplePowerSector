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
	results <- cbind(TST,H,X,Psi,alpha,F)
	names(results) <- cbind("TST", "H", "X", "Psi", "alpha", "F")
	results
}
