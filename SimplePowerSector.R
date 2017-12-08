library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions

rm(list=ls())
source("Calc_IO_metrics.R")
source("Conversions.R")
#
# Initial Input Parameters:
#

TFO     <- 100  # total final output
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)

phys.units	  <-	"MW"
Res.desc 	    <- matrix(c("Coal","NG"))
Res.units	    <- c(rep(phys.units,2))
Res.prices    <- c(55,3)
Res.prices.units	<- c("MT","MMBTU")

Mfg.prices    <- c(rep(1,Mfg.n))    ### Placeholder. There are no interindustry trades 
                                    ### yet. All of the products are sold to the final 
                                    ### sector.

Fin.desc	<- c("Res","Com")
Fin.units	<- c(rep(phys.units,2))
Fin.prices <- c(0.10,0.10)
Fin.prices.units	<- c("kWh","kWh")

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

Res.prices.conv <- Convert.prices(Res.prices,Res.prices.units,curr.scale) 
Fin.prices.conv <- Convert.prices(Fin.prices,Fin.prices.units,curr.scale)
Prod.prices.conv <- c(Res.prices.conv,Mfg.prices)

f.split <- matrix(c(.5,.5),
                  nrow = 1, ncol = 2, byrow = TRUE) %>%   
  setrownames_byname("Products") %>% 
  setrowtype("Products") %>% 
  setcolnames_byname(fin.names) %>% 
  setcoltype("Industries") 

f.product.coeffs <- matrix(
            c(0,0,
              0,0,
              0.6,0,
              0.4, 0.0,
              0, 0.3, 
              0.0, .7),
            nrow = 6, ncol = 2, byrow = TRUE) %>%   
            setrownames_byname(product.names) %>% 
            setrowtype("Products") %>% 
            setcolnames_byname(fin.names) %>% 
            setcoltype("Industries")  

A.mat <- matrix(c(0,0,1,1,0,0,
                  0,0,0,0,1,1,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0, 
                  0,0,0,0,0,0),
                nrow = 6, ncol = 6, byrow = TRUE) %>%   
  setrownames_byname(product.names) %>% 
  setrowtype("Products") %>% 
  setcolnames_byname(industry.names) %>% 
  setcoltype("Industries")  

Mfg.etas <- matrix(c(1,1,1/3,0.40,0.40,0.50),nrow=1,ncol=Ind.n,byrow=T) %>%
  setrownames_byname("Products") %>% 
  setrowtype("Products") %>% 
  setcolnames_byname(industry.names) %>% 
  setcoltype("Industries") 


prices.mat <- cbind(
    prod.prices.mat <- matrix(cbind(Prod.prices.conv),nrow=Prod.n,ncol=Ind.n) %>% 
  setcolnames_byname(industry.names)  %>% 
  setrownames_byname(product.names) %>%
  setcoltype("Industries") %>%
  setrowtype("Products"),
    fin.prices.mat <- matrix(cbind(Fin.prices.conv),nrow=Prod.n,ncol=Fin.n) %>%
  setcolnames_byname(fin.names)  %>% 
  setrownames_byname(product.names) %>%
  setcoltype("Industries") %>%
  setrowtype("Products"))

DF.base <- data.frame(scenario = 1) %>%
  mutate( TFO = TFO,   # <- 200
          Res.n = Res.n,  # <- 2
          Mfg.n	= Mfg.n, #	<- 4		# number of intermediate industries/products
          Fin.n	= Fin.n, #	<- 2 		# number of final output industries (should be perfect complements)
          
          phys.units = phys.units, #	  <-	"MW"
          Res.desc = lapply(X=scenario, function(X) {Res.desc}), # 	    <- matrix(c("Coal","NG"))
          Res.units = lapply(X=scenario, function(X) {Res.units}), #	    <- c(rep(phys.units,2))
          Res.prices = lapply(X=scenario, function(X) {Res.prices}), #   <- c(55,3)
          Res.prices.units = lapply(X=scenario, function(X) {Res.prices.units}), #	<- c("MT","MMBTU")
          
          Mfg.prices  = lapply(X=scenario, function(X) {Mfg.prices}), #  <- c(rep(1,Mfg.n))    ### Placeholder. There are no interindustry trades 
                                                            ### yet. All of the products are sold to the final 
                                                              ### sector.
          
          Fin.desc = lapply(X=scenario, function(X) {Fin.desc}), #	<- c("Res","Com")
          Fin.units	= lapply(X=scenario, function(X) {Fin.units}), # <- c(rep(phys.units,2))
          Fin.prices = lapply(X=scenario, function(X) {Fin.prices}), # <- c(0.10,0.10)
          Fin.prices.units = lapply(X=scenario, function(X) {Fin.prices.units}), #	<- c("kWh","kWh")
  
          Prod.n = Prod.n, # <- Res.n + Mfg.n
          Ind.n = Ind.n , #  <- Res.n + Mfg.n

          curr.scale = curr.scale, #	<- 10^(-6)
          curr.scale.display = curr.scale.display, # <- "Millions USD"
 #         Res.prices.conv = lapply(X=scenario, function(X) {
 #            Convert.prices(Res.prices,Res.prices.units,curr.scale) })  )#,
 #          Fin.prices.conv = lapply(X=scenario, function(X){
 #            list(Convert.prices(Fin.prices,Fin.prices.units,curr.scale)) }),
 #          Prod.prices.conv  = lapply(X=scenario, function(X) {
#            c(Res.prices.conv,Mfg.prices)}),
          f.split = lapply(X=scenario, function(X) f.split),
          f.product.coeffs = lapply(X=scenario, function(X) f.product.coeffs),
#
# Begin building Eurostat matrices from input factors here:
#
          Y.colsum = elementproduct_byname(TFO, f.split),
          Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
          y = rowsums_byname(Y),
          A.mat = lapply(X=scenario, function(X) A.mat),
          Z = lapply(X=scenario, function(X) {matrix(elementquotient_byname(1,Mfg.etas),nrow=1)  %>% 
              matrix(rbind(rep(.,6)),nrow=Prod.n,ncol=Ind.n)  %>% 
              setrownames_byname(product.names ) %>% 
              setrowtype("Products") %>% 
              setcolnames_byname(industry.names) %>% 
              setcoltype("Industries")  %>%
              elementproduct_byname(.,A.mat)})  ,
          D = transpose_byname(identize_byname(Z)),
          A = matrixproduct_byname(Z,D),
          q = matrixproduct_byname(invert_byname(Iminus_byname(A)),y),
          V = matrixproduct_byname(D,hatize_byname(q)),
          g = rowsums_byname(V),
          U = matrixproduct_byname(Z,hatize_byname(g))   )
  DF.base <- mutate(DF.base,  
                    IO.phys = lapply(X=scenario, function(X) {
                      matrix(unlist(cbind(U,Y)),
                             nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ) %>%
                        setcolnames_byname(c(industry.names,fin.names))  %>% 
                        setrownames_byname(product.names) %>%
                        setcoltype("Industries") %>%
                        setrowtype("Products")}) ) 
  DF.base <- mutate(DF.base, 
                    IO.phys.sumall = sumall_byname(IO.phys),
                    TST.phys = lapply(X=scenario, function(X) { ## should be same as sumall
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["TST"] )}),
                    alpha.phys = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["alpha"] )}),
                    F.phys = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["F"] )}))  
 
  DF.base <- mutate(DF.base,
                    IO.curr = lapply(X=scenario, function(X) {
                      elementproduct_byname(prices.mat,IO.phys) }))
              #      %>% sort_rows_cols(.,colorder=(c(industry.names,fin.names))) })  ) ##########  
  
  DF.base <- mutate(DF.base, 
                    IO.curr.sumall = sumall_byname(IO.curr),
                    TST.curr = lapply(X=scenario, function(X) { ## should be same as sumall
                      unlist(calc.IO.metrics(matrix(unlist(IO.curr),
                                                    nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["TST"] )}),
                    alpha.curr = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.curr),
                                                    nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["alpha"] )}),
                    F.curr = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.curr),
                                                    nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["F"] )})) 

  #################################################################################################
  ########  Up to this point the code works. But, 
  ########   1) is there a way to not have to unlist and rebuilding the matrices?
  ########   2) re-order the columns of the result of the elementproduct_byname? 
  ###############################################################################################
  
#########################################################################################################
# End of Base scenario DF
#########################################################################################################

##
  ## Work in progress:
  ##
  
DF.scenario1 <- data.frame(val = seq(100, 1000, by = 100),DF.base)  %>% 
  mutate(
    scenario.factor = "TFO",
    TFO = val,
    val=NULL)

  ##
  ## ACK!! Why doesn't Y.colsum work anymore? It worked in DF.base.
  ##
  DF.scenario1 <- mutate(DF.scenario1,
                         Y.colsum = elementproduct_byname(TFO, f.split))
   

