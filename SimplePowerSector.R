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
# Eventually this code will go away as the initial values are built "in line" in the data frame.
# For now however, leave as is....skip to the first DF <- data.frame....statement to begin
# work.
#

# TFO     <- 100  # total final output
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


########################
# Evenutally the above code will be incorporated into the DF.scenario.factors build
# "In line". But, will leave it this way for now and focus on getting the other aspects
# correct.
########################


DF.scenario.factors <- data.frame(scenario = seq(0,1)) %>%
  mutate( TFO = c(100,200),
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
          
  ### The below lines are commented out because I could not get them to
  ### work correctly "in line." For now, I convert the prices outside
  ### the DF build like everything else.
  #         Res.prices.conv = lapply(X=scenario, function(X) {
 #            Convert.prices(Res.prices,Res.prices.units,curr.scale) })  )#,
 #          Fin.prices.conv = lapply(X=scenario, function(X){
 #            list(Convert.prices(Fin.prices,Fin.prices.units,curr.scale)) }),
 #          Prod.prices.conv  = lapply(X=scenario, function(X) {
 #            c(Res.prices.conv,Mfg.prices)}),
          f.split = lapply(X=scenario, FUN = function(X) f.split),
          f.product.coeffs = lapply(X=scenario, FUN = function(X) f.product.coeffs))

###
### End of Scenarios df build
###

#
# Begin building Eurostat matrices using all of the rows of input factors 
# based on the different scenarios:
#

############################################################################################
# MKH work area:

### HERE BEGINS THE PROBLEM. When there is more than one scenario in the DF.scenario.factors
### data.frame, the following
### code does not compute correctly. 
###
### It would be helpful if you could see what
### needs to be adjusted - either how the DF.scenario.factors are built or
### how the DF.eurostat uses them to build out the matrices.  Or how the process
### works in general.
###
### I assume that if you get it working correctly for the first variable in the 
### DF.eurostat, the rest will fall into place. It will be easy to double check that...
### The second DF.eurostat statement (after the commented line below) builds out everything.
###
# DF.eurostat <- DF.scenario.factors %>% 
#   mutate(
#     Y.colsum = elementproduct_byname(TFO, f.split)
#   )
#   
#############  The following commands build out the entire DF.eurostat  ###################
#  DF.eurostat <- DF.scenario.factors %>% 
#   mutate(
#     Y.colsum = elementproduct_byname(TFO, f.split),
#           Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
#           y = rowsums_byname(Y),
#           A.mat = lapply(X=scenario, function(X) A.mat),
#           Z = lapply(X=scenario, FUN = function(X) Mfg.etas  %>% 
#               matrix(rbind(rep(.,Prod.n)),nrow=Prod.n,ncol=Ind.n)  %>% 
#               setrownames_byname(product.names ) %>% 
#               setrowtype("Products") %>% 
#               setcolnames_byname(industry.names) %>% 
#               setcoltype("Industries")  %>%
#               elementquotient_byname(A.mat,.)),
#           D = transpose_byname(identize_byname(Z)),
#           A = matrixproduct_byname(Z,D),
#           q = matrixproduct_byname(invert_byname(Iminus_byname(A)),y),
#           V = matrixproduct_byname(D,hatize_byname(q)),
#           g = rowsums_byname(V),
#           U = matrixproduct_byname(Z,hatize_byname(g))   )
# 
# DF.eurostat %>% 
#   mutate( IO.phys = lapply(X=scenario, function(X) {
#                       matrix(unlist(cbind(U,Y)),
#                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ) %>%
#                         setcolnames_byname(c(industry.names,fin.names))  %>% 
#                         setrownames_byname(product.names) %>%
#                         setcoltype("Industries") %>%
#                         setrowtype("Products")}), 
#           
#                     IO.phys.sumall = sumall_byname(IO.phys),
#           
#                     TST.phys = lapply(X=scenario, function(X) { ## should be same as sumall
#                       unlist(calc.IO.metrics(matrix(unlist(IO.phys),
#                               nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["TST"] )}),
#           
#                     alpha.phys = lapply(X=scenario, function(X) {
#                       unlist(calc.IO.metrics(matrix(unlist(IO.phys),
#                               nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["alpha"] )}),
#           
#                     F.phys = lapply(X=scenario, function(X) {
#                       unlist(calc.IO.metrics(matrix(unlist(IO.phys),
#                               nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["F"] )}),
#           
#         IO.curr = lapply(X=scenario, function(X) {
#                       elementproduct_byname(prices.mat,IO.phys) }),
#               #      %>% sort_rows_cols(.,colorder=(c(industry.names,fin.names))) })  ) ##########  
#                     IO.curr.sumall = sumall_byname(IO.curr),
#                     TST.curr = lapply(X=scenario, function(X) { ## should be same as sumall
#                       unlist(calc.IO.metrics(matrix(unlist(IO.curr),
#                               nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["TST"] )}),
#                     alpha.curr = lapply(X=scenario, function(X) {
#                       unlist(calc.IO.metrics(matrix(unlist(IO.curr),
#                                                     nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["alpha"] )}),
#                     F.curr = lapply(X=scenario, function(X) {
#                       unlist(calc.IO.metrics(matrix(unlist(IO.curr),
#                                                     nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["F"] )})) 

#########################################################################################################
# End of Eurostat DF
#########################################################################################################
#
# End of MKH work area. To avoid code conflicts, I will work only below this area.
#
# Beginning of BRH work area:
#
##
## Example scenario build using gather & collapse:
##
# DF3 <- data.frame(i1 = seq(0, 1, by = 0.1)) %>% 
#   mutate(
#     i2 = 1 - i1,
#     f1 = i1 # This becomes the metadata column
#   ) %>% 
#   gather(key = row.name, value = value, i1, i2) %>%
#   mutate(
#     matrix.name = "f",col.name="Products",row.type="Industries",col.type="Products") %>% 
#   group_by(f1) %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name", 
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(f = value) %>% 
#   mutate(
#     g = elementproduct_byname(tfo, f),
#     V = matrixproduct_byname(C, hatize_byname(g)) %>% transpose_byname()
#   )
  ##
  ## Work in progress:
  ##
    DF.s1 <- data.frame(F1 = seq(0, 1, by = 0.1)) %>%
      mutate(
        scenario=1,
        F2 = 1 - F1,
        sweep.val = F1 # This becomes the metadata column
      ) %>% 
      gather(key = col.name, value = value, F1, F2) %>% 
      mutate( matrix.name = "f.split",
              col.type="Industries",row.name="Products",row.type="Products") %>% 
      group_by(sweep.val) %>%  
      collapse_to_matrices(matnames = "matrix.name", values = "value",
                           rownames = "row.name", colnames = "col.name",
                           rowtypes = "row.type", coltypes = "col.type") %>% 
      rename(f.split = value) %>%
      cbind(.,mutate(DF.scenario.factors,scenario=NULL,f.split=NULL)) %>% 
      mutate(    Y.colsum = elementproduct_byname(TFO, f.split))
View(DF.s1)
