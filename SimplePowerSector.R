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

Z <- elementquotient_byname(1,Mfg.etas)  %>% 
     matrix(rbind(rep(.,6)),nrow=Prod.n,ncol=Ind.n) %>% 
        setrownames_byname(product.names) %>% 
        setrowtype("Products") %>% 
        setcolnames_byname(industry.names) %>% 
        setcoltype("Industries")  %>%
        elementproduct_byname(.,A.mat) 

D <- transpose_byname(identize_byname(Z))

A <- matrixproduct_byname(Z,D)

q <- matrixproduct_byname(invert_byname(Iminus_byname(A)),y)

V <- matrixproduct_byname(D,hatize_byname(q))

g <- rowsums_byname(V)

U <- matrixproduct_byname(Z,hatize_byname(g))

IO.phys <- cbind(U,Y)

prod.prices.mat <- matrix(cbind(Prod.prices.conv),nrow=Prod.n,ncol=Ind.n) %>% 
    setcolnames_byname(industry.names)  %>% 
    setrownames_byname(product.names) %>%
    setcoltype("Industries") %>%
    setrowtype("Products")

fin.prices.mat <- matrix(cbind(Fin.prices.conv),nrow=Prod.n,ncol=Fin.n) %>%
  setcolnames_byname(fin.names)  %>% 
  setrownames_byname(product.names) %>%
  setcoltype("Industries") %>%
  setrowtype("Products")
 
IO.curr <- elementproduct_byname(cbind(prod.prices.mat,fin.prices.mat),IO.phys) %>%
    sort_rows_cols(.,colorder=(colnames(IO.phys))) 

DF.base <- data.frame(scenario = 1) %>%
  mutate( TFO = TFO,   # <- 200
          Res.n = Res.n,  # <- 2
          Mfg.n	= Mfg.n, #	<- 4		# number of intermediate industries/products
          Fin.n	= Fin.n, #	<- 2 		# number of final output industries (should be perfect complements)
          
          phys.units = phys.units, #	  <-	"MW"
          Res.desc = Res.desc, # 	    <- matrix(c("Coal","NG"))
          Res.units = Res.units, #	    <- c(rep(phys.units,2))
          Res.prices = Res.prices, #   <- c(55,3)
          Res.prices.units = Res.prices.units, #	<- c("MT","MMBTU")
          
          Mfg.prices  = Mfg.prices, #  <- c(rep(1,Mfg.n))    ### Placeholder. There are no interindustry trades 
                                                            ### yet. All of the products are sold to the final 
                                                              ### sector.
          
          Fin.desc = Fin.desc, #	<- c("Res","Com")
          Fin.units	= Fin.units, # <- c(rep(phys.units,2))
          Fin.prices = Fin.prices, # <- c(0.10,0.10)
          Fin.prices.units = Fin.prices.units, #	<- c("kWh","kWh")
          
          Prod.n = Prof.n, # <- Res.n + Mfg.n
          Ind.n = Ind.n, #  <- Res.n + Mfg.n
          
          product.names   = product.names, # <- c(paste0("P",seq(1:Prod.n)))
          industry.names  = industry.names, # <- c(paste0("I",seq(1:Ind.n)))
          fin.names       = fin.names, #<- c(paste0("F",seq(1:Fin.n)))
          
          curr.scale = curr.scale, #	<- 10^(-6)
          curr.scale.display = curr.scale.display, # <- "Millions USD"
          Res.prices.conv = lapply(seq_len(nrow(.)), function(X) {
            Convert.prices(Res.prices,Res.prices.units,curr.scale) }),
          Fin.prices.conv = lapply(seq_len(nrow(.)), function(X) {
            Convert.prices(Fin.prices,Fin.prices.units,curr.scale) }),
          Prod.prices.conv  = lapply(seq_len(nrow(.)), function(X) {
            c(Res.prices.conv,Mfg.prices)}),
          f.split = lapply(seq_len(nrow(.)), function(X) f.split),
          f.product.coeffs = lapply(seq_len(nrow(.)), function(X) f.product.coeffs),
          Y.colsum = elementproduct_byname(TFO, f.split),
          Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
          y = rowsums_byname(Y),
          A.mat = lapply(seq_len(nrow(.)), function(X) A.mat),
          Z = lapply(seq_len(nrow(.)), function(X) Z),
          D = lapply(seq_len(nrow(.)), function(X) D),
          A = lapply(seq_len(nrow(.)), function(X) A),
          q = lapply(seq_len(nrow(.)), function(X) q),
          V = lapply(seq_len(nrow(.)), function(X) V),
          g = lapply(seq_len(nrow(.)), function(X) g),
          U = lapply(seq_len(nrow(.)), function(X) U),
          IO.phys1 = lapply(seq_len(nrow(.)), function(X) IO.phys),  #use 1 in name of Df var
          IO.curr1 = lapply(seq_len(nrow(.)), function(X) IO.curr), #during testing to make
          IO.phys.sumall = sumall_byname(IO.phys1),                 #sure functions are using
          IO.curr.sumall = sumall_byname(IO.curr1))                 #DF mats and not global mats

DF.base <- mutate(DF.base,ENA.phys = lapply(seq_len(nrow(DF.base)),function(X) TST(IO.phys1)))
        

#########################################################################################################
# End of Base scenario DF
#########################################################################################################




DF.scenario1 <- data.frame(val = seq(1, 100, by = 10)) %>% 
  mutate(
    TFO = val,
    scenario.val = val, # This becomes the metadata column
    scenario.factor = "TFO"
  ) %>% 
  mutate(
    f.split = lapply(seq_len(nrow(.)), function(X) f.split),
    f.product.coeffs = lapply(seq_len(nrow(.)), function(X) f.product.coeffs),
    A.mat = lapply(seq_len(nrow(.)), function(X) A.mat),
    Z = lapply(seq_len(nrow(.)), function(X) Z),
    D = lapply(seq_len(nrow(.)), function(X) D),
    A = lapply(seq_len(nrow(.)), function(X) A),
    q = lapply(seq_len(nrow(.)), function(X) q),
    V = lapply(seq_len(nrow(.)), function(X) V),
    g = lapply(seq_len(nrow(.)), function(X) g),
    U = lapply(seq_len(nrow(.)), function(X) U),
    IO.phys1 = lapply(seq_len(nrow(.)), function(X) IO.phys),  #use 1 in name of Df var
    IO.curr1 = lapply(seq_len(nrow(.)), function(X) IO.curr), #during testing to make
    IO.phys.sumall = sumall_byname(IO.phys1),                 #sure functions are using
    IO.curr.sumall = sumall_byname(IO.curr1))  
    



