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
#
# Initial Input Parameters:
#

TFO     <- 200  # total final output
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)

phys.units	  <-	"MW"
Res.desc 	    <- matrix(c("Coal","NG"))
Res.units	    <- c(rep(phys.units,2))
Res.prices.units	<- c("MT","MMBTU")
Res.prices.conv	<- c(0,0)

Fin.desc	<- c("Res","Com")
Fin.units	<- c(rep(phys.units,2))
Fin.prices.units	<- c("kWh","kWh")
Fin.prices.conv 	<- c(0,0)

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n
curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

product.names   <- c(paste0("P", seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

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

Y.colsum <- elementproduct_byname(TFO, f.split)
Y <- matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum))
y <- rowsums_byname(Y)

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

D <- identize_byname(Z)

A <- matrixproduct_byname(Z,transpose_byname(D))

q <- matrixproduct_byname(invert_byname(Iminus_byname(A)),y)

V <- matrixproduct_byname(transpose_byname(D),hatize_byname(q))

g <- rowsums_byname(V)

U <- matrixproduct_byname(Z,hatize_byname(g))

IO <- cbind(U,Y)

DF.base <- data.frame(scenario = 1) %>%
  mutate( f.split = lapply(seq_len(nrow(.)), function(X) f.split),
          f.product.coeffs = lapply(seq_len(nrow(.)), function(X) f.product.coeffs),
          A.mat = lapply(seq_len(nrow(.)), function(X) A.mat),
          Z = lapply(seq_len(nrow(.)), function(X) Z),
          D = lapply(seq_len(nrow(.)), function(X) D),
          A = lapply(seq_len(nrow(.)), function(X) A),
          q = lapply(seq_len(nrow(.)), function(X) q),
          V = lapply(seq_len(nrow(.)), function(X) V),
          g = lapply(seq_len(nrow(.)), function(X) g),
          U = lapply(seq_len(nrow(.)), function(X) U),
          IO = lapply(seq_len(nrow(.)), function(X) IO) )


  mutate(
          ENA = lapply(seq_len(nrow(.)), function(X) calc.IO.metrics(IO)  )  )

DF.scenario1 <- data.frame(F1 = seq(0, 1, by = 0.1)) %>% 
  mutate(
    F2 = 1 - F1,
    scenario.val = F1, # This becomes the metadata column
    scenario.factor = "f"
  ) %>% 
  # Use gather to form a tidy data frame that can be converted into matrices
  # (or, in this case, vectors).
  gather(key = col.name, value = value, F1, F2) %>% 
  # Add other metadata columns that will be required before 
  # collapsing into matrices.
  mutate(
    matrix.name = "f",
    row.name = "Products",
    row.type = "Products",
    col.type = "Industries"
   ) %>% 
  # Set grouping to preserve metadata columns
  group_by(scenario.factor,scenario.val) %>% 
  # Collapse to form matrices (actually, vectors)
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name", 
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(
    f = value  # For readability.  These are f vectors.
  ) %>% 
  # At this point, we now have our column of f vectors.
  # Calculate g and V accordingly.
  mutate(
    f.mat = lapply(seq_len(nrow(.)), function(X) f.mat), 
    ysum = elementproduct_byname(TFO, f),
    Y = matrixproduct_byname(f.mat,hatize_byname(ysum)),
    y = rowsums_byname(Y)
    
  )


