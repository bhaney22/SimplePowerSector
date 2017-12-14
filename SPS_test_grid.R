library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions

load(".RData")
#
# Set Initial Input scalars, vectors, and matrices
#
#

Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)
nodes.n	<- Res.n + Mfg.n + Fin.n

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

curr.scale	<- 10^(-6)
curr.scale.display <- "millions"

Mfg.etas.mat <- matrix(rep(c(1, 1, .3, 0.4, 0.4, 0.5),Prod.n),
                      nrow = Prod.n, ncol = Ind.n, byrow = TRUE) %>%
                      setrownames_byname(product.names) %>%
                      setrowtype("Products") %>%
                      setcolnames_byname(industry.names) %>%
                      setcoltype("Industries")

A.mat <- matrix(c(0,0,1,1,0,0,
                  0,0,0,0,1,1,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0),
                nrow = Prod.n, ncol = Ind.n, byrow = TRUE) %>%
  setrownames_byname(product.names) %>%
  setrowtype("Products") %>%
  setcolnames_byname(industry.names) %>%
  setcoltype("Industries")

Prices.mat <- matrix(c(rep(Convert.prices(55,"MT",curr.scale),Ind.n),0,0,   # Row 1: P1 
                       rep(Convert.prices(3,"MMBTU",curr.scale),Ind.n),0,0, # Row 2: P2 
                       rep(c(0,0,0,0,0,0,
                       Convert.prices(0.10,"kWh",curr.scale),    # Row 3-6: P3-P6=0
                       Convert.prices(0.10,"kWh",curr.scale)),Mfg.n)),   # then F1,F2
                       nrow = Prod.n, ncol = Ind.n + Fin.n, byrow = TRUE) %>% 
  setrownames_byname(product.names) %>%
  setrowtype("Products") %>%
  setcolnames_byname(c(industry.names,fin.names)) %>%
  setcoltype("Industries")

##########################################################################################################
#
# STEP 1: Add the static matrices to each scenario in the grid
#
#########################################################################################
DF.scenarios <- DF.scenario.matrices %>%
  rename(TFO=tfo, Mfg.etas.mat=mfg.etas,Prices.mat=prices)  %>%
  mutate(A.mat = lapply(X=TFO, function(X) A.mat)) %>%
          select(order(colnames(.)))

###########################################################################
# 
# STEP 3:
# Once all scenarios are built, create all of the eurostat matrices 
# and the IO flow tables that come from them.
#
#  1: In matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b)) :
#   data length [17] is not a sub-multiple or multiple of the number of columns [2]
###########################################################################
DF.eurostat <- data.frame(DF.scenarios) %>% 
  mutate( Y.colsum = elementproduct_byname(TFO, f.split),
          Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
          y = rowsums_byname(Y),
          Z =  elementquotient_byname(A.mat,Mfg.etas.mat),
          D = transpose_byname(identize_byname(Z)),
          A = matrixproduct_byname(Z,D),
          q = matrixproduct_byname(invert_byname(Iminus_byname(A)),y),
          V = matrixproduct_byname(D,hatize_byname(q)),
          g = rowsums_byname(V),
          U = matrixproduct_byname(Z,hatize_byname(g)), 
          IO.phys = sum_byname(U,Y), 
          IO.curr = elementproduct_byname(IO.phys,Prices.mat)   %>% 
              sort_rows_cols(.,colorder=(c(industry.names,fin.names))))

###############################################################################
#
# STEP 4: Run the results
# BRH TODO: calculate PRR in currency units. 
#################################################################################
DF.results <- data.frame(DF.eurostat) %>% 
  mutate(
        IO.phys.sumall = sumall_byname(IO.phys),
        IO.curr.sumall = sumall_byname(IO.curr),
        GDP.curr = sumall_byname(select_cols_byname(IO.curr,retain_pattern = "^F")),
        PRR.phys = TFO/(as.numeric(IO.phys.sumall)-TFO),
        PRR.curr = as.numeric(GDP.curr)/(as.numeric(IO.curr.sumall)-as.numeric(GDP.curr)),
        TST.phys = lapply(X=IO.phys, function(X) calc.TST(X)),
        alpha.phys = lapply(X=IO.phys, function(X) calc.alpha(X)),
        F.phys = lapply(X=IO.phys, function(X) calc.F(X)),
        TST.curr = lapply(X=IO.curr, function(X) calc.TST(X)),
        alpha.curr = lapply(X=IO.curr, function(X) calc.alpha(X)),
        F.curr = lapply(X=IO.curr, function(X) calc.F(X)))
          



 
#########################################################################################################
# End  Results
#########################################################################################################

save.image()


