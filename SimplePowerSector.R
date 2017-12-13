library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions
library(statnet)
library(igraph)
library(qgraph)

rm(list=ls())
source("Calc_IO_metrics.R")
source("Conversions.R")
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

f.split <- data.frame(F1 = 0.5, F2 = 0.5) %>%
  gather(key = col.name, value = value, F1, F2) %>% 
  mutate( matrix.name = "f.split",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(f.split = value) 

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

f.product.coeffs <- matrix(
  c(0.0, 0.0,
    0.0, 0.0,
    0.25, 0.4,
    0.25, 0.1,
    0.25, 0.2, 
    0.25, 0.3),
  nrow = 6, ncol = 2, byrow = TRUE) %>%   
  setrownames_byname(product.names) %>% 
  setrowtype("Products") %>% 
  setcolnames_byname(fin.names) %>% 
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
# STEP 1: BUILD OUT THE INITIAL INPUT FACTOR VARIABLES FOR "SCENARIO 0"
#
#########################################################################################

DF.scenario.factors <- data.frame(scenario = 0) %>%
  mutate( TFO = 100,
          f.product.coeffs = lapply(X=scenario, function(X) f.product.coeffs),
          Mfg.etas.mat = lapply(X=scenario, function(X) Mfg.etas.mat),
          Prices.mat = lapply(X=scenario, function(X) Prices.mat),
          A.mat = lapply(X=scenario, function(X) A.mat))%>%
          cbind(.,f.split) %>%
          select(order(colnames(.)))



###############################################################################
#
# STEP 2: BUILD OUT SCENARIOS
# One working prototype scenario - for a vector f.split. BRH can't build sweeps for
# any other datatypes yet.
# BRH TODO: figure out how to make new scenarios for matrices: Mfg.etas.mat, Prices.mat
# and f.product.coeffs (matrix)
#
# MKH question: ideas how to do this? Do I need to construct them differently initially to
# by in a data.frame like f.split?
#
###############################################################################
#
# Change percent of electricity used by final output sectors
# (although BRH thinks they should be called industries because this is a model built
#  at the scale of one sector - several industries within it. The final output
#  good goes to another industry - not to the final demand sector.)
#
    DF.IO.1 <- data.frame(F1 = seq(0, 1, by = 0.1)) %>%
      mutate(
        F2 = 1 - F1,
        sweep.val = F1 
      ) %>% 
      gather(key = col.name, value = value, F1, F2) %>% 
      mutate( matrix.name = "f.split",
              col.type="Industries",row.name="Products",row.type="Products") %>% 
      group_by(sweep.val) %>%  
      collapse_to_matrices(matnames = "matrix.name", values = "value",
                           rownames = "row.name", colnames = "col.name",
                           rowtypes = "row.type", coltypes = "col.type") %>% 
      rename(f.split = value) %>% 
      mutate(scenario=1,sweep.factor = "f.split") %>%
      cbind(.,mutate(DF.scenario.factors,scenario=NULL,f.split=NULL)) %>%
      select(order(colnames(.)))

DF.scenarios <- data.frame(rbind(DF.IO.1))
###########################################################################
# 
# STEP 3:
# Once all scenarios are built, create all of the eurostat matrices 
# and the IO flow tables that come from them.
#
# MKH Question: The following warning gets thrown for every observation
# when building the eurostat DF. From my trying different things, I think
# it has something to do with the Mfg.etas.mat * A.mat, but not sure.
# 
# Warning messages:
#   1: In matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b)) :
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
          IO.phys = sum_byname(U,Y) %>% 
            sort_rows_cols(.,colorder=(c(industry.names,fin.names))),
          IO.curr = elementproduct_byname(IO.phys,Prices.mat) %>% 
              sort_rows_cols(.,colorder=(c(industry.names,fin.names)))  )


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




