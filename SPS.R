###########################################################################
# Source code: SPS.R (SimplePowerSector)
# BRH 12.14.2017
#
# Calls BuildScenarios to perform a scenarios based on a paramater sweep of 
# prices,efficiencies, and market shares.
#
# These scenarios are then used (by mutate) to build a DF of the eurostat matrices 
# for "Model B".
#
# Finally, Save_results.R is called to parse the factors from the scenarios
# and save the results as (1) a .csv file of factors + results (all scalars) and 
# (2) a DF of factors + results + IO phys and curr matrices.
###########################################################################
library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions

source("BuildScenarios.R")
source("Calc_IO_metrics.R")

###########################################################################
# 
# Create all of the eurostat matrices 
# and the IO flow tables that come from them.
#
# MKH question: is it a problem that I get a warning message everytime at this step:
#  1: In matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b)) :
#   data length [12288] is not a sub-multiple or multiple of the number of columns [2]
###########################################################################
DF.eurostat <- DF.scenario.matrices %>% 
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
              sort_rows_cols(.,colorder=(c(industry.names,fin.names))))

###############################################################################
#
# Create the IO metrics results DF
#  
#################################################################################
DF.results <- DF.eurostat  %>% 
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
# Remove intermediate data frames
#########################################################################################################
rm(DF.eurostat,DF.scenario.matrices)
save(DF.results,file="DF.results")

source("Save_results.R")
