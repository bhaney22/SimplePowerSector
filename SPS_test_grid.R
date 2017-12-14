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

##########################################################################################################
#
# STEP 1: Pull in the scenario.matrices and choose the scenarios, if this is possible for
# non-scalar factors, such as f.split.
#
#########################################################################################
DF.scenarios <- DF.scenario.matrices %>% filter(TFO==100)
                                                

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
          IO.phys = sum_byname(U,Y) %>% 
            sort_rows_cols(.,colorder=(c(industry.names,fin.names))), 
          IO.curr = elementproduct_byname(IO.phys,Prices.mat) %>% 
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
# Save matrices
#########################################################################################################

save.image()


