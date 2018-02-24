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
library(matsbyname)   # Provides convenient matrix manipulation in data frames.
library(parallel)     # For the function mcMap.  (Not sure why this didn't come in with matsbyname.)
library(magrittr)     # For the pipe operator (%>%)
library(dplyr)        # For mutate and other helpful functions
library(tidyr)        # For spread and gather functions
library(tibble)       # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval)     # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf)     # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)      # For awesome plotting functions

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
DF.eurostat <- data.frame(DF.scenario.matrices) %>% 
  mutate( Y.colsum = elementproduct_byname(TFO, F.split),
          Y = matrixproduct_byname(F.product.coeffs,hatize_byname(Y.colsum)),
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
              sort_rows_cols(.,colorder=(c(industry.names,fin.names)))) %>%
        select(.,-(Y.colsum:U))

###############################################################################
#
# Create the IO metrics results DF
#  
#################################################################################
DF.results <- data.frame(DF.eurostat)  %>% 
  mutate(
        IO.phys.sumall = sumall_byname(IO.phys),
        IO.curr.sumall = sumall_byname(IO.curr),
        GDP.curr = as.numeric(sumall_byname(select_cols_byname(IO.curr,retain_pattern = "^F"))),
        PRR.phys = TFO/(as.numeric(IO.phys.sumall)-TFO),  # Net Energy Ratio Economy Wide
        PRR.curr = as.numeric(GDP.curr)/(as.numeric(IO.curr.sumall)-as.numeric(GDP.curr)),  #Value-Added Economy Wide
        TST.phys = sapply(X=IO.phys, function(X) calc.TST(X)),
        alpha.phys = sapply(X=IO.phys, function(X) calc.alpha(X)),
        F.phys = sapply(X=IO.phys, function(X) calc.F(X)),
        TST.curr = sapply(X=IO.curr, function(X) calc.TST(X)),
        alpha.curr = sapply(X=IO.curr, function(X) calc.alpha(X)),
        F.curr = sapply(X=IO.curr, function(X) calc.F(X)),                
        Flows.phys=lapply(X=IO.phys, function(IO.phys) {
          matrix(0,nrow=nodes.n,ncol=nodes.n)  %>%
            setrownames_byname(c(product.names,fin.names)) %>%
            setrowtype("Products") %>%
            setcolnames_byname(c(industry.names,fin.names)) %>%
            setcoltype("Industries") %>%
            sum_byname(.,IO.phys) %>% 
            sort_rows_cols(roworder=(c(product.names,fin.names)),
                           colorder=(c(industry.names,fin.names)))
        }),
        Flows.curr=lapply(X=IO.curr, function(IO.curr){ 
          matrix(0,nrow=nodes.n,ncol=nodes.n) %>%
            setrownames_byname(c(product.names,fin.names)) %>%
            setrowtype("Products") %>%
            setcolnames_byname(c(industry.names,fin.names)) %>%
            setcoltype("Industries") %>%
            sum_byname(.,IO.curr) %>% 
            sort_rows_cols(roworder=(c(product.names,fin.names)),
                           colorder=(c(industry.names,fin.names)))
        }),
        VA.3=sapply(X=Flows.curr,function(Flows.curr)
          sum(Flows.curr[Mfg.nodes[1],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[1]])),
        VA.4=sapply(X=Flows.curr,function(Flows.curr)
          sum(Flows.curr[Mfg.nodes[2],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[2]])),
        VA.5=sapply(X=Flows.curr,function(Flows.curr)
          sum(Flows.curr[Mfg.nodes[3],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[3]])),
        VA.6=sapply(X=Flows.curr,function(Flows.curr)
          sum(Flows.curr[Mfg.nodes[4],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[4]])),
        Mfg.etas.mat=NULL,Prices.mat=NULL,f.split=NULL,f.product.coeffs=NULL)

save(DF.results,file="DF.results.Rda")

