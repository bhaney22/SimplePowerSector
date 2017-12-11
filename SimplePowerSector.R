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

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

# The following coding approach is 
# commented out because this doesn't seem to be the right way to build the matrix.
# However, BRH is keeping it in case it is needed to build the scenarios.
#
# Mfg.etas <- data.frame(I1 = 1,I2=1, I3=1/3, I4=0.40, I5=0.40,I6=0.50) %>%
#   gather(key = col.name, value = value, I1,I2,I3,I4,I5,I6) %>% 
#   mutate( matrix.name = "Mfg.etas",
#           col.type="Industries",row.name="Products",row.type="Products") %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Mfg.etas = value)

Mfg.etas.mat <- matrix(rep(c(1, 1, 1/3, 0.4, 0.4, 0.5),Prod.n),
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

# The following coding approach is 
# commented out because this doesn't seem to be the right way to build the matrix.
# However, BRH is keeping it in case it is needed to build the scenarios.
#
# I'm not sure that you need to use collapse to make A.mat. 
# A.mat.2 <- data.frame(value = c(1,1,1,1)) %>% 
#   mutate(
#     rownames = c("P1", "P1", "P2", "P2"),
#     colnames = c("I3", "I4", "I5", "I6"),
#     mat.name = rep.int("A.mat", 4),
#     rowtypes = rep.int("Products", 4),
#     coltypes = rep.int("Industries", 4)
#   ) %>% 
#   collapse_to_matrices(matnames = "mat.name", values = "value",
#                        rownames = "rownames", colnames = "colnames", 
#                        rowtypes = "rowtypes", coltypes = "coltypes") %>% 
#   rename(A.mat = value) 

f.product.coeffs <- matrix(
  c(0.0, 0.0,
    0.0, 0.0,
    0.6, 0.0,
    0.4, 0.0,
    0.0, 0.3, 
    0.0, 0.7),
  nrow = 6, ncol = 2, byrow = TRUE) %>%   
  setrownames_byname(product.names) %>% 
  setrowtype("Products") %>% 
  setcolnames_byname(fin.names) %>% 
  setcoltype("Industries")  

# The following coding approach is 
# commented out because this doesn't seem to be the right way to build the matrix.
# However, BRH is keeping it in case it is needed to build the scenarios.
#
# #
# # Convert from price per physical unit to price per MW -- 
# # BRH comment: Actually, this needs to be done differently (see code below the commented sections)
# # so that during the DF.scenarios
# # build these can be altered.
# # 
# Res.prices <- data.frame(I1 = Convert.prices(55,"MT",curr.scale),
#                          I2 = Convert.prices(3,"MMBTU",curr.scale)) %>% 
#   gather(key = col.name, value = value, I1, I2) %>% 
#   mutate( matrix.name = "Res.prices",
#           col.type="Industries",row.name="Products",row.type="Products") %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Res.prices = value)
# 
# Mfg.prices <- data.frame(I3=1,I4=1,I5=1,I6=1) %>%     ### These are just placeholders, no wholesale trade yet.
#   gather(key = col.name, value = value, I3, I4, I5, I6) %>% 
#   mutate( matrix.name = "Fin.prices",
#           col.type="Industries",row.name="Products",row.type="Products") %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Mfg.prices = value)
# 
# Fin.prices <- data.frame(F1 = Convert.prices(.10,"kWh",curr.scale),
#                          F2 = Convert.prices(.10,"kWh",curr.scale)) %>% 
#   gather(key = col.name, value = value, F1, F2) %>% 
#   mutate( matrix.name = "Fin.prices",
#           col.type="Industries",row.name="Products",row.type="Products") %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Fin.prices = value)


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
#
## The following coding approach is 
# commented out because this doesn't seem to be the right way to build the matrix.
# However, BRH is keeping it in case it is needed to build the scenarios.
#
# Keep NG price at $3, but vary the price of coal
#
# DF.IO.2a <- data.frame(P1 = c(Convert.prices(55,"MT",curr.scale),
#                              Convert.prices(110,"MT",curr.scale),
#                              Convert.prices(220,"MT",curr.scale))) %>%
#   mutate(
#     P2 = 3,
#     sweep.val = P1 # This becomes the metadata column
#   ) %>% 
#   gather(key = col.name, value = value, P1, P2) %>% 
#   mutate( matrix.name = "Res.prices",
#           col.type="Products",row.name="Products",row.type="Products") %>% 
#   group_by(sweep.val) %>%  
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Res.prices = value) %>% 
#   mutate(scenario=2,sweep.factor = "Res.prices") %>%
#   cbind(.,mutate(DF.scenario.factors,scenario=NULL,Res.prices=NULL)) %>%
#   select(order(colnames(.)))
# 
# DF.IO.2b <- data.frame(P2 = c(Convert.prices(3,"MMBTU",curr.scale),
#                              Convert.prices(6,"MMBTU",curr.scale),
#                              Convert.prices(9,"MMBTU",curr.scale))) %>%
#   mutate(
#     P1 = 55,
#     sweep.val = P2 # This becomes the metadata column
#   ) %>% 
#   gather(key = col.name, value = value, P1, P2) %>% 
#   mutate( matrix.name = "Res.prices",
#           col.type="Products",row.name="Products",row.type="Products") %>% 
#   group_by(sweep.val) %>%  
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Res.prices = value) %>% 
#   mutate(scenario=2,sweep.factor = "Res.prices") %>%
#   cbind(.,mutate(DF.scenario.factors,scenario=NULL,Res.prices=NULL)) %>%
#   select(order(colnames(.)))
# 
# #
# Double all Mfg etas
# BRH TODO: this doesn't seem to be working now with the new way BRH created the .mat
#
  # DF.IO.3 <- data.frame(Mfg.etas.mat) %>%
  #   mutate(sweep.val = 2,
  #     Mfg.etas.mat = elementproduct_byname(sweep.val,Mfg.etas.mat), 
  #     scenario=3,sweep.factor = "Mfg.etas.mat") %>%
  #   cbind(.,mutate(DF.scenario.factors,scenario=NULL,Mfg.etas.mat=NULL)) %>%
  #   select(order(colnames(.)))
  
  ##
  ## BRH TODO List #4: Why doesn't this work??? The mutate function uses the
  ## last value of TFO, rather than the TFO on each row.
  ## MKH: Thoughts?
  ##
  # DF.IO.4 <- data.frame( TFO = seq(10, 100, by = 10)) %>%
  #   mutate(
  #     scenario=2,
  #     sweep.factor = "TFO",
  #     sweep.val = TFO
  #   ) %>%
  #   cbind(.,mutate(DF.scenario.factors,scenario=NULL,TFO=NULL)) %>%
  #   select(order(colnames(.)))
  
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
          Z =  elementproduct_byname(Mfg.etas.mat,A.mat),
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
        PRR.phys = TFO/(as.numeric(IO.phys.sumall)-TFO),
        TST.phys = lapply(X=IO.phys, function(X) calc.TST(X)),
        alpha.phys = lapply(X=IO.phys, function(X) calc.alpha(X)),
        F.phys = lapply(X=IO.phys, function(X) calc.F(X)),
        TST.curr = lapply(X=IO.curr, function(X) calc.TST(X)),
        alpha.curr = lapply(X=IO.curr, function(X) calc.alpha(X)),
        F.curr = lapply(X=IO.curr, function(X) calc.F(X)))


 
#########################################################################################################
# End  Results
#########################################################################################################

#########################################################################################################
# Begin Plots
#########################################################################################################


 
 ##################################################################################
 # Function: create.network.pdf
 # Draw the network: help pages are here
 # http://igraph.org/r/doc/graph_from_adjacency_matrix.html 
 ##################################################################################
# 
# Set indexes to point to different nodes.
#
nodes.n	<- Res.n + Mfg.n + Fin.n
Mfg.first	<- Res.n + 1		# node number of first Mfg
Fin.first	<- Res.n + Mfg.n + 1	# node number of first Fin

Res.nodes	<- seq(1,Res.n)
Mfg.nodes	<- seq(Mfg.first,length=Mfg.n)
Fin.nodes	<- seq(Fin.first,length=Fin.n)

   groups 	<- list(	"Input Units" =c(Res.nodes),
                    "Efficiency"=c(Mfg.nodes),
                    "Output Units"=c(Fin.nodes))
   groups.curr	<- list(	"Input Prices"=c(Res.nodes),
                        "Value-Added"=c(Mfg.nodes),
                        "Output Prices"=c(Fin.nodes))
   # 
   shapes <- c(rep("ellipse",Res.n),rep("rectangle",Mfg.n),rep("ellipse",Fin.n))
   L <- matrix(c(
     0,2.5, 0,.5,
     1,3, 1,2, 1,1, 1,0,
     2,2.5, 2,.5),
     ncol=2,byrow=TRUE)
   # 
   # options(width=200)
   # pdf(file=paste(image.dir,"Flows_phys_",run.num,".pdf",sep=""))
   qgraph(DF.results$IO.phys[[5]][,3:8],  ### BRH TODO - figure this out
          edge.labels=T,
          edge.label.cex=1.25,edge.color="black",fade=F,
          # BRH Commented out during csv file creation
          # not working edge.label.position=c(rep(.5,Res.n),rep(.7,Mfg.n),rep(.7,Fin.n)),
          layout=L,
          groups=groups,
          borders=F,
          labels=c(industry.names,fin.names),
          # nodeNames=nodes.names.phys.legend,
          shape=shapes,
          palette="colorblind",
          title=paste("(Flows in MW)",sep=""))
   # dev.off()
   # 
   # options(width=200)
   # pdf(file=paste(image.dir,"Flows_curr_",run.num,".pdf",sep=""))
  #  }




