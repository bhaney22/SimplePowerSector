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
# Set Initial Input scalars, vectors, and matrices
#
# >>>>>>> New code BRH 2017.12.08
#
# Make individual data.frames of vector or matrix input factors that are then cbinded to scalar inputs.
# At the end of the DF.scenario.factors build, there is a cbind statement 
# to put these in.
# 
# 
# MKH TODO List: Can you show me how to:
#     1. Make the A.mat (and other matrices) into a data.frame that
#        uses gather/collapse like the Mfg.etas vector? I think I only
#        have this figured out for vectors. 
#     2. "Cbind" in byname world. We thought that the sum_byname would work
#        like that, but it didn't with my data objects. I have two places
#        where I need to do this: 
#            a. to create the IO.phys = cbind(U,Y) and
#            b. to make the prices matrix that is element by element
#                multiplied to IO.phys to get the IO.curr matrix.
#     3. Create the Z matrix.
#     4. How to mutate TFO and have it work correctly in elementproduct with f.split 
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

#
# Yeah! This works!
#
Mfg.etas <- data.frame(I1 = 1,I2=1, I3=1/3, I4=0.40, I5=0.40,I6=0.50) %>%
  gather(key = col.name, value = value, I1,I2,I3,I4,I5,I6) %>% 
  mutate( matrix.name = "Mfg.etas",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Mfg.etas = value) 

f.split <- data.frame(F1 = .5, F2=.5) %>%
  gather(key = col.name, value = value, F1, F2) %>% 
  mutate( matrix.name = "f.split",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(f.split = value) 

#
# Boo! I don't know how to byname-ize a matrix, only vectors like above. --brh
#
# MKH TODO list #1 project:
#
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

# I'm not sure that you need to use collapse to make A.mat. 
# The code above is just fine.
# But if you want to use collapse, you might try the code below.
# Note that you DON'T need to create all rows and columns, 
# only rows and columns for which non-zero entries are present.
# The _byname functions will add rows or columns of zeroes as needed
# When A.mat is an operand in one of the functions.
A.mat.2 <- data.frame(value = c(1,1,1,1)) %>% 
  mutate(
    rownames = c("P1", "P1", "P2", "P2"),
    colnames = c("I3", "I4", "I5", "I6"),
    mat.name = rep.int("A.mat", 4),
    rowtypes = rep.int("Product", 4),
    coltypes = rep.int("Industries", 4)
  ) %>% 
  collapse_to_matrices(matnames = "mat.name", values = "value",
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  rename(A.mat = value)
  

#
# BRH TODO: Byname-ize the following matrix after shown how to do it.
#
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


#
# Convert from price per physical unit to price per MW
# 
#
# Yeah! BRH byname-ized and converted units for all of the price vectors!
#
Res.prices <- data.frame(P1 = Convert.prices(55,"MT",curr.scale),
                         P2 = Convert.prices(3,"MMBTU",curr.scale)) %>% 
  gather(key = col.name, value = value, P1, P2) %>% 
  mutate( matrix.name = "Res.prices",
          col.type="Products",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Res.prices = value)

Mfg.prices <- data.frame(P3=1,P4=1,P5=1,P6=1) %>%     ### These are just placeholders, no wholesale trade yet.
  gather(key = col.name, value = value, P5, P6) %>% 
  mutate( matrix.name = "Fin.prices",
          col.type="Products",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Mfg.prices = value)

Fin.prices <- data.frame(F1 = Convert.prices(.10,"kWh",curr.scale),
                         F2 = Convert.prices(.10,"kWh",curr.scale)) %>% 
  gather(key = col.name, value = value, F1, F2) %>% 
  mutate( matrix.name = "Fin.prices",
          col.type="Products",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Fin.prices = value)

#
# Boo! The following code isn't working the way I need it to.
#
#

# All of the prices are ultimately used to create the prices matrix that is the same
# dimension and col/row names as the IO.phys matrix. 
#

# MKH TODO list #2 project: How do I need to created my data objects so that they 
# work with sum_byname? (Same problem that has to be solved for 
# IO.phys = sum_byname(U,Y), which didn't work as expected either.)
#

prices.mat <- sum_byname(Res.prices,Mfg.prices) %>% sum_byname(.,Fin.prices)



##########################################################################################################


DF.scenario.factors <- data.frame(scenario = 0) %>%
  mutate( TFO = 200,
          A.mat = lapply(X=scenario, function(X) A.mat),
          f.product.coeffs = lapply(X=scenario, function(X) f.product.coeffs))%>%
##
## cbind all of the initial matrices here. Hopefully the two holdouts above
## A.mat and f.product.coeffs will be added to the cbind list soon!
##
          cbind(.,Mfg.etas,Res.prices,Mfg.prices,Fin.prices,f.split) %>%
          select(order(colnames(.)))
###############################################################################
#
# NEW SECTION: BUILD OUT SCENARIOS
# Three working prototype scenarios are built -- brh
#
###############################################################################

    DF.s1 <- data.frame(F1 = seq(0, 1, by = 0.1)) %>%
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
# Keep NG price at $3, but vary the price of coal
#
DF.s2 <- data.frame(P1 = seq(55,555,by=100)) %>%
  mutate(
    P2 = 3,
    sweep.val = 
      P1 # This becomes the metadata column
  ) %>% 
  gather(key = col.name, value = value, P1, P2) %>% 
  mutate( matrix.name = "Res.prices",
          col.type="Products",row.name="Products",row.type="Products") %>% 
  group_by(sweep.val) %>%  
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Res.prices = value) %>% 
  mutate(scenario=2,sweep.factor = "Res.prices") %>%
  cbind(.,mutate(DF.scenario.factors,scenario=NULL,Res.prices=NULL)) %>%
  select(order(colnames(.)))

#
# Double all Mfg etas
#
  DF.s3 <- data.frame(Mfg.etas) %>%
    mutate(
      sweep.val = 2,
      Mfg.etas = elementproduct_byname(sweep.val,Mfg.etas), 
      scenario=3,sweep.factor = "Mfg.etas") %>%
    cbind(.,mutate(DF.scenario.factors,scenario=NULL,Mfg.etas=NULL)) %>%
    select(order(colnames(.)))
  
  ###
  ### MKH TODO List #4: Why doesn't this one work???
  ###
  # DF.s4 <- data.frame( TFO = seq(10, 100, by = 10)) %>%
  #   mutate(
  #     scenario=2,
  #     sweep.factor = "TFO",
  #     sweep.val = TFO 
  #   ) %>%
  #   cbind(.,mutate(DF.scenario.factors,scenario=NULL,TFO=NULL)) %>%
  #   select(order(colnames(.)))
  
DF.scenarios <- data.frame(rbind(DF.s1,DF.s2,DF.s3)) 
###########################################################################
# 
# NEW SECTION
# Once all scenarios are built, create all of the eurostat matrices.
#
###########################################################################
DF.eurostat <- data.frame(DF.scenarios) %>% 
  mutate(    Y.colsum = elementproduct_byname(TFO, f.split),
             Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
             y = rowsums_byname(Y),
          A.mat = lapply(X=scenario, function(X) A.mat))
##
## WORKING CODE STOPS HERE.
##
################################################################################
###
### MKH TODO list #3: the dang Z matrix!
###
DF.eurostat  %>% 
  mutate(
          Z = lapply(X=scenario, function(X) { Mfg.etas  %>%
              matrix(rbind(rep(.,Prod.n)),nrow=Prod.n,ncol=Ind.n,byrow=T)  %>% 
              setrownames_byname(product.names ) %>%
              setrowtype("Products") %>%
              setcolnames_byname(industry.names) %>%
              setcoltype("Industries")  %>%
              elementquotient_byname(A.mat,.)} ) )
DF.eurostat  %>% 
  mutate( D = transpose_byname(identize_byname(Z)),
          A = matrixproduct_byname(Z,D),
          q = matrixproduct_byname(invert_byname(Iminus_byname(A)),y),
          V = matrixproduct_byname(D,hatize_byname(q)),
          g = rowsums_byname(V),
          U = matrixproduct_byname(Z,hatize_byname(g))   )

###############################################################################
#
# NEW SECTION: Run the results
# 
###############################################################################
## MKH TODO list #2 (continued) 
## the following code works, but is there a better way to do it?
##
DF.eurostat %>%
  mutate( IO.phys = lapply(X=scenario, function(X) {
                      matrix(unlist(cbind(U,Y)),
                             nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ) %>%
                        setcolnames_byname(c(industry.names,fin.names))  %>%
                        setrownames_byname(product.names) %>%
                        setcoltype("Industries") %>%
                        setrowtype("Products")}),

                    IO.phys.sumall = sumall_byname(IO.phys),

                    TST.phys = lapply(X=scenario, function(X) { ## should be same as sumall
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["TST"] )}),

                    alpha.phys = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["alpha"] )}),

                    F.phys = lapply(X=scenario, function(X) {
                      unlist(calc.IO.metrics(matrix(unlist(IO.phys),
                              nrow=Prod.n,ncol=(Ind.n+Fin.n),byrow=T ))["F"] )}),

        IO.curr = lapply(X=scenario, function(X) {
                      elementproduct_byname(prices.mat,IO.phys) }),
 ### MKH Bonus question - how do I do this ----->   %>% sort_rows_cols(.,colorder=(c(industry.names,fin.names))) })  ) ##########
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

#########################################################################################################
# End 
#########################################################################################################





