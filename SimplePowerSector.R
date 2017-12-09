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
#
# Make individual data.frames of vector or matrix input factors that are then cbinded to scalar inputs.
# At the end of the DF.scenario.factors build, there is a cbind statement 
# to put these in.
# 
# 
# MKH TODO List: Can you show me how to:
#     1. DONE! Make the A.mat (and other matrices) into a data.frame that
#        uses gather/collapse like the Mfg.etas vector? I think I only
#        have this figured out for vectors. DONE!
#     2. "Cbind" in byname world. We thought that the sum_byname would work
#        like that, but it didn't with my data objects. I have two places
#        where I need to do this: 
#            a. DONE! to create the IO.phys = sum_byname(U,Y)   WORKS NOW! DONE
#            b. (Still needed ) to make the prices matrix that is element by element
#                multiplied to IO.phys to get the IO.curr matrix.
#     3. DONE! Create the Z matrix. DONE!
#     4. (still needed, I think, I haven't looked at this since all of the changes)
#       How to mutate TFO and have it work correctly in elementproduct with f.split 
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
# Mfg.etas <- data.frame(I1 = 1,I2=1, I3=1/3, I4=0.40, I5=0.40,I6=0.50) %>%
#   gather(key = col.name, value = value, I1,I2,I3,I4,I5,I6) %>% 
#   mutate( matrix.name = "Mfg.etas",
#           col.type="Industries",row.name="Products",row.type="Products") %>% 
#   collapse_to_matrices(matnames = "matrix.name", values = "value",
#                        rownames = "row.name", colnames = "col.name",
#                        rowtypes = "row.type", coltypes = "col.type") %>% 
#   rename(Mfg.etas = value)

# Failed attempt #1
# Mfg.etas.mat <- data.frame(matrix(rbind(Mfg.etas,Mfg.etas,Mfg.etas,
#                                         Mfg.etas,Mfg.etas,Mfg.etas) %>%
#   setrownames_byname(product.names) %>% 
#   setrowtype("Products") %>% 
#   setcolnames_byname(industry.names) %>% 
#   setcoltype("Industries") ) )

Mfg.etas.mat <- matrix(rep(c(1,1,1/3,.4,.4,.5),Prod.n),
                      nrow = Prod.n, ncol = Ind.n, byrow = TRUE) %>%
                      setrownames_byname(product.names) %>%
                      setrowtype("Products") %>%
                      setcolnames_byname(industry.names) %>%
                      setcoltype("Industries")

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
                nrow = Prod.n, ncol = Ind.n, byrow = TRUE) %>%
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

#
# BRH to MKH
# 1. I want to be able to just cbind this matrix as a data element so that 
# I can use it in linear algebra functions, the current A.mat approach 
# seems to break down along the way.
#
# 2. A.mat.2 doesn't create the A.mat matrix when I run the code.
#
#
A.mat.2 <- data.frame(value = c(1,1,1,1)) %>% 
  mutate(
    rownames = c("P1", "P1", "P2", "P2"),
    colnames = c("I3", "I4", "I5", "I6"),
    mat.name = rep.int("A.mat", 4),
    rowtypes = rep.int("Products", 4),
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
Res.prices <- data.frame(I1 = Convert.prices(55,"MT",curr.scale),
                         I2 = Convert.prices(3,"MMBTU",curr.scale)) %>% 
  gather(key = col.name, value = value, I1, I2) %>% 
  mutate( matrix.name = "Res.prices",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Res.prices = value)

Mfg.prices <- data.frame(I3=1,I4=1,I5=1,I6=1) %>%     ### These are just placeholders, no wholesale trade yet.
  gather(key = col.name, value = value, I3, I4, I5, I6) %>% 
  mutate( matrix.name = "Fin.prices",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name",
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(Mfg.prices = value)

Fin.prices <- data.frame(F1 = Convert.prices(.10,"kWh",curr.scale),
                         F2 = Convert.prices(.10,"kWh",curr.scale)) %>% 
  gather(key = col.name, value = value, F1, F2) %>% 
  mutate( matrix.name = "Fin.prices",
          col.type="Industries",row.name="Products",row.type="Products") %>% 
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

# MKH TODO list #2 project: How do I need to create prices.mat so that I 
# can matrixproduct_byname(IO.phys,prices.mat)
#

U.mat.pattern <- matrix(rep(0,Prod.n * (Ind.n +Fin.n)),
                nrow = Prod.n, ncol = (Ind.n + Fin.n), byrow = TRUE) %>% 
  setrownames_byname(product.names) %>%
  setrowtype("Products") %>%
  setcolnames_byname(c(industry.names,fin.names)) %>%
  setcoltype("Industries")

# Nope.
# prices.mat <- sum_byname(U.mat.pattern,Res.prices)
# prices.mat


##########################################################################################################
#
# BUILD OUT THE INITIAL INPUT FACTOR VARIABLES FOR "SCENARIO 0"
#
#########################################################################################

DF.scenario.factors <- data.frame(scenario = 0) %>%
  mutate( TFO = 200,
          f.product.coeffs = lapply(X=scenario, function(X) f.product.coeffs),
          Mfg.etas.mat = lapply(X=scenario, function(X) Mfg.etas.mat),
          A.mat = lapply(X=scenario, function(X) A.mat))%>%
          cbind(.,Res.prices,Mfg.prices,Fin.prices,f.split) %>%
          select(order(colnames(.)))



###############################################################################
#
# NEW SECTION: BUILD OUT SCENARIOS
# Three working prototype scenarios are built -- brh
#
###############################################################################
#
# Change percent of electricity used by final output sectors
# (although BRH thinks they should be called industries because this is a model built
#  at the scale of one sector - several industries within it. The final output
#  good goes to another industry - not to the final demand sector.)
#
    DF.IO1 <- data.frame(F1 = seq(0, 1, by = 0.1)) %>%
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
DF.IO2a <- data.frame(P1 = c(Convert.prices(55,"MT",curr.scale),
                             Convert.prices(110,"MT",curr.scale),
                             Convert.prices(220,"MT",curr.scale))) %>%
  mutate(
    P2 = 3,
    sweep.val = P1 # This becomes the metadata column
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

DF.IO2b <- data.frame(P2 = c(Convert.prices(3,"MMBTU",curr.scale),
                             Convert.prices(6,"MMBTU",curr.scale),
                             Convert.prices(9,"MMBTU",curr.scale))) %>%
  mutate(
    P1 = 55,
    sweep.val = P2 # This becomes the metadata column
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
# BRH TODO: this doesn't seem to be working now with the new way BRH created the .mat
#
  # DF.IO3 <- data.frame(Mfg.etas.mat) %>%
  #   mutate(sweep.val = 2,
  #     Mfg.etas.mat = elementproduct_byname(sweep.val,Mfg.etas.mat), 
  #     scenario=3,sweep.factor = "Mfg.etas.mat") %>%
  #   cbind(.,mutate(DF.scenario.factors,scenario=NULL,Mfg.etas.mat=NULL)) %>%
  #   select(order(colnames(.)))
  
  ###
  ### MKH TODO List #4: Why doesn't this one work???
  ###
  # DF.IO4 <- data.frame( TFO = seq(10, 100, by = 10)) %>%
  #   mutate(
  #     scenario=2,
  #     sweep.factor = "TFO",
  #     sweep.val = TFO 
  #   ) %>%
  #   cbind(.,mutate(DF.scenario.factors,scenario=NULL,TFO=NULL)) %>%
  #   select(order(colnames(.)))
  
DF.scenarios <- data.frame(rbind(DF.IO1,DF.IO2a,DF.IO2b))
###########################################################################
# 
# NEW SECTION
# Once all scenarios are built, create all of the eurostat matrices.
#
###########################################################################
DF.eurostat <- data.frame(DF.scenarios) %>% 
  mutate(    Y.colsum = elementproduct_byname(TFO, f.split),
             Y = matrixproduct_byname(f.product.coeffs,hatize_byname(Y.colsum)),
             y = rowsums_byname(Y)) 
#
# BRH to MKH - This is what I am trying to get to...only linear algebra in eurostat code
#
DF.eurostat <- data.frame(DF.eurostat) %>% 
  mutate( Z =  elementproduct_byname(Mfg.etas.mat,A.mat),
          D = transpose_byname(identize_byname(Z)),
          A = matrixproduct_byname(Z,D),
          q = matrixproduct_byname(invert_byname(Iminus_byname(A)),y),
          V = matrixproduct_byname(D,hatize_byname(q)),
          g = rowsums_byname(V),
          U = matrixproduct_byname(Z,hatize_byname(g))   )

###############################################################################
#
# NEW SECTION: Run the results
# 
#################################################################################
DF.eurostat <- data.frame(DF.eurostat) %>% 
  mutate(IO.phys = sum_byname(U,Y) %>% 
           sort_rows_cols(.,colorder=(c(industry.names,fin.names))),
         IO.phys.sumall = sumall_byname(IO.phys))

###############################################################################
## MKH TODO list #2b (continued) 
## the following code doesn't work yet because prices.mat isn't built correctly
## yet.
##
##############################################################################
DF.eurostat <- data.frame(DF.eurostat) %>% 
  mutate(IO.curr = elementproduct_byname(prices.mat,IO.phys),
         IO.curr.sumall = sumall_byname(IO.curr))

######################################################################################
###
### BRH believes the code works up to here! Yeah!
###
###
### BRH TODO: 
###   1. figure out how to call the calc IO metrics function within DF
###
#######################################################################################
DF.eurostat <- data.frame(DF.eurostat) %>% mutate(TST.phys = calc.TST(IO.phys),
                                                  alpha.phys = calc.alpha(IO.phys),
                                                  F.phys = calc.F(IO.phys)) 

#########################################################################################################
# End 
#########################################################################################################





