#
# A script to build scenarios
# 
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
source("Conversions.R")
source("helpers_scenarios.R")

##################################################################################################
# Step 0: Do nothing. The following structural parameters will rarely change.
##################################################################################################
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)
nodes.n   <- Res.n + Mfg.n + Fin.n
Mfg.first	<- Res.n + 1		# node number of first Mfg
Fin.first	<- Res.n + Mfg.n + 1	# node number of first Fin

Res.nodes	<- seq(1,Res.n)
Mfg.nodes	<- seq(Mfg.first,length=Mfg.n)
Fin.nodes	<- seq(Fin.first,length=Fin.n)

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

#
# Step 1: Set base values for manufacturing etas and prices
# 
Eta.1 = 1     #Coal Extraction industry 
Eta.2 = 1     #NG Extraction industry
Eta.3 = .25   #Coal power plant 1
Eta.4 = .3    #Coal power plant 2
Eta.5 = .4    #NG power plant 1
Eta.6 = .5    #NG power plant 2
Res.1.price = Convert.prices(55, "MT", curr.scale)  #price of coal per MT -> MW
Res.2.price = Convert.prices(3,"MMBTU",curr.scale)  #price of NG per MMBTU -> MW
Fin.1.price = Convert.prices(.10,"kWh",curr.scale)  #price of res.elec per kWh -> MW
Fin.2.price = Convert.prices(.15,"kWh",curr.scale) #price of com.elec per kWh -> MW

#
# Step 2: Establish sweep values for factors
# 
# 
# Problem that needs to be fixed: list of sweep factors needs to have
# two values even if only want to sweep 1. Makes matrix too big.
#
tfo <- c(100)
f1 <- c(0.4)              # Split of total output between Final Output sectors
fpcs <- c(0,.25, .5, .75, 1)      # Plant shares in output
# gammas <- c(1,2)               # Eta multipliers
mus <- c(1, 1.05,.95,1.10,.90,1.20,.80, 1.25, .75)                  # Price multipliers

#
# Step 3: Set the A.mat coefficients
# The A.mat are the coefficients pertaining to the percent of each product that
# goes into each Industry. In SPS, P1 (Coal) is the one an only input into
# the two coal power plants (I3,4) and P2 (NG) is the one and only input
# into the gas power plants (I5,6).
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
####################################################################################################
# The rest of the script builds the scenarios based on the inputs above.
####################################################################################################
mfg.etas.base_list <- list(I1 = Eta.1, I2 = Eta.2, I3 = Eta.3, I4 = Eta.4, I5 = Eta.5, I6 = Eta.6)
mfg.etas.base_matrix <- do.call(create_mfg.etas_matrix, mfg.etas.base_list)

prices.base_list <- list(P1 = Res.1.price, 
                         P2 = Res.2.price,
                         F1 = Fin.1.price,
                         F2 = Fin.2.price)
prices.base_matrix <- do.call(create_price_matrix, prices.base_list)


# 
# Work on f.split matrices
# Each f.split matrix is a function of the value of f1.
# 
# F.split_matrices <- data.frame(f1 = f1s) %>%
#   mutate(
#     F.split = create_F.split_matrix(f1)
# ) 
F.split = create_F.split_matrix(f1)

# 
# Work on f.product.coeffs
# Each f.product.coeffs matrix is a function of 
# fpc31, fpc32, fpc41, fpc42, fpc51, and fpc52.
# fpc61 and fpc62 are calculated from the input factors,
# and we check (later) whether any of fpc6x values
# become negative.
# If so, we delete them from the scenarios under consideration.
# 
fpc_factors <- list(
  fpc13 = fpcs, fpc23 = fpcs,
  fpc14 = fpcs, fpc24 = fpcs,
  fpc15 = fpcs, fpc25 = fpcs
) 
F.product.coeffs_matrices <- expand.grid(fpc_factors) %>% 
  # Check for valid values of fpc61 and fpc62
  # by calculating fpc61 and fpc62 ...
  mutate(
    fpc16 = 1 - fpc13 - fpc14 - fpc15,
    fpc26 = 1 - fpc23 - fpc24 - fpc25
  ) %>%   
  # ... then requiring that both fpc61 and fpc62 are non-negative.
  filter(fpc16 >= 0 & fpc26 >= 0) %>% 
  mutate(F.product.coeffs = 
           create_F.product.coeffs_matrix( fpc13 = fpc13, fpc23 = fpc23,
                                           fpc14 = fpc14, fpc24 = fpc24,
                                           fpc15 = fpc15, fpc25 = fpc25)
  ) %>%  
  mutate( Fin.1.I.3.share = fpc13,
          Fin.1.I.4.share = fpc14,
          Fin.1.I.5.share = fpc15,
          Fin.1.I.6.share = fpc16,
          Fin.2.I.3.share = fpc23,
          Fin.2.I.4.share = fpc24,
          Fin.2.I.5.share = fpc25,
          Fin.2.I.6.share = fpc26) %>%
  mutate(Resources=ifelse(Fin.1.I.3.share==0 & Fin.2.I.3.share==0 & 
                          Fin.1.I.4.share==0 & Fin.2.I.4.share==0,"NG Only",
                   ifelse(Fin.1.I.5.share==0 & Fin.2.I.5.share==0 & 
                          Fin.1.I.6.share==0 & Fin.2.I.6.share==0,"Coal Only",
                        "Coal & NG")))   ##  %>%
  ## filter(Resources != "Coal & NG") ## DO NOT DROP intermediate combinations


  tally(group_by(F.product.coeffs_matrices,Resources))
  
#
# Work on manufacturing efficiencies
# Each mfg.etas matrix is a function of gamma1 ... gamma6,
# where gamma is a multiplier on a base value for manufacturing efficiency.
# 
# gamma_factors <- names(mfg.etas.base_list) %>% 
#   lapply(., function(gammaname){gammas}) %>% 
#   set_names(paste0("gamma", 1:Ind.n))
# 
# Mfg.etas_matrices <- 
#   expand.grid(gamma_factors) %>% 
#   mutate(
#     gammas = create_mfg.etas_matrix(I1 = gamma1, I2 = gamma2, I3 = gamma3, I4 = gamma4, I5 = gamma5, I6 = gamma6),
#     mfg.etas = elementproduct_byname(gammas, mfg.etas.base_matrix),
#     gammas = NULL
#   )

#
# Work on prices 
# Each price matrix is a function of mu1 ... mu4,
# where mu is a multiplier on a base value for price.
# 
mu_factors <- names(prices.base_list) %>% 
  lapply(., function(muname){mus}) %>%
  set_names(paste0("mu", 1:4))

# Don't expand grid over the output prices (mu3 and 4)
mu_factors$mu3=1
mu_factors$mu4=1

Prices_matrices <- 
  expand.grid(mu_factors) %>% 
  mutate(
    mus = create_price_matrix(P1 = mu1, P2 = mu2, F1 = mu3, F2 = mu4), 
    prices = elementproduct_byname(mus, prices.base_matrix) %>% 
      sort_rows_cols(margin = 2, colorder = c(industry.names, fin.names)),
    mus = NULL
  )

#
# Create a named list of all factors and their possible sweep values
# 
# factors_list <- c(tfo = list(tfos), 
#                   f1 = list(f1s1), 
#                   fpc_factors, 
#                   gamma_factors, 
#                   mu_factors)
factors_list <- c(fpc_factors, 
                  mu_factors)

# 
# From factors_list, create a data frame of scenarios
#
DF.scenario.matrices <- 
  # Create the grid of all unique combinations of factors
  expand.grid(factors_list) %>% 
  # As above in creating the F.product.coeffs do this again:
  # Check for valid values of fpc61 and fpc62
  # by calculating fpc61 and fpc62 ...
  mutate(
    fpc16 = 1 - fpc13 - fpc14 - fpc15,
    fpc26 = 1 - fpc23 - fpc24 - fpc25
  ) %>%   
  # ... then requiring that both fpc61 and fpc62 are non-negative.
  filter(fpc16 >= 0 & fpc26 >= 0) %>% 
  left_join(F.product.coeffs_matrices, 
            by = c("fpc13", "fpc14", "fpc15","fpc16",
                   "fpc23", "fpc24", "fpc25","fpc26")) %>% 
  # filter(!is.na(Resources)) %>%   # DO NOT Keep only the scenarios
                                  # with Resources we kept above
  # Join the rest of  matrices by the factors that make them unique,
  # thereby providing a data frame that contains all factors
  # and associated matrices in a single data frame.
  # Each row of this data frame is a scenario to be evaluated.
  # left_join(F.split_matrices, by = "f1") %>% 
  # left_join(Mfg.etas_matrices, by = c("gamma1", "gamma2", "gamma3", "gamma4", "gamma5", "gamma6")) %>% 
  left_join(Prices_matrices, by = c("mu1", "mu2", "mu3", "mu4") ) %>% 
# Filter out the extra gammas/mus that were needed above to make the grid work
# filter(f1 == 0.4) %>%
#  rename(Fin.1.Mkt.share = f1,
  rename(Prices.mat = prices) %>%
  mutate(TFO = tfo,
         Fin.1.Mkt.share = f1,
         F.split = lapply(X = TFO, function(X) {F.split}),
         A.mat = lapply(X = TFO, function(X) {A.mat}),
         Mfg.etas.mat  = lapply(X = TFO, function(X) {mfg.etas.base_matrix}),
         fpc13 = NULL,
         fpc14 = NULL,
         fpc15 = NULL,
         fpc16 = NULL,
         fpc23 = NULL,
         fpc24 = NULL,
         fpc25 = NULL,
         fpc26 = NULL,
        Eta.1=sapply(X=Mfg.etas.mat, function(X) X[1,1]),  ## Get actual eta and price factors
        Eta.2=sapply(X=Mfg.etas.mat, function(X) X[1,2]),
        Eta.3=sapply(X=Mfg.etas.mat, function(X) X[1,3]),
        Eta.4=sapply(X=Mfg.etas.mat, function(X) X[1,4]),
        Eta.5=sapply(X=Mfg.etas.mat, function(X) X[1,5]),
        Eta.6=sapply(X=Mfg.etas.mat, function(X) X[1,6]),
        Res.1.price=sapply(X=Prices.mat, function(X) X[1,1]),
        Res.2.price=sapply(X=Prices.mat, function(X) X[2,1]),
        Fin.1.price=sapply(X=Prices.mat, function(X) X[Mfg.first,Fin.first]),
        Fin.2.price=sapply(X=Prices.mat, function(X) X[Mfg.first,Fin.first+1]))   

save(DF.scenario.matrices,file="DF.scenario.matrices.Rda")

