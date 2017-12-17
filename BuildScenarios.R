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


Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)
nodes.n   <- Res.n + Mfg.n + Fin.n

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

#
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

#
# Base values for manufacturing etas and prices
# 
mfg.etas.base_list <- list(I1 = 1, I2 = 1, I3 = 1/3, I4 = 0.4, I5 = 0.4, I6 = 0.5)
mfg.etas.base_matrix <- do.call(create_mfg.etas_matrix, mfg.etas.base_list)

prices.base_list <- list(P1 = Convert.prices(55, "MT", curr.scale), 
                         P2 = Convert.prices(3,"MMBTU",curr.scale),
                         F1 = Convert.prices(0.10,"kWh",curr.scale),
                         F2 = Convert.prices(0.15,"kWh",curr.scale))
prices.base_matrix <- do.call(create_price_matrix, prices.base_list)

#
# Establish sweep values for factors
# 
tfos <- c(100, 200)
f1s <- c(0.4, 0.6)
fpcs <- c(0.1, 0.6)
gammas <- c(1, 2)
mus <- c(1, 2)

# 
# Work on f.split matrices
# Each f.split matrix is a function of the value of f1.
# 
F.split_matrices <- data.frame(f1 = f1s) %>% 
  mutate(
    F.split = create_F.split_matrix(f1)
  )

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
  fpc31 = fpcs, fpc32 = fpcs,
  fpc41 = fpcs, fpc42 = fpcs,
  fpc51 = fpcs, fpc52 = fpcs
)

F.product.coeffs_matrices <- expand.grid(fpc_factors) %>% 
  mutate(
    F.product.coeffs = create_F.product.coeffs_matrix(fpc31 = fpc31, fpc32 = fpc32,
                                                      fpc41 = fpc41, fpc42 = fpc42,
                                                      fpc51 = fpc51, fpc52 = fpc52)
  )
  
#
# Work on manufacturing efficiencies
# Each mfg.etas matrix is a function of gamma1 ... gamma6,
# where gamma is a multiplier on a base value for manufacturing efficiency.
# 
gamma_factors <- names(mfg.etas.base_list) %>% 
  lapply(., function(gammaname){gammas}) %>% 
  set_names(paste0("gamma", 1:Ind.n))

Mfg.etas_matrices <- 
  expand.grid(gamma_factors) %>% 
  mutate(
    gammas = create_mfg.etas_matrix(I1 = gamma1, I2 = gamma2, I3 = gamma3, I4 = gamma4, I5 = gamma5, I6 = gamma6),
    mfg.etas = elementproduct_byname(gammas, mfg.etas.base_matrix),
    gammas = NULL
  )

#
# Work on prices 
# Each price matrix is a function of mu1 ... mu4,
# where mu is a multiplier on a base value for price.
# 
mu_factors <- names(prices.base_list) %>% 
  lapply(., function(muname){mus}) %>%
  set_names(paste0("mu", 1:4))

Prices_matrices <- 
  expand.grid(mu_factors) %>% 
  mutate(
    mus = create_price_matrix(P1 = mu1, P2 = mu2, F1 = mu3, F2 = mu4), 
    prices = elementproduct_byname(mus, prices.base_matrix) %>% sort_rows_cols(margin = 2, colorder = c(industry.names, fin.names)),
    mus = NULL
  )

#
# Create a named list of all factors and their possible sweep values
# 
factors_list <- c(tfo = list(tfos), f1 = list(f1s), fpc_factors, gamma_factors, mu_factors)

# 
# From factors_list, create a data frame of scenarios
#
DF.scenarios.matrices <- 
  # Create the grid of all unique combinations of factors
  expand.grid(factors_list) %>% 
  # Check for valid values of pfc61 and fpc62
  # by calculating pfc61 and fpc62 ...
  mutate(
    fpc61 = 1 - fpc31 - fpc41 - fpc51,
    fpc62 = 1 - fpc32 - fpc42 - fpc52
  ) %>% 
  # ... then requiring that both fpc61 and fpc62 are non-negative.
  filter(fpc61 >= 0 & fpc62 >= 0) %>% 
  mutate(
    fpc61 = NULL,
    fpc62 = NULL
  ) %>% 
  # Join all matrices by the factors that make them unique,
  # thereby providing a data frame that contains all factors
  # and associated matrices in a single data frame.
  # Each row of this data frame is a scenario to be evaluated.
  left_join(F.split_matrices, by = "f1") %>% 
  left_join(F.product.coeffs_matrices, by = c("fpc31", "fpc32", "fpc41", "fpc42", "fpc51", "fpc52")) %>% 
  left_join(Mfg.etas_matrices, by = c("gamma1", "gamma2", "gamma3", "gamma4", "gamma5", "gamma6")) %>% 
  left_join(Prices_matrices, by = c("mu1", "mu2", "mu3", "mu4")) %>% View

save(DF.scenario.matrices,file="DF.scenario.matrices")