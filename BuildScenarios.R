#
# A script to build scenarios
# 

rm(list=ls())
source("Calc_IO_metrics.R")
source("Conversions.R")

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
# Base values
# 
mfg.etas.base <- list(I1 = 1, I2 = 1, I3 = 1/3, I4 = 0.4, I5 = 0.4, I6 = 0.5)

prices.base <- list(PP1 = Convert.prices(55, "MT", curr.scale),
                    PP2 = Convert.prices(3,"MMBTU",curr.scale),
                    PF1 = Convert.prices(0.10,"kWh",curr.scale),
                    PF2 = Convert.prices(0.10,"kWh",curr.scale))

#
# Sweep values
# 
tfos <- c(100, 200)
f1s <- c(0.2, 0.5)
fpcs <- c(0.25, 0.4)
gammas <- c(1, 2)
mus <- c(1, 2)

# Start the list that will be expanded into all scenarios.
# First item is TFOs
running_list_for_expand.grid <- list(tfo = tfos)

# Next item is f1s
running_list_for_expand.grid$f1 <- f1s

# Work on fpcs
fpc.names <- paste0("fpc_", c("31", "32", "41", "42", "51", "52"))
fpc_list <- lapply(fpc.names, function(fpcn){fpcs}) %>% set_names(fpc.names)

# gammas
gamma.names <- paste0("gamma_", names(mfg.etas.base))
gamma_list <- lapply(gamma.names, function(gn){gammas}) %>% set_names(gamma.names)

# mus
mu.names <- paste0("mu_", names(prices.base))
mu_list <- lapply(mu.names, function(mn){mus}) %>% set_names(mu.names)

# Add fpcs, gammas, and mus to our list.
for (l in list(fpc_list, gamma_list, mu_list)){
  # Each of these lists contains sub-vectors, 
  # each of which needs to be included on its own
  # in the running_list.  
  for (i in 1:length(l)){
    running_list_for_expand.grid[[names(l)[[i]]]] <- l[[i]]
  }
}

DF.scenario.factors <- expand.grid(running_list_for_expand.grid) %>%
  mutate(
    # Calculate auxiliary varlues
    f2 = 1 - f1,
    fpc_61 = 1 - fpc_31 - fpc_41 - fpc_51, 
    fpc_62 = 1 - fpc_32 - fpc_42 - fpc_52
  ) %>% 
  # None of the scenarios with negative values for fpc_61 or fpc_62 are valid scenarios.
  filter(fpc_61 >= 0 & fpc_62 >= 0) %>% 
  # Promote the row names (which are simply integers) to a column to provide scenario identifiers
  rownames_to_column("scenario.n") %>% 
  # reorder columns
  select(scenario.n, tfo, f1, f2, starts_with("fpc"), everything())

View(DF.scenario.factors)
