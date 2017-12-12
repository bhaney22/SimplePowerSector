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

# Start the list that will be expanded using TFOs
running_list_for_expand.grid <- list(tfo = tfos)

# Add f1 values to our list
running_list_for_expand.grid$f1 <- f1s

# Add fpc values to our list
fpc.names <- paste0("fpc_", c("31", "32", "41", "42", "51", "52"))
fpc_list <- lapply(fpc.names, function(fpcn){fpcs}) %>% set_names(fpc.names)
# Add to our list
for(i in 1:length(fpc_list)){
  running_list_for_expand.grid[[names(fpc_list)[[i]]]] <- fpc_list[[i]]
}

# Add gamma values to our list
gamma.names <- paste0("gamma_", names(mfg.etas.base))
gamma_list <- lapply(gamma.names, function(gn){gammas}) %>% set_names(gamma.names)
# Add to our list
for(i in 1:length(gamma_list)){
  running_list_for_expand.grid[[names(gamma_list)[[i]]]] <- gamma_list[[i]]
}

# Add mu values to our list
mu.names <- paste0("mu_", names(prices.base))
mu_list <- lapply(mu.names, function(mn){mus}) %>% set_names(mu.names)
# Add to our list
for(i in 1:length(mu_list)){
  running_list_for_expand.grid[[names(mu_list)[[i]]]] <- mu_list[[i]]
}

DF.scenario.factors <- expand.grid(running_list_for_expand.grid)

View(DF.scenario.factors)
