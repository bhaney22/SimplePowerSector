#
# A script to build scenarios
# 

rm(list=ls())
source("Calc_IO_metrics.R")
source("Conversions.R")

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

#
# Some base values
# 
mfg.etas.base <- list(I1 = 1, I2 = 1, I3 = 1/3, I4 = 0.4, I5 = 0.4, I6 = 0.5)

prices.base <- list(PP1 = Convert.prices(55, "MT", curr.scale),
                    PP2 = Convert.prices(3,"MMBTU",curr.scale),
                    PF1 = Convert.prices(0.10,"kWh",curr.scale),
                    PF2 = Convert.prices(0.10,"kWh",curr.scale))

# Values of TFO over which we want to sweep
tfo_list <- list(100, 200)

# Values of F1 over which we want to sweep
f1_list <- list(0.2, 0.5)

# Values of product coefficients over which we want to sweep
fpc_list <- list(0.25, 0.4)

# Values of gamma over which we want to sweep
gamma_list <- list(0.5, 1, 2)
gamma.names <- paste0("gamma.", names(mfg.etas.base))

# Values of mu over which we want to sweep
mu_list <- list(0.5, 1, 2)
mu.names <- paste0("mu.", names(prices.base))


expand.grid(mu = mu_list, base.price = names(prices.base))
prices_list <- lapply(prices.base, function(p){
  return(mu_list)
})


DF.scenario.factors <- expand.grid(TFO = TFO_list, F1 = F1_list, stringsAsFactors = FALSE) %>% 
  mutate(
    F1 = as.numeric(F1),
    F2 = 1-F1
  )

View(DF.scenario.factors)