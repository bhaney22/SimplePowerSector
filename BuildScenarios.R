#
# A script to build scenarios
# 

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

#
# Some base values
# 
Mfg.etas.base <- list(I1 = 1, I2 = 1, I3 = 1/3, I4 = 0.4, I5 = 0.4, I6 = 0.5)

Prices.base <- list(PP1 = Convert.prices(55, "MT", curr.scale),
                    PP2 = Convert.prices(3,"MMBTU",curr.scale),
                    PF1 = Convert.prices(0.10,"kWh",curr.scale),
                    PF2 = Convert.prices(0.10,"kWh",curr.scale))

# Values of TFO over which we want to sweep
TFO_list <- list(100, 200)

# Values of F1 over which we want to sweep
F1_list <- list(0.2, 0.5)


DF.scenario.factors <- expand.grid(TFO = TFO_list, F1 = F1_list)

View(DF.scenario.factors)