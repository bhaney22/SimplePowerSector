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

# Next item is f.split matrices
f.split_list <- lapply(f1s, function(f1){
  matrix(c(f1, 1-f1),
         nrow = 1, ncol = 2, byrow = TRUE) %>% 
    setrownames_byname("row") %>% setcolnames_byname(c("F1", "F2")) %>% 
    setrowtype("row") %>% setcoltype("Sectors")
})
running_list_for_expand.grid$f.split <- f.split_list

# Work on f.product.coeffs
f.product.coeffs_DF <- list(
  fpc31 = fpcs, fpc32 = fpcs,
  fpc41 = fpcs, fpc42 = fpcs,
  fpc51 = fpcs, fpc52 = fpcs
) %>% 
  expand.grid() %>% 
  mutate(
    fpc61 = 1 - fpc31 - fpc41 - fpc51,
    fpc62 = 1 - fpc32 - fpc42 - fpc52
  ) %>% 
  filter(fpc61 >= 0 & fpc62 >= 0) %>% 
  mutate(
    fpc11 = 0, fpc12 = 0,
    fpc21 = 0, fpc22 = 0
  ) %>% 
  rownames_to_column("scenario") %>% 
  mutate(
    scenario = as.numeric(scenario)
  ) %>% 
  gather(key = "var", value = "val", fpc11, fpc12,
         fpc21, fpc22,
         fpc31, fpc32,
         fpc41, fpc42,
         fpc51, fpc52,
         fpc61, fpc62) %>% 
  mutate(
    rownames = case_when(
      startsWith(.data$var, "fpc1") ~ "P1",
      startsWith(.data$var, "fpc2") ~ "P2",
      startsWith(.data$var, "fpc3") ~ "P3",
      startsWith(.data$var, "fpc4") ~ "P4",
      startsWith(.data$var, "fpc5") ~ "P5",
      startsWith(.data$var, "fpc6") ~ "P6"
    ),
    colnames = case_when(
      endsWith(.data$var, "1") ~ "F1",
      endsWith(.data$var, "2") ~ "F2",
      TRUE ~ NA_character_
    ),
    rowtypes = "Products",
    coltypes = "Sectors", 
    matnames = "f.product.coeffs"
  ) %>% 
  group_by(scenario) %>% 
  collapse_to_matrices(values = "val", matnames = "matnames", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  rename(f.product.coeffs = val) %>%
  mutate(scenario = NULL)

running_list_for_expand.grid$f.product.coeffs <- f.product.coeffs_DF$f.product.coeffs

# Work on gammas






# gammas
# gamma.names <- paste0("gamma_", names(mfg.etas.base))
# gamma_list <- lapply(gamma.names, function(gn){gammas}) %>% set_names(gamma.names)

# mus
# mu.names <- paste0("mu_", names(prices.base))
# mu_list <- lapply(mu.names, function(mn){mus}) %>% set_names(mu.names)

# Add fpcs, gammas, and mus to our list.
# for (l in list(fpc_list, gamma_list, mu_list)){
#   # Each of these lists contains sub-vectors, 
#   # each of which needs to be included on its own
#   # in the running_list.  
#   for (i in 1:length(l)){
#     running_list_for_expand.grid[[names(l)[[i]]]] <- l[[i]]
#   }
# }

DF.scenario.factors <- expand.grid(running_list_for_expand.grid) #%>%
# mutate(
#   # Calculate auxiliary varlues
#   f2 = 1 - f1,
#   fpc_61 = 1 - fpc_31 - fpc_41 - fpc_51, 
#   fpc_62 = 1 - fpc_32 - fpc_42 - fpc_52
# ) %>% 
# # None of the scenarios with negative values for fpc_61 or fpc_62 are valid scenarios.
# filter(fpc_61 >= 0 & fpc_62 >= 0) %>% 
# # Promote the row names (which are simply integers) to a column to provide scenario identifiers
# rownames_to_column("scenario.n") %>% 
# # reorder columns
# select(scenario.n, tfo, f1, f2, starts_with("fpc"), everything()) 

# temp <- DF.scenario.factors %>% 
#   select(scenario.n, f1, f2) %>% 
#   gather(key = variable, value = value, f1, f2) %>% 
#   mutate(
#     rownames = "rn",
#     rowtypes = "Product",
#     coltypes = "Industry",
#     matnames = "f.split",
#     colnames = case_when(
#       .data$variable == "f1" ~ "I1",
#       .data$variable == "f2" ~ "I2",
#       TRUE ~ NA_character_
#     )
#   ) %>% 
#   group_by(scenario.n) %>% 
#   collapse_to_matrices(values = "value", matnames = "matnames", 
#                        rownames = "rownames", colnames = "colnames", 
#                        rowtypes = "rowtypes", coltypes = "coltypes")

# DF.scenario.matrices <- DF.scenario.factors %>% 
#   full_join(.,
#             select(., scenario.n, f1, f2)) %>% 

View(DF.scenario.factors)
