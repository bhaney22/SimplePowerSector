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
# Base values for manufacturing etas and prices
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


# Start the list that will be expand.grid'ed into all scenarios.
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
# Put this into a function later!
f.product.coeffs_DF <- list(
  fpc31 = fpcs, fpc32 = fpcs,
  fpc41 = fpcs, fpc42 = fpcs,
  fpc51 = fpcs, fpc52 = fpcs
) %>% 
  expand.grid() %>% 
  # At this point, we have a data frame whose rows
  # contain all possible combinations of fpcs for 
  # entries in rows 3, 4, and 5 of the f.product.coeffs matrix.
  # Calculate the values for the 6th row,
  # even if some of them might be negative.
  mutate(
    fpc61 = 1 - fpc31 - fpc41 - fpc51,
    fpc62 = 1 - fpc32 - fpc42 - fpc52
  ) %>% 
  # Eliminate any scenarios that 
  # contain negative values
  # in the 6th row.
  filter(fpc61 >= 0 & fpc62 >= 0) %>% 
  mutate(
    # Add zeroes for the first and second rows
    fpc11 = 0, fpc12 = 0,
    fpc21 = 0, fpc22 = 0
  ) %>% 
  # Keep track of which scenario we're evaluating
  rownames_to_column("scenario") %>% 
  mutate(
    scenario = as.numeric(scenario)
  ) %>% 
  # Get into tidy structure
  gather(key = "var", value = "val", fpc11, fpc12,
         fpc21, fpc22,
         fpc31, fpc32,
         fpc41, fpc42,
         fpc51, fpc52,
         fpc61, fpc62) %>% 
  mutate(
    # Add metadata for collapsing to matrices
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
  # Make one matrix for each scenario (each row of the data frame)
  group_by(scenario) %>% 
  # Use the metadata to collapse to matrices
  collapse_to_matrices(values = "val", matnames = "matnames", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  rename(f.product.coeffs = val) %>%
  mutate(scenario = NULL)

running_list_for_expand.grid$f.product.coeffs <- f.product.coeffs_DF$f.product.coeffs

# Work on gammas
# Put this into a function later
gammas_DF <- paste0("gamma_", names(mfg.etas.base)) %>% 
  lapply(., function(gn){gammas}) %>% 
  set_names(paste0("gamma_", names(mfg.etas.base))) %>% 
  # At this point, we have a named list of multipliers on manufacturing eta values.
  # Convert to a data frame containing all combinations.
  expand.grid() %>% 
  # Keep track of scenarios: one per row.
  rownames_to_column(var = "scenario") %>% 
  mutate(scenario = as.numeric(scenario)) %>% 
  # Create a tidy data frame in preparation for creating matrices
  gather(key = "colnames", value = "gammas", -scenario) %>% 
  arrange(scenario) %>% 
  # Add metadata in preparation for creating matrices
  mutate(
    matnames = "gammas",
    rownames = "row", 
    rowtypes = "row", 
    coltypes = "Industries"
  ) %>% 
  group_by(scenario) %>% 
  # Create the gamma matrices
  collapse_to_matrices(matnames = "matnames", values = "gammas", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  mutate(scenario = NULL)

running_list_for_expand.grid$gammas <- gammas_DF$gammas

# Work on mus.
# Move into a function later.
mus_DF <- paste0("mu_", names(prices.base)) %>% 
  lapply(., function(mn){mus}) %>% 
  set_names(paste0("mu_", names(prices.base))) %>% 
  # At this point, we have a named list of multipliers on prices.
  # Convert to a data frame containing all combinations.
  expand.grid() %>% 
  # Keep track of scenarios: one per row.
  rownames_to_column(var = "scenario") %>% 
  mutate(scenario = as.numeric(scenario)) %>% 
  # Create a tidy data frame in preparation for creating matrices
  gather(key = "colnames", value = "mus", -scenario) %>% 
  arrange(scenario) %>% 
  # Add metadata in preparation for creating matrices
  mutate(
    matnames = "mus",
    rownames = "row", 
    rowtypes = "row", 
    coltypes = "Products"
  ) %>% 
  group_by(scenario) %>% 
  # Create the mu matrices
  collapse_to_matrices(matnames = "matnames", values = "mus", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  mutate(scenario = NULL)

running_list_for_expand.grid$mus <- mus_DF$mus

# 
# Create the data frame of scenarios
#
DF.scenario.factors <- expand.grid(running_list_for_expand.grid)

View(DF.scenario.factors)
