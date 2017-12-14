#
# A script to build scenarios
# 

rm(list=ls())
source("Calc_IO_metrics.R")
source("Conversions.R")
source("helpers_scenarios.R")

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
mfg.etas.base <- data.frame(I1 = 1, I2 = 1, I3 = 1/3, I4 = 0.4, I5 = 0.4, I6 = 0.5) %>% as.matrix %>%
  setrownames_byname("P1") %>% 
  setrowtype("Products") %>% setcoltype("Industries")

prices.base_list <- list(P1 = Convert.prices(55, "MT", curr.scale), 
                         P2 = Convert.prices(3,"MMBTU",curr.scale),
                         F1 = Convert.prices(0.10,"kWh",curr.scale),
                         F2 = Convert.prices(0.10,"kWh",curr.scale))

prices.base_matrix <- create_price_matrix(P1 = prices.base_list[["P1"]], 
                                          P2 = prices.base_list[["P2"]],
                                          F1 = prices.base_list[["F1"]],
                                          F2 = prices.base_list[["F2"]])

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
    setrownames_byname("Product") %>% setcolnames_byname(c("F1", "F2")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
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
    coltypes = "Industries", 
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

# Work on manufacturing efficiencies
# Put this into a function later
mfg.etas_DF <- colnames(mfg.etas.base) %>% 
  lapply(., function(gn){gammas}) %>% 
  set_names(colnames(mfg.etas.base)) %>% 
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
    rownames = "P1", 
    rowtypes = "Products", 
    coltypes = "Industries"
  ) %>% 
  group_by(scenario) %>% 
  # Create the gamma matrices
  collapse_to_matrices(matnames = "matnames", values = "gammas", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  # From the gamma matrices, create the mfg.eta matrices
  mutate(
    etas_temp = elementproduct_byname(gammas, mfg.etas.base),
    gammas = NULL,
    etas = sum_byname(etas_temp, etas_temp %>% setrownames_byname("P2")),
    etas = sum_byname(etas, etas_temp %>% setrownames_byname("P3")),
    etas = sum_byname(etas, etas_temp %>% setrownames_byname("P4")),
    etas = sum_byname(etas, etas_temp %>% setrownames_byname("P5")),
    etas = sum_byname(etas, etas_temp %>% setrownames_byname("P6"))
  ) %>%
  mutate(
    scenario = NULL, 
    etas_temp = NULL
  )

running_list_for_expand.grid$mfg.etas <- mfg.etas_DF$etas

# Work on the prices matrix
# Move into a function later.
Prices_DF <- names(prices.base_list) %>% 
  lapply(., function(mn){mus}) %>%
  set_names(names(prices.base_list)) %>% 
  # Convert to a data frame containing all combinations.
  expand.grid() %>%
  # Keep track of scenarios: one per row.
  rownames_to_column(var = "scenario") %>%
  mutate(scenario = as.numeric(scenario)) %>% 
  # Create the matrices that will multiply the prices
  mutate(
    mus = create_price_matrix(P1 = .data$P1, 
                              P2 = .data$P2,
                              F1 = .data$F1,
                              F2 = .data$F2), 
    prices_matrix = elementproduct_byname(prices.base_matrix, mus) %>% 
      sort_rows_cols(margin = 2, colorder = c(industry.names, fin.names))    
  )
  
running_list_for_expand.grid$prices <- Prices_DF$prices_matrix

# 
# Create the data frame of scenarios
#
DF.scenario.matrices <- expand.grid(running_list_for_expand.grid)

View(DF.scenario.matrices)
