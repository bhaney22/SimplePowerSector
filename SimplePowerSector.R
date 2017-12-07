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

#
# Initial Input Parameters:
#

TFO     <- 200  # total final output
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)

phys.units	  <-	"MW"
Res.desc 	    <- matrix(c("Coal","NG"))
Res.units	    <- c(rep(phys.units,2))
Res.prices.units	<- c("MT","MMBTU")
Res.prices.conv	<- c(0,0)

Fin.desc	<- c("Res","Com")
Fin.units	<- c(rep(phys.units,2))
Fin.prices.units	<- c("kWh","kWh")
Fin.prices.conv 	<- c(0,0)

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n
curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

product.names   <- c(paste0("P", seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

# Make the C matrix with the help of the byname package.
# Do so in a way that is visually correct (by using byrow = TRUE), 
# because it is simpler to debug.
f.mat <- matrix(c(0,0,
              0,0,
              0,0,
              0.6, 0.0,
              0.4, 0.0, 
              0.0, 1.0),
            nrow = 6, ncol = 2, byrow = TRUE) %>%   
            setrownames_byname(product.names) %>% # Products in rows
            setrowtype("Products") %>% # Set row type to ensure errors are thrown when manipulating matrices with wrong types.
            setcolnames_byname(fin.names) %>% # Industries in columns
            setcoltype("Industries")  # Set col type to ensure errors are thrown when manipulating matrices with wrong types.

 DF <- data.frame(F1 = seq(0, 1, by = 0.1)) %>% 
  mutate(
    F2 = 1 - F1,
    scenario.val = F1, # This becomes the metadata column
    scenario.factor = "f"
  ) %>% 
  # Use gather to form a tidy data frame that can be converted into matrices
  # (or, in this case, vectors).
  gather(key = col.name, value = value, F1, F2) %>% 
  # Add other metadata columns that will be required before 
  # collapsing into matrices.
  mutate(
    matrix.name = "f",
    row.name = "Products",
    row.type = "Products",
    col.type = "Industries"
   ) %>% 
  # Set grouping to preserve metadata columns
  group_by(scenario.factor,scenario.val) %>% 
  # Collapse to form matrices (actually, vectors)
  collapse_to_matrices(matnames = "matrix.name", values = "value",
                       rownames = "row.name", colnames = "col.name", 
                       rowtypes = "row.type", coltypes = "col.type") %>% 
  rename(
    f = value  # For readability.  These are f vectors.
  ) %>% 
  # At this point, we now have our column of f vectors.
  # Calculate g and V accordingly.
  mutate(
    f.mat = lapply(seq_len(nrow(.)), function(X) f.mat), 
    ysum = elementproduct_byname(TFO, f),
    Y = matrixproduct_byname(f.mat,hatize_byname(ysum)),
    # y = matrixproduct_byname(Y,as.vector(rep(1,Fin.n),mode="any"))
    # rowsums_byname is easier :-)
    y = rowsums_byname(Y)
  )


