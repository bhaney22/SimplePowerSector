library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions
setwd("C:/Users/brh22/Google Drive/_ Energy Economics/Input-Output/R programs")
rm(list=ls())

image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Notes/Images/")
data.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Notes/Images/")
source("Housekeeping.R")
source("Calc_IO_metrics.R")
source("Conversions.R")
source("FormattedResults.R")

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

product.names   <- c(paste("P",seq(1:Prod.n),sep=""))
industry.names  <- c(paste("I",seq(1:Ind.n),sep=""))
fin.names       <- c(paste("F",seq(1:Fin.n),sep=""))

rm(DF)

# Make the C matrix with the help of the byname package.
# Do so in a way that is visually correct (by using byrow = TRUE), 
# because it is simpler to debug.
f.mat <- matrix(c(0,0,
              0,0,
              0,0,
              0.6, 0.0,
              0.4, 0.0, 
              0.0, 1.0),
            nrow = 6, ncol = 2, byrow = TRUE) %>%   # Redundant to supply both nrow and ncol, but makes intent very clear.
  setrownames_byname(product.names) %>% # Products in rows
  setrowtype("Products") %>% # Set row type to ensure errors are thrown when manipulating matrices with wrong types.
  setcolnames_byname(fin.names) %>% # Industries in columns
  setcoltype("Industries")  # Set col type to ensure errors are thrown when manipulating matrices with wrong types.

DF <- data.frame(i1 = seq(0, 1, by = 0.1)) %>% 
  # Calculate i2 values
  mutate(
    i2 = 1 - i1,
    f1 = i1 # This becomes the metadata column
  ) %>% 
  # Use gather to form a tidy data frame that can be converted into matrices
  # (or, in this case, vectors).
  gather(key = row.name, value = value, i1, i2) %>% 
  # Add other metadata columns that will be required before 
  # collapsing into matrices.
  mutate(
    matrix.name = "f",
    col.name = "Products",
    row.type = "Industries",
    col.type = "Products"
  ) %>% 
  # Set grouping to preserve metadata columns
  group_by(f1) %>% 
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
    Y = matrixproduct_byname(f.mat,hatize_byname(ysum))
    # V = matrixproduct_byname(C, hatize_byname(g)) %>% transpose_byname()
  )

## Testing
YY = DF$f.mat[[1]] %*% hatize_byname(DF$ysum[[1]])  # Gives the correct answer!!
YY

ZZ = matrixproduct_byname(DF$f.mat[[1]],hatize_byname(DF$ysum[[1]]))
ZZ
