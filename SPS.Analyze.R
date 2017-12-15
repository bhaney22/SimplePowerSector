##############################################################################################
# Source Code: SPS.Analyze.R
#
# Statistical and graphical analysis of the SPS results.
##############################################################################################
library(byname)   # Provides convenient matrix manipulation in data frames.
library(parallel) # For the function mcMap.  (Not sure why this didn't come in with byname.)
library(magrittr) # For the pipe operator (%>%)
library(dplyr)    # For mutate and other helpful functions
library(tidyr)    # For spread and gather functions
library(tibble)   # For the rownames_to_column function.  (Not sure why this didn't come in with matsindf.)
library(lazyeval) # For the interp function.  (Not sure why this didn't come in with matsindf.)
library(matsindf) # For collapse_to_matrices and expand_to_tidy functions
library(ggplot2)  # For awesome plotting functions
library(plotly)
library(statnet)
library(igraph)
library(qgraph)

image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Note/Images/")
load("DF.results.f.RDa")

##########################################################################################
# Explore the relationship between efficiency in ENA (measured as alpha) and 
# efficiency in energy as measured by the PRR (PowerReturnRatio),
# controlling for various factors: prices, etas, and market shares.
# 
# Each measure of efficiency is measured in both physical and currency units.
##########################################################################################
Eta.1.f           <- levels(as.factor(unlist(DF.results.f$Eta.1)))
Eta.2.f           <- levels(as.factor(unlist(DF.results.f$Eta.2)))
Eta.3.f           <- levels(as.factor(unlist(DF.results.f$Eta.3)))
Eta.4.f           <- levels(as.factor(unlist(DF.results.f$Eta.4)))
Res.1.price.f     <- levels(as.factor(unlist(DF.results.f$Res.1.price)))
Res.2.price.f     <- levels(as.factor(unlist(DF.results.f$Res.2.price)))
Fin.1.price.f     <- levels(as.factor(unlist(DF.results.f$Fin.1.price)))
Fin.2.price.f     <- levels(as.factor(unlist(DF.results.f$Fin.2.price)))
Fin.1.Mkt.share.f <- levels(as.factor(unlist(DF.results.f$Fin.1.Mkt.share)))
Fin.1.I.1.share.f <- levels(as.factor(unlist(DF.results.f$Fin.1.I.1.share)))
Fin.1.I.2.share.f <- levels(as.factor(unlist(DF.results.f$Fin.1.I.2.share)))
Fin.1.I.3.share.f <- levels(as.factor(unlist(DF.results.f$Fin.1.I.3.share)))
Fin.2.I.1.share.f <- levels(as.factor(unlist(DF.results.f$Fin.2.I.1.share)))
Fin.2.I.2.share.f <- levels(as.factor(unlist(DF.results.f$Fin.2.I.2.share)))
Fin.2.I.3.share.f <- levels(as.factor(unlist(DF.results.f$Fin.2.I.3.share)))

DF.base         <- subset(DF.results.f,
                              Eta.1==Eta.1.f[1] & 
                              Eta.2==Eta.2.f[1] &
                              Eta.3==Eta.3.f[1] &
                              Eta.4==Eta.4.f[1] &
                              Res.1.price==Res.1.price.f[1] &
                              Res.2.price==Res.2.price.f[1] &
                              Fin.1.price==Fin.1.price.f[1] &
                              Fin.2.price==Fin.2.price.f[1] &
                              Fin.1.Mkt.share==Fin.1.Mkt.share.f[1] &
                              Fin.1.I.1.share==Fin.1.I.1.share.f[1] &
                              Fin.1.I.2.share==Fin.1.I.2.share.f[1] &
                              Fin.1.I.3.share==Fin.1.I.3.share.f[1] &
                              Fin.2.I.1.share==Fin.2.I.1.share.f[1] &
                              Fin.2.I.2.share==Fin.2.I.2.share.f[1] &
                              Fin.2.I.3.share==Fin.2.I.3.share.f[1])

DF.f.split         <- subset(DF.results.f,
                          Eta.1==Eta.1.f[1] & 
                            Eta.2==Eta.2.f[1] &
                            Eta.3==Eta.3.f[1] &
                            Eta.4==Eta.4.f[1] &
                            Res.1.price==Res.1.price.f[1] &
                            Res.2.price==Res.2.price.f[1] &
                            Fin.1.price==Fin.1.price.f[1] &
                            Fin.2.price==Fin.2.price.f[1] &
                         #   Fin.1.Mkt.share==Fin.1.Mkt.share.f[1] &
                            Fin.1.I.1.share==Fin.1.I.1.share.f[1] &
                            Fin.1.I.2.share==Fin.1.I.2.share.f[1] &
                            Fin.1.I.3.share==Fin.1.I.3.share.f[1] &
                            Fin.2.I.1.share==Fin.2.I.1.share.f[1] &
                            Fin.2.I.2.share==Fin.2.I.2.share.f[1] &
                            Fin.2.I.3.share==Fin.2.I.3.share.f[1])
