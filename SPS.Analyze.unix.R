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
library(purrr)
library(statnet)
library(igraph)
library(qgraph)
library(webshot)

#image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Note/Images/")

##########################################################################################
# Explore the relationship between efficiency in ENA (measured as alpha) and 
# efficiency in energy as measured by the PRR (PowerReturnRatio),
# controlling for various factors: prices, etas, and market shares.
# 
# Each measure of efficiency is measured in both physical and currency units.
##########################################################################################
load("DF.results.Rda")
df <- DF.results[order(DF.results$PRR.curr),] %>% 
          mutate(scenario=seq(1:nrow(DF.results)),
                 price.ratio=Res.1.price/Res.2.price,
                 Num.Coal.plants=sapply(X=F.product.coeffs, function(X) sum(sum(X[3,])>0,sum(X[4,])>0)),
                 Num.NG.plants=sapply(X=F.product.coeffs, function(X) sum(sum(X[5,])>0,sum(X[6,])>0)),
                 Even.split.coal=sapply(X=F.product.coeffs, function(X) (X[3,1])==.5 & X[4,1]==.5),
                 Even.split.NG=sapply(X=F.product.coeffs, function(X) (X[5,1])==.5 & X[6,1]==.5),
                 Num.plants=ifelse(Num.Coal.plants + Num.NG.plants == 1,"1. One Plant",
                            ifelse(Even.split.coal=="TRUE" | Even.split.NG == "TRUE", "2. Two Plants 50/50",
                                   "3. Two Plants"))) %>%
          sort_rows_cols(.,margin=2,colorder=sort(colnames(.))) 


tally(group_by(df,Resources))


df.maxvals <- df %>% filter(Resources == "Coal & NG") %>%
  select(Res.1.price,Res.2.price,
         F.phys, F.curr,PRR.curr, PRR.phys) %>%
  mutate(
     max_econ_eff = sapply(X=Res.1.price, Y=Res.2.price, function(X,Y) { 
        max(df[which(Res.1.price==X & Res.2.price==Y),]$PRR.curr)
  }),max_bio_eff = sapply(X=Res.1.price, Y=Res.2.price, function(X,Y) { 
        max(df[which(Res.1.price==X & Res.2.price==Y),]$PRR.phys)
  }),max_econ_robust = sapply(X=Res.1.price, Y=Res.2.price, function(X,Y) { 
        max(df[which(Res.1.price==X & Res.2.price==Y),]$F.curr)
  }),max_bio_robust = sapply(X=Res.1.price, Y=Res.2.price, function(X,Y) { 
        max(df[which(Res.1.price==X & Res.2.price==Y),]$F.phys)
  })) 

df.maxvals.z <- as.data.frame(scale(df.maxvals))
  
save(df.maxvals,file="df.maxvals.Rda")
