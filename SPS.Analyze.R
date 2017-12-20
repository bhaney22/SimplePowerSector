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

image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Note/Images/")

##########################################################################################
# Explore the relationship between efficiency in ENA (measured as alpha) and 
# efficiency in energy as measured by the PRR (PowerReturnRatio),
# controlling for various factors: prices, etas, and market shares.
# 
# Each measure of efficiency is measured in both physical and currency units.
##########################################################################################
load("DF.results.full.Rda")
df <- DF.results[order(DF.results$PRR.curr),] %>% 
          mutate(scenario=seq(1:nrow(DF.results)),
                 Num.Coal.plants=sapply(X=F.product.coeffs, function(X) sum(sum(X[3,])>0,sum(X[4,])>0)),
                 Num.NG.plants=sapply(X=F.product.coeffs, function(X) sum(sum(X[5,])>0,sum(X[6,])>0)),
                 Even.split.coal=sapply(X=F.product.coeffs, function(X) (X[3,1])==.5 & X[4,1]==.5),
                 Even.split.NG=sapply(X=F.product.coeffs, function(X) (X[5,1])==.5 & X[6,1]==.5),
                 Num.plants=ifelse(Num.Coal.plants + Num.NG.plants == 1,"1. One Plant",
                            ifelse(Even.split.coal=="TRUE" | Even.split.NG == "TRUE", "2. Two Plants 50/50",
                                   "3. Two Plants"))) %>%
          sort_rows_cols(.,margin=2,colorder=sort(colnames(.))) %>%
    filter(!(Resources=="Coal & NG" & Num.plants=="3. Two Plants"))

tally(group_by(df,Resources,Num.plants))

df.gath <- df %>% 
  select(Resources,Res.2.price,Eta.3,Num.plants,
         alpha.phys, alpha.curr,
         F.phys, F.curr,PRR.curr, PRR.phys) %>%
  gather(key = "y.var", value = "y.val",
        # F.phys, F.curr, alpha.phys, alpha.curr, PRR.phys)
F.curr, F.phys,PRR.phys)

#
# ggplot 
#

pdf(file=paste0(image.dir,"SPSplotsfull1.pdf"))

df.gath %>% filter(Res.2.price > .1 & Eta.3 < .5) %>%
ggplot(mapping = aes_string(x = "PRR.curr", y = "y.val")) +
  geom_point(aes(color = Num.plants)) +
#  geom_text() +
 facet_grid(y.var ~ Resources, scales = "free_y") +
# scale_x_continuous(limits = c(5,6.5)) +
  labs(x = "Economic Value Added", y = NULL) +
  ggtitle("NG Price (High) & Coal Plant 1 Eta (Lowest)") +
  theme_bw()
dev.off()

pdf(file=paste0(image.dir,"SPSplotsfull2.pdf"))
df.gath %>% filter(Res.2.price > .1 & Eta.3 >= .5) %>%
  ggplot(mapping = aes_string(x = "PRR.curr", y = "y.val")) +
  geom_point(aes(color = Num.plants)) +
  #  geom_text() +
  facet_grid(y.var ~ Resources, scales = "free_y") +
  # scale_x_continuous(limits = c(5,6.5)) +
  labs(x = "Economic Value Added", y = NULL) +
  ggtitle("NG Price (High) & Coal Plant 1 Eta (Highest)") +
  theme_bw()
dev.off()

pdf(file=paste0(image.dir,"SPSplotsfull3.pdf"))
df.gath %>% filter(Res.2.price < .1 & Eta.3 < .5) %>%
  ggplot(mapping = aes_string(x = "PRR.curr", y = "y.val")) +
  geom_point(aes(color = Num.plants)) +
  #  geom_text() +
  facet_grid(y.var ~ Resources, scales = "free_y") +
  # scale_x_continuous(limits = c(5,6.5)) +
  labs(x = "Economic Value Added", y = NULL)  +
  ggtitle("NG Price (Low) & Coal Plant 1 Eta (Lowest)") +
  theme_bw()
dev.off()

pdf(file=paste0(image.dir,"SPSplotsfull4.pdf"))
df.gath %>% filter(Res.2.price < .1 & Eta.3 >= .5) %>%
  ggplot(mapping = aes_string(x = "PRR.curr", y = "y.val")) +
  geom_point(aes(color = Num.plants)) +
  #  geom_text() +
  facet_grid(y.var ~ Resources, scales = "free_y") +
  # scale_x_continuous(limits = c(5,6.5)) +
  labs(x = "Economic Value Added", y = NULL) +
  ggtitle("NG Price (Low) & Coal Plant 1 Eta (Highest)") +
  theme_bw()

dev.off()

########################################################################################
# WRite out long and wide .csv files
########################################################################################
df.gath.csv <- df %>% 
  select(Resources,Res.2.price,Eta.3,Num.plants,
         alpha.phys, alpha.curr,
         F.phys, F.curr,PRR.curr, PRR.phys) %>%
  gather(key = "y.var", value = "y.val",
         alpha.phys,
         F.curr, F.phys,PRR.phys)

write.csv(df.gath.csv,"df.gath.csv")
df.spread <- spread(df.gath.csv,y.var,y.val)
write.csv(df.spread,"df.spread.csv")

