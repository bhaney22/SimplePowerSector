##############################################################################################
# Source Code: Draw_network.R
#
# Builds Network Structure and names input parameters (not passed as arguments)
# Creates the following objects:
# 	Flow.curr
# 	Flow.phys
# 	value.added.per.dollar.in (for each Process - Tot $ out/Tot $ in
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
library(readr)

load(".RData")

comma		<- c(",")
newline	<- c(" \n")

# 
# Set indexes to point to different nodes.
#
Mfg.first	<- Res.n + 1		# node number of first Mfg
Fin.first	<- Res.n + Mfg.n + 1	# node number of first Fin


Res.nodes	<- seq(1,Res.n)
Mfg.nodes	<- seq(Mfg.first,length=Mfg.n)
Fin.nodes	<- seq(Fin.first,length=Fin.n)

#
# Node Names
#
phys.units	<-	c("MW")
Res.names 	<- matrix(c("Coal","NG"))
Res.units	<- c(rep(phys.units,2))
Res.prices.units	<- c("MT","MMBTU")


Fin.names	<- c("Res","Com") 
Fin.units	<- c(rep(phys.units,2))
Fin.prices.units	<- c("kWh","kWh")


curr.scale.display <- "Millions USD"


##############################################################################################################
#
# Create .csv file of results
#
##############################################################################################################


#############################################################################################################
## Generate .csv file that has all of the factors and the results.
#############################################################################################################
RESULTS = paste("Row",
                "Eta.1",
                "Eta.2",
                "Eta.3",
                "Eta.4",
                "VA.1",
                "VA.2",
                "VA.3",
                "VA.4",
                "Res.1.price",
                "Res.2.price",
                "Fin.1.price",
                "Fin.2.price",
                "Fin.1.Mkt.share",
                "Fin.1.I.1.share",
                "Fin.1.I.2.share",
                "Fin.1.I.3.share",
                "Fin.1.I.4.share",
                "Fin.2.I.1.share",
                "Fin.2.I.2.share",
                "Fin.2.I.3.share",
                "Fin.2.I.4.share",
                "TFO","TST.phys","PRR.phys",
                "GDP.curr","TST.curr","PRR.curr",
                "alpha.phys","F.phys",
                "alpha.curr","F.curr",
                "IO.phys","IO.curr",
                sep=",")

for(row.num in 1:nrow(DF.results)) { 

Mfg.etas = DF.results$Mfg.etas.mat[[row.num]][1,]
Res.prices	= DF.results$Prices.mat[[row.num]][1:2,1]	
Fin.prices  = DF.results$Prices.mat[[row.num]][Mfg.first,Fin.nodes]


Mfg.names	<- rep("",Mfg.n)
  for(i in 1:Mfg.n) Mfg.names[i] <- paste("I",i,sep = "") 
Mfg.names.csv <- Mfg.names[i]
nodes.names <- c(Res.names,Mfg.names,Fin.names)

Res.names.phys	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.phys[i] <- 
    paste(Res.names[i],"(",Res.units[i],")",sep = "")}

Fin.names.phys	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.phys[i] <- 
    paste(Fin.names[i],"(",Fin.units[i],")",sep = "")}


Mfg.names.phys	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.phys[i] <- 
    paste("I",i,"(Eta=",round(Mfg.etas[i],1),")",sep = "")}

Mfg.names.phys.legend	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.phys.legend[i] <- 
    paste(" ",round(Mfg.etas[2+i],2),sep = "")}  #Add 2 since Mfg.etas includes resources now

Res.names.curr	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.curr[i] <- 
    paste(Res.names[i]," ($",Res.prices[i],"/",Res.prices.units[i],")",sep = "")}

Res.names.curr.legend	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.curr.legend[i] <- 
    paste(" ($",Res.prices[i],"/",Res.prices.units[i],")",sep = "")}

Fin.names.curr	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.curr[i] <- 
    paste(Fin.names[i]," ($",Fin.prices[i],"/",Fin.prices.units[i],")",sep = "")}

Fin.names.curr.legend	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.curr.legend[i] <- 
    paste(" ($",Fin.prices[i],"/",Fin.prices.units[i],")",sep = "")}

nodes.names.phys <- c(Res.names.phys,Mfg.names.phys,Fin.names.phys)
nodes.names.phys.legend <- c(Res.units,Mfg.names.phys.legend,Fin.units)
nodes.names.curr	<- c(Res.names.curr,Mfg.names.phys,Fin.names.curr)


Flows.phys			<- matrix(0,nrow=nodes.n,ncol=nodes.n)  %>%
  setrownames_byname(c(product.names,fin.names)) %>%
  setrowtype("Products") %>%
  setcolnames_byname(c(industry.names,fin.names)) %>%
  setcoltype("Industries") %>%
  sum_byname(.,DF.results$IO.phys[[row.num]]) %>% 
  sort_rows_cols(roworder=(c(product.names,fin.names)),
                 colorder=(c(industry.names,fin.names)))

Flows.curr		<- matrix(0,nrow=nodes.n,ncol=nodes.n) %>%
  setrownames_byname(c(product.names,fin.names)) %>%
  setrowtype("Products") %>%
  setcolnames_byname(c(industry.names,fin.names)) %>%
  setcoltype("Industries") %>%
  sum_byname(.,DF.results$IO.curr[[row.num]]) %>% 
  sort_rows_cols(roworder=(c(product.names,fin.names)),
                 colorder=(c(industry.names,fin.names)))

#
# Compute value-added for each industry
#
va	<-rep(0,Mfg.n)
for (i in 1:Mfg.n) {
  va[i] <- round(sum(Flows.curr[Mfg.nodes[i],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[i]]),2)
}

#########################################################################################################
# Begin .csv file
#########################################################################################################

row.results <- paste(row.num,
Mfg.etas[3],Mfg.etas[4],Mfg.etas[5],Mfg.etas[6],va[1],va[2],va[3],va[4],
Res.prices[1],Res.prices[2],
Fin.prices[1],Fin.prices[2],
DF.results$f.split[[row.num]][1],
DF.results$f.product.coeffs[[row.num]][3,1],
DF.results$f.product.coeffs[[row.num]][4,1],
DF.results$f.product.coeffs[[row.num]][5,1],
DF.results$f.product.coeffs[[row.num]][6,1],
DF.results$f.product.coeffs[[row.num]][3,2],
DF.results$f.product.coeffs[[row.num]][4,2],
DF.results$f.product.coeffs[[row.num]][5,2],
DF.results$f.product.coeffs[[row.num]][6,2],
DF.results$TFO[[row.num]],
DF.results$TST.phys[[row.num]],
DF.results$PRR.phys[[row.num]],
DF.results$GDP.curr[[row.num]],
DF.results$TST.curr[[row.num]],
DF.results$PRR.curr[[row.num]], 
DF.results$alpha.phys[[row.num]],
DF.results$F.phys[[row.num]],
DF.results$alpha.curr[[row.num]],
DF.results$F.curr[[row.num]],
DF.results$IO.phys[[row.num]],
DF.results$IO.curr[[row.num]],
sep=",")

RESULTS	<- rbind(RESULTS,row.results)
row.num <- row.num + 1
}

write(RESULTS,"Results.csv")








