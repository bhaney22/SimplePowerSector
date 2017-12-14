##############################################################################################
# Source Code: Save_results.R
# BRH 12.14.2017
# This script is called by SPS.R to (1) write the results to a .csv file and (2) save a DF
# that contains only the factors, IO metrics, and IO flows matrices that can be loaded in
# for analysis.
#
# Future TODO: use mutate and lapply to build a DF that is written to the .csv file.
#
##############################################################################################
library(readr)

rm(list=ls())
load("DF.results")

# 
# Set indexes to point to different nodes.
#
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)
nodes.n <- Res.n + Mfg.n + Fin.n

Mfg.first	<- Res.n + 1		# node number of first Mfg
Fin.first	<- Res.n + Mfg.n + 1	# node number of first Fin


Res.nodes	<- seq(1,Res.n)
Mfg.nodes	<- seq(Mfg.first,length=Mfg.n)
Fin.nodes	<- seq(Fin.first,length=Fin.n)

#
# Node Names
#
Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

phys.units	<-	c("MW")
Res.names 	<- matrix(c("Coal","NG"))
Res.units	<- c(rep(phys.units,2))
Res.prices.units	<- c("MT","MMBTU")


Fin.names	<- c("Res","Com") 
Fin.units	<- c(rep(phys.units,2))
Fin.prices.units	<- c("kWh","kWh")


curr.scale.display <- "Millions USD"

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
                sep=",")

for(row.num in 1:nrow(DF.results)) { 

Mfg.etas = DF.results$Mfg.etas.mat[[row.num]][1,]
Res.prices	= DF.results$Prices.mat[[row.num]][1:2,1]	
Fin.prices  = DF.results$Prices.mat[[row.num]][Mfg.first,Fin.nodes]

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
sep=",")

RESULTS	<- rbind(RESULTS,row.results)
row.num <- row.num + 1
}

write(RESULTS,"Results.csv")
DF.results.f <- data.frame(read.csv("Results.csv"))
DF.IO.mats <- select(DF.results,IO.phys, IO.curr)
DF.results.f <- cbind(DF.results.f,DF.IO.mats)
save(DF.results.f,file="DF.results.f")
rm(list=ls())






