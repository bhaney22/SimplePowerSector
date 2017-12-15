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
DF.results.f <- DF.results %>%
  mutate(Row=seq(1,nrow(DF.results)),
                Eta.1=lapply(X=Mfg.etas.mat, function(X) X[1,3]),
                Eta.2=lapply(X=Mfg.etas.mat, function(X) X[1,4]),
                Eta.3=lapply(X=Mfg.etas.mat, function(X) X[1,5]),
                Eta.4=lapply(X=Mfg.etas.mat, function(X) X[1,6]),
                Res.1.price=lapply(X=Prices.mat, function(X) X[1,1]),
                Res.2.price=lapply(X=Prices.mat, function(X) X[2,1]),
                Fin.1.price=lapply(X=Prices.mat, function(X) X[Mfg.first,Fin.first]),
                Fin.2.price=lapply(X=Prices.mat, function(X) X[Mfg.first,Fin.first+1]),
                Fin.1.Mkt.share=lapply(X=f.split, function(X) X[1]),
                Fin.1.I.1.share=lapply(X=f.product.coeffs, function(X) X[3,1]),
                Fin.1.I.2.share=lapply(X=f.product.coeffs, function(X) X[4,1]),
                Fin.1.I.3.share=lapply(X=f.product.coeffs, function(X) X[5,1]),
                Fin.1.I.4.share=lapply(X=f.product.coeffs, function(X) X[6,1]),
                Fin.2.I.1.share=lapply(X=f.product.coeffs, function(X) X[3,2]),
                Fin.2.I.2.share=lapply(X=f.product.coeffs, function(X) X[4,2]),
                Fin.2.I.3.share=lapply(X=f.product.coeffs, function(X) X[5,2]),
                Fin.2.I.4.share=lapply(X=f.product.coeffs, function(X) X[6,2]),
                Flows.phys=lapply(X=IO.phys, function(IO.phys) {
                    matrix(0,nrow=nodes.n,ncol=nodes.n)  %>%
                    setrownames_byname(c(product.names,fin.names)) %>%
                    setrowtype("Products") %>%
                    setcolnames_byname(c(industry.names,fin.names)) %>%
                    setcoltype("Industries") %>%
                    sum_byname(.,IO.phys) %>% 
                    sort_rows_cols(roworder=(c(product.names,fin.names)),
                                   colorder=(c(industry.names,fin.names)))
                }),
                 Flows.curr=lapply(X=IO.curr, function(IO.curr){ 
                   matrix(0,nrow=nodes.n,ncol=nodes.n) %>%
                   setrownames_byname(c(product.names,fin.names)) %>%
                   setrowtype("Products") %>%
                   setcolnames_byname(c(industry.names,fin.names)) %>%
                   setcoltype("Industries") %>%
                   sum_byname(.,IO.curr) %>% 
                   sort_rows_cols(roworder=(c(product.names,fin.names)),
                                  colorder=(c(industry.names,fin.names)))
                }),
                   VA.1=lapply(X=Flows.curr,function(Flows.curr)
                     sum(Flows.curr[Mfg.nodes[1],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[1]])),
                   VA.2=lapply(X=Flows.curr,function(Flows.curr)
                     sum(Flows.curr[Mfg.nodes[2],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[2]])),
                   VA.3=lapply(X=Flows.curr,function(Flows.curr)
                     sum(Flows.curr[Mfg.nodes[3],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[3]])),
                   VA.4=lapply(X=Flows.curr,function(Flows.curr)
                     sum(Flows.curr[Mfg.nodes[4],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[4]])),
                    Mfg.etas.mat=NULL,Prices.mat=NULL,f.split=NULL,f.product.coeffs=NULL)


save(DF.results.f,file="DF.results.f")
rm(list=ls())






