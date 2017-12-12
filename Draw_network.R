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
library(statnet)
library(igraph)
library(qgraph)

source("Calc_IO_metrics.R")
#
# BRH TODO: Get the prices, etas, and V-A from DF.results
#
Mfg.etas <- c(1, 1, 1/3, 0.4, 0.4, 0.5)
Res.prices	= c(55,3)				#Resource USD per unit
Fin.prices	= c(0.10,0.10)		#Final output USD per unit
# 
# Set indexes to point to different nodes.
#
nodes.n	<- Res.n + Mfg.n + Fin.n
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

Mfg.names	<- rep("",Mfg.n)
for(i in 1:Mfg.n){ Mfg.names[i] <- paste("I",i,sep = "") }
Mfg.names.csv <- Mfg.names[i]
nodes.names <- c(Res.names,Mfg.names,Fin.names)
nodes.names

Res.names.phys	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.phys[i] <- 
    paste(Res.names[i],"(",Res.units[i],")",sep = "")}
Res.names.phys

Fin.names.phys	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.phys[i] <- 
    paste(Fin.names[i],"(",Fin.units[i],")",sep = "")}
Fin.names.phys

Mfg.names	<- rep("",Mfg.n)
for(i in 1:Mfg.n){ Mfg.names[i] <- paste("I",i,sep = "") }
Mfg.names.csv <- Mfg.names[i]
nodes.names <- c(Res.names,Mfg.names,Fin.names)
nodes.names

Res.names.phys	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.phys[i] <- 
    paste(Res.names[i],"(",Res.units[i],")",sep = "")}
Res.names.phys

Fin.names.phys	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.phys[i] <- 
    paste(Fin.names[i],"(",Fin.units[i],")",sep = "")}
Fin.names.phys

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

#
# Create Physical and Monetary IO Matrices
#
Flows.phys			<- matrix(0,nrow=nodes.n,ncol=nodes.n) 
rownames(Flows.phys)	<- c(product.names,fin.names)
colnames(Flows.phys)	<- c(industry.names,fin.names)
Flows.phys <- sum_byname(Flows.phys,DF.results$IO.phys[[5]]) %>% 
  sort_rows_cols(roworder=(c(product.names,fin.names)),colorder=(c(industry.names,fin.names)))

Flows.curr		<- matrix(0,nrow=nodes.n,ncol=nodes.n)
rownames(Flows.curr)	<- c(product.names,fin.names)
colnames(Flows.curr)	<- c(industry.names,fin.names)
Flows.curr <- sum_byname(Flows.curr,DF.results$IO.curr[[5]]) %>% 
  sort_rows_cols(roworder=(c(product.names,fin.names)),colorder=(c(industry.names,fin.names)))


#
# Compute value-added for each industry
#
value.added.per.dollar.in	<-rep(0,Mfg.n)
for (i in 1:Mfg.n) {
  value.added.per.dollar.in[i] <- round(sum(Flows.curr[Mfg.nodes[i],Fin.nodes])/sum(Flows.curr[,Mfg.nodes[i]]),2)
}

Mfg.names.curr.legend	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.curr.legend[i] <- 
    paste(" ",round(value.added.per.dollar.in[i],2),sep = "")}
nodes.names.curr.legend	<- c(Res.names.curr.legend,Mfg.names.curr.legend,Fin.names.curr.legend)


# 
# Create run.names for .tex formated line of selected IO_metrics results
#
create.phys.run.name	<- function(etas) {
  etas.string		<- paste(sprintf("%3.2f",etas),collapse=",")
  paste(paste("    Physical & (", etas.string,")",sep=""),collapse="")
}

create.curr.run.name	<- function(vas) {
  vas.string		<- paste(sprintf("%3.2f",vas),collapse=",")
  curr.run.name	<- paste(paste("     Currency &(",vas.string,")",sep=""),collapse="")
}
phys.run.name	<- create.phys.run.name(Mfg.etas)
curr.run.name	<- create.curr.run.name(value.added.per.dollar.in)

#########################################################################################################
# Begin Plots
#########################################################################################################



##################################################################################
# Function: create.network.pdf
# Draw the network: help pages are here
# http://igraph.org/r/doc/graph_from_adjacency_matrix.html 
##################################################################################
groups 	<- list(	"Input Units" =c(Res.nodes),
                 "Plant Efficiencies"=c(Mfg.nodes),
                 "Output Units"=c(Fin.nodes))
groups.curr	<- list(	"Input Prices"=c(Res.nodes),
                     "Value-Added"=c(Mfg.nodes),
                     "Output Prices"=c(Fin.nodes))
# 
shapes <- c(rep("ellipse",Res.n),rep("rectangle",Mfg.n),rep("ellipse",Fin.n))
L <- matrix(c(
  0,2.5, 0,.5,
  1,3, 1,2, 1,1, 1,0,
  2,2.5, 2,.5),
  ncol=2,byrow=TRUE)
# 
# options(width=200)
# pdf(file=paste(image.dir,"Flows_phys_",run.num,".pdf",sep=""))
qgraph(Flows.phys,  
       edge.labels=T,
       edge.label.cex=1.25,edge.color="black",fade=F,
       # BRH Commented out during csv file creation
       # not working edge.label.position=c(rep(.5,Res.n),rep(.7,Mfg.n),rep(.7,Fin.n)),
       layout=L,
       groups=groups,
       borders=F,
       labels=nodes.names,
       nodeNames=nodes.names.phys.legend,
       shape=shapes,
       palette="colorblind",
       title=paste("(Flows in MW)",sep=""))
# dev.off()
# 
# options(width=200)
# pdf(file=paste(image.dir,"Flows_curr_",run.num,".pdf",sep=""))

qgraph(Flows.curr,edge.labels=T,edge.label.cex=1.25,edge.color="black",fade=F,
# BRH Commented out during csv file creation
edge.label.position=c(rep(.5,Mfg.n),rep(.7,Mfg.n),rep(.7,Mfg.n)),
layout=L,
groups=groups.curr,
borders=F,
labels=nodes.names,
nodeNames=nodes.names.curr.legend,
shape=shapes,
palette="colorblind",
title=paste("TST=",round(DF.results$TST.curr[[5]],2)," (",curr.scale.display,
            ") alpha=",round(DF.results$alpha.curr[[5]],2),
            " F=",round(DF.results$F.curr[[5]],2),"\n",
            "PRR (phys) =",round(DF.results$PRR.phys[[5]],2),sep="") )

