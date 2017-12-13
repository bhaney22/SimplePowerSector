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
library(statnet)
library(igraph)
library(qgraph)

image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Notes/Images/")
source("Calc_IO_metrics.R")

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
# Create Physical and Monetary IO Matrices from DF.results, row.num
#
##############################################################################################################
#### for(row.num in 1:nrow(DF.results)) { 
for(row.num in 1:2) {

Mfg.etas = DF.results$Mfg.etas.mat[[row.num]][1,]
Res.prices	= DF.results$prices.mat[[row.num]][1:2,1]	
Fin.prices  = DF.results$Prices.mat[[row.num]][Mfg.first,Fin.nodes]


Mfg.names	<- rep("",Mfg.n)
for(i in 1:Mfg.n){ Mfg.names[i] <- paste("I",i,sep = "") }
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
  vas.string		<- paste(sprintf("%3.1f",vas),collapse=",")
  curr.run.name	<- paste(paste("     Currency &(",vas.string,")",sep=""),collapse="")
}
phys.run.name	<- create.phys.run.name(Mfg.etas)
curr.run.name	<- create.curr.run.name(value.added.per.dollar.in)

#########################################################################################################
# Begin Plots
#########################################################################################################
num.edges <- ecount(graph_from_adjacency_matrix(Flows.phys,weighted=T))
edge.label.position.vals <- c(rep(.5,Mfg.n),rep(.7,(num.edges-Mfg.n)))

groups 	<- list(	"Input Units" =c(Res.nodes),
                 "MW produced per MW"=c(Mfg.nodes),
                 "Output Units"=c(Fin.nodes))
groups.curr	<- list(	"Input Prices"=c(Res.nodes),
                     "Value-Added per $"=c(Mfg.nodes),
                     "Output Prices"=c(Fin.nodes))

shapes <- c(rep("ellipse",Res.n),rep("rectangle",Mfg.n),rep("ellipse",Fin.n))

L <- matrix(c(
  0,2.5, 0,.5,
  1,3, 1,2, 1,1, 1,0,
  2,2.5, 2,.5),
  ncol=2,byrow=TRUE)
############################################################################################
# Graph Physical Flows network
############################################################################################# 
pdf(file=paste0(image.dir,"Flows_phys_",row.num,".pdf"))
qgraph(Flows.phys,  
       edge.labels=T,
       edge.label.cex=1.25,edge.color="black",fade=F,
       edge.label.position=edge.label.position.vals,
       layout=L,
       groups=groups,
       borders=F,
       labels=nodes.names,
       nodeNames=nodes.names.phys.legend,
       shape=shapes,
       palette="colorblind",
       title=paste0("Total Final Output ",DF.results$TFO[[row.num]]," (MW)     ",
       "PRR =",round(DF.results$PRR.phys[[row.num]],2),
       " (phys), ",round(DF.results$PRR.curr[[row.num]],2), " (curr)", 
       "\n a=",round(DF.results$alpha.phys[[row.num]],2),
       "\n F=",round(DF.results$F.phys[[row.num]],2)) )
dev.off()

############################################################################################
# Graph Currency Flows network
############################################################################################
pdf(file=paste0(image.dir,"Flows_curr_",row.num,".pdf"))

qgraph(Flows.curr,
        edge.labels=T,
        edge.label.cex=1.25,edge.color="black",fade=F,
        edge.label.position=edge.label.position.vals,
        layout=L,
        groups=groups.curr,
        borders=F,
        labels=nodes.names,
        nodeNames=nodes.names.curr.legend,
        shape=shapes,
        palette="colorblind",
        title=paste0("GDP $",round(DF.results$GDP.curr[[row.num]],2),
                     " (millions)     Power Return Ratio =",round(DF.results$PRR.curr[[row.num]],2),
                    " (phys), ",round(DF.results$PRR.curr[[row.num]],2), " (curr)", 
                    "\n a=",round(DF.results$alpha.curr[[row.num]],2),
                    "\n F=",round(DF.results$F.curr[[row.num]],2)) )
dev.off()       


row.num <- row.num + 1
}