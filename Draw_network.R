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

image.dir	<- c("C:/Users/brh22/Dropbox/Apps/ShareLaTeX/Sabbatical Technical Note/Images/")
load("DF.results.Rda")
df <- DF.results[order(DF.results$PRR.curr),] %>% 
  mutate(scenario=seq(1:nrow(DF.results))) %>%
  sort_rows_cols(.,margin=2,colorder=sort(colnames(.))) 

# 
# Set indexes to point to different nodes.
#
Res.n		<- 2		# number of extraction industries/products
Mfg.n		<- 4		# number of intermediate industries/products
Fin.n		<- 2 		# number of final output industries (should be perfect complements)
nodes.n   <- Res.n + Mfg.n + Fin.n

curr.scale	<- 10^(-6)
curr.scale.display <- "Millions USD"

Mfg.first	<- Res.n + 1		# node number of first Mfg
Fin.first	<- Res.n + Mfg.n + 1	# node number of first Fin

Res.nodes	<- seq(1,Res.n)
Mfg.nodes	<- seq(Mfg.first,length=Mfg.n)
Fin.nodes	<- seq(Fin.first,length=Fin.n)

Prod.n  <- Res.n + Mfg.n
Ind.n   <- Res.n + Mfg.n

product.names   <- c(paste0("P",seq(1:Prod.n)))
industry.names  <- c(paste0("I",seq(1:Ind.n)))
fin.names       <- c(paste0("F",seq(1:Fin.n)))

#
# Node Names
#
phys.units	<-	c("MW")
Res.names 	<- matrix(c("Coal","NG"))
Res.units	<- c(rep(phys.units,2))
#Res.prices.units	<- c("MT","MMBTU")
Res.prices.units	<- c("W","W")


Fin.names	<- c("Res","Com") 
Fin.units	<- c(rep(phys.units,2))
# Fin.prices.units	<- c("kWh","kWh")
Fin.prices.units	<- c("W","W")

Mfg.names	<- rep("",Mfg.n)
for(i in 1:Mfg.n){ Mfg.names[i] <- paste("I",i,sep = "") }

Res.names.phys	<- rep("",Res.n)
for(i in 1:Res.n){
  Res.names.phys[i] <- 
    paste(Res.names[i],"(",Res.units[i],")",sep = "")}

Fin.names.phys	<- rep("",Fin.n)
for(i in 1:Fin.n){
  Fin.names.phys[i] <- 
    paste(Fin.names[i],"(",Fin.units[i],")",sep = "")}

nodes.names <- c(Res.names,Mfg.names,Fin.names)

curr.scale.display <- "Millions USD"

##############################################################################################################
#
# Create Physical and Monetary IO Matrices from DF.results
#
##############################################################################################################
#
# Choose the scenarios that have the max PRR.curr, PRR.phys, F.phys
#
max.PRR.curr = df$scenario[[which.max(df$PRR.curr)]]
max.PRR.phys = df$scenario[[which.max(df$PRR.phys)]]
max.F.curr   = df$scenario[[which.max(df$F.curr)]]
max.F.phys   = df$scenario[[which.max(df$F.phys)]]

for(scenario in list(max.PRR.curr,max.PRR.phys,max.F.phys,max.F.curr))  { 

Mfg.etas = c(round(df$Eta.3[[scenario]],2),
             round(df$Eta.4[[scenario]],2),
             round(df$Eta.5[[scenario]],2),
             round(df$Eta.6[[scenario]],2))
VAs      = c(round(df$VA.3[[scenario]],2),
             round(df$VA.4[[scenario]],2),
             round(df$VA.5[[scenario]],2),
             round(df$VA.6[[scenario]],2))
Res.prices	= c(round(df$Res.1.price[[scenario]],2),
                round(df$Res.2.price[[scenario]],2))
Fin.prices  = c(round(df$Fin.1.price[[scenario]],2),
                round(df$Fin.2.price[[scenario]],2))

Mfg.names.phys	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.phys[i] <- 
    paste("I",i,"(Eta=",round(Mfg.etas[i],1),")",sep = "")}

Mfg.names.phys.legend	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.phys.legend[i] <- 
    paste(" ",round(Mfg.etas[i],2),sep = "")}  

Mfg.names.curr.legend	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.curr.legend[i] <- 
    paste(" $",round(VAs[i],2),sep = "")}

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
nodes.names.curr.legend	<- c(Res.names.curr.legend,
                             Mfg.names.curr.legend,
                             Fin.names.curr.legend)
#########################################################################################################
# Set up for Network graphs
#########################################################################################################
num.res.flows <- colSums(df$Flows.phys[[scenario]] != 0)[1:6] %>% sum()
num.fin.flows <- colSums(df$Flows.phys[[scenario]] != 0)[7:8] %>% sum()
edge.label.position.vals <- c(rep(.5,num.res.flows),rep(.7,(num.fin.flows)))

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
pdf(file=paste0(image.dir,"Flows_phys_",scenario,".pdf"))
qgraph(df$Flows.phys[[scenario]],  
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
       title=paste0("Total Final Output ",df$TFO[[scenario]]," (MW)     ",
       "Efficiency =",round(df$PRR.phys[[scenario]],2),
       " (phys), ",round(df$PRR.curr[[scenario]],2), " (curr)", 
       "\n a=",round(df$alpha.phys[[scenario]],2),
       "\n F=",round(df$F.phys[[scenario]],2),
       "\n (Flows in MW)") )
dev.off()

############################################################################################
# Graph Currency Flows network
############################################################################################
pdf(file=paste0(image.dir,"Flows_curr_",scenario,".pdf"))

qgraph(df$Flows.curr[[scenario]],
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
        title=paste0("GDP $",format(df$GDP.curr[[scenario]],width=7)," (million)               ",
                    "Efficiency =",round(df$PRR.phys[[scenario]],2),
                    " (phys), ",round(df$PRR.curr[[scenario]],2), " (curr)", 
                    "\n a=",round(df$alpha.curr[[scenario]],2),
                    "\n F=",round(df$F.curr[[scenario]],2),
                    "\n (Flows in $millions)") )
dev.off()       

}

