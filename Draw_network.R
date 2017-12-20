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
  mutate(scenario=seq(1:nrow(DF.results)),
         Resources=ifelse(Fin.1.I.3.share==0 & Fin.2.I.3.share==0 & 
                            Fin.1.I.4.share==0 & Fin.2.I.4.share==0,"NG Only",
                          ifelse(Fin.1.I.5.share==0 & Fin.2.I.5.share==0 & 
                                   Fin.1.I.6.share==0 & Fin.2.I.6.share==0,"Coal Only",
                                 "Coal & NG")),
         NG.price.f=ifelse(Res.2.price < .1,"NG Low Price",
                           "NG High Price"),
         Scenario=ifelse(Resources=="Coal Only" & 
                           Res.2.price < .1,"Coal (NG Low Price)",
                         ifelse(Resources=="Coal Only" & 
                                  Res.2.price > .1,"Coal (NG High Price)",
                                ifelse(Resources=="NG Only" &
                                         Res.2.price < .1,"NG (NG Low Price)",
                                       ifelse(Resources=="NG Only" &
                                                Res.2.price > .1,"NG (NG High Price)",
                                              "Other"))))) %>%  
  sort_rows_cols(.,margin=2,colorder=sort(colnames(.))) %>% 
  filter(Resources != "Coal & NG")


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
# Create Physical and Monetary IO Matrices from DF.results, row.num
#
##############################################################################################################
for(row.num in seq(1,4)) { # nrow(DF.results))) { 

Mfg.etas = c(round(DF.results$Eta.1[[row.num]],2),
             round(DF.results$Eta.2[[row.num]],2),
             round(DF.results$Eta.3[[row.num]],2),
             round(DF.results$Eta.4[[row.num]],2))
VAs      = c(round(DF.results$VA.1[[row.num]],2),
             round(DF.results$VA.2[[row.num]],2),
             round(DF.results$VA.3[[row.num]],2),
             round(DF.results$VA.4[[row.num]],2))
Res.prices	= c(round(DF.results$Res.1.price[[row.num]],2),
                round(DF.results$Res.2.price[[row.num]],2))
Fin.prices  = c(round(DF.results$Fin.1.price[[row.num]],2),
                round(DF.results$Fin.2.price[[row.num]],2))

Mfg.names.phys	<- rep("",Mfg.n)
for(i in 1:Mfg.n){
  Mfg.names.phys[i] <- 
    paste("I",i,"(Eta=",round(Mfg.etas[i+2],1),")",sep = "")}

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
num.res.flows <- colSums(DF.results$Flows.phys[[row.num]] != 0)[1:6] %>% sum()
num.fin.flows <- colSums(DF.results$Flows.phys[[row.num]] != 0)[7:8] %>% sum()
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
pdf(file=paste0(image.dir,"Flows_phys_",row.num,".pdf"))
qgraph(DF.results$Flows.phys[[row.num]],  
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
       "Power Return Ratio =",round(DF.results$PRR.phys[[row.num]],2),
       " (phys), ",round(DF.results$PRR.curr[[row.num]],2), " (curr)", 
       "\n a=",round(DF.results$alpha.phys[[row.num]],2),
       "\n F=",round(DF.results$F.phys[[row.num]],2),
       "\n (Flows in MW)") )
dev.off()

############################################################################################
# Graph Currency Flows network
############################################################################################
pdf(file=paste0(image.dir,"Flows_curr_",row.num,".pdf"))

qgraph(DF.results$Flows.curr[[row.num]],
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
        title=paste0("GDP $",format(DF.results$GDP.curr[[row.num]],width=7)," (million)               ",
                    "Power Return Ratio =",round(DF.results$PRR.phys[[row.num]],2),
                    " (phys), ",round(DF.results$PRR.curr[[row.num]],2), " (curr)", 
                    "\n a=",round(DF.results$alpha.curr[[row.num]],2),
                    "\n F=",round(DF.results$F.curr[[row.num]],2),
                    "\n (Flows in $millions)") )
dev.off()       


row.num <- row.num + 1
}

