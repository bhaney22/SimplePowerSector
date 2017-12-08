##############################################################################################
# Function: Convert.prices
#
# Convert Prices in $/phys unit to flow price in $/MW per year
# Requires 3 arguments: 1. Physical unit price 
#				2. Physical unit
#				3. currency scale factor (e.g. 10^-6 to put in $Millions USD)
#
##############################################################################################
Convert.prices 	<- function(prices,units,scale) {

###################################
# Set conversion factor constants #
###################################
MJ.per.MWs		<- 1
kWh.per.Joule	<- 1/(3.6*10^6)
MWh.per.Joule	<- 1/3.6
Joule.per.MJ	<- 10^6
MT.per.kg		<- 1/1000
kg.per.MJ		<- 1/31
MJ.per.Joule	<- 1/10^6
MMBTU.per.BTU	<- 1/10^6
BTU.per.Joule	<- 1/1055.056
kW.per.MW		<- 10^6
sec.per.year	<- 60*60*24*365

#########################################################################
# BRH scratch pad for reference only:
# 1 MW sec 	= 1 MJ
# 1 MW year = 1 MJ(3600sec/hr)(24hr/day)(365days/year)
# example:
# Annual flow cost of 1 MW NG 		=  $89,671 when NG   = $3/MMBTU
# Annual flow cost of 1 MW Coal 	=  $55,951 when Coal = $55/MT
# Annual flow cost of 1 MW Elec		= $876,000 when Elec = $0.10/kWh
###########################################################################

price.conversion	<-c(	"MT"	  = (MT.per.kg * kg.per.MJ * 
					MJ.per.MWs * sec.per.year),
				"MMBTU" = (MMBTU.per.BTU * BTU.per.Joule * Joule.per.MJ * 
					MJ.per.MWs * sec.per.year),
				"kWh"	  = (kWh.per.Joule * Joule.per.MJ *
					MJ.per.MWs * sec.per.year)   )
	result	<- c(prices*price.conversion[units]*scale)
}