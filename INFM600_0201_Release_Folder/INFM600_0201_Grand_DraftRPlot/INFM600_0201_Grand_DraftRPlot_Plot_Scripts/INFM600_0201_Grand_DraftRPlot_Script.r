#Command to read data,data2014 is the variable used to store the dataset TRI_2014_US_Dataset.csv
data2014=read.csv(file.choose(),header=TRUE)
#Command to load ggplot2 package
library(ggplot2)
#Command to load extrafont package
library(extrafont)
#command to load xkcd package
library(xkcd)
#------Q1a) Total Pollution State-wise--------

#State wise data is aggregated based on total realeases(Pollution)
#state_aggregated_data is the variable which stores a statewise list of pollution emitted 
state_aggregated_data= aggregate(data2014$TOTAL_RELEASES,list(data2014$ST),sum,na.rm=TRUE)

#******ggplot commands with xkcd packages********

ggplot(state_aggregated_data,aes(x=state_aggregated_data$Group.1,y=state_aggregated_data$x,fill=state_aggregated_data$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+xlab("States of US")+ylab("Pollutants Released in pounds")+ggtitle("State wise Pollutant Emission")+theme(legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="States of US"))+theme_xkcd()

#******ggplot commands with light theme packages******
ggplot(state_aggregated_data,aes(x=state_aggregated_data$Group.1,y=state_aggregated_data$x,fill=state_aggregated_data$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+xlab("States of US")+ylab("Pollutants Released in pounds")+ggtitle("State wise Pollutant Emission")+theme(legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="States of US"))+theme_light()

#--------Q1b) Most polluted county in each state ------

#subsetting data for state Alaska
alaska_subset=data2014[which(data2014$ST=='AK'),]
#alaskacounty is a variable used for storing aggregated data from alaska_subset according to total releases county wise of each state
alaskacounty=aggregate(alaska_subset$TOTAL_RELEASES,list(alaska_subset$COUNTY),sum,na.rm=TRUE)

#******Commands for ggplot**********
ggplot(alaskacounty,aes(x=alaskacounty$Group.1,y=alaskacounty$x,fill=alaskacounty$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("County")+ylab("Pollutants Released in pounds")+ggtitle("County wise distribution of Pollutant Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="County Name"))

#-----------Q2)--------------------------------------------

#Prerequisities
#Install required packages
install.packages("ggplot2")
install.packages("extrafont")
intsall.packages("reshape")
intsall.packages("xkcd")

#Load required packages
library(ggplot2)
library(extrafont)
library(reshape)
library(xkcd)

#Load the XKCD font
#make sure XKCD font is present in C:/Windows/Fonts
#else download at https://github.com/ekmett/arcade/blob/master/static/fonts/xkcd.ttf
font_import(pattern="[X/x]kcd", prompt=FALSE)
loadfonts(device="win")
#Check if xkcd fonts loaded
fonts()
fonttable()

#Q2a) How much waste is being recovered? (in pounds)
total_recovery_onsite = sum(data2014$X8.2_ENERGY_RECOVERY_ON.SITE,na.rm=TRUE)
total_recovery_offsite = sum(data2014$X8.3_ENERGY_RECOVERY_OFF.SITE,na.rm=TRUE)
total_recovery = total_recovery_onsite + total_recovery_offsite

#Q2b) How much waste is being recycled?
total_recycling_offsite = sum(data2014$X8.5_RECYCLING_OFF.SITE,na.rm=TRUE)
total_recycling_onsite = sum(data2014$X8.4_RECYCLING_ON.SITE,na.rm=TRUE)
total_recycled = total_recycling_offsite + total_recycling_onsite

#Q2c) How much waste is being treated?
total_treated_onsite = sum(data2014$X8.6_TREATMENT_ON.SITE,na.rm=TRUE)
total_treated_offsite = sum(data2014$X8.7_TREATMENT_OFF.SITE,na.rm=TRUE)
total_treated = total_treated_onsite + total_treated_offsite

#Q2d) Are the industries moving towards better waste management?
#Find the total amount of waste being released in the year 2014
total_releases = sum(data2014$TOTAL_RELEASES,na.rm=TRUE)
#FCalculate the total amount of waste generated
total_waste = total_releases + total_recovery + total_recycled + total_treated

#To represent the waste management distribution we can use a Stacked Column Chart

#Make a data frame using the values obtained above
waste_mgmt_stats = data.frame(measure=c('recycled','recovered','treated','released'),value=c(total_recycled,total_recovery,total_treated,total_releases))
#Calculate the percentage share
waste_mgmt_stats$percent_share = (waste_mgmt_stats$value/total_waste)*100
waste_mgmt_stats = waste_mgmt_stats[c("measure","percent_share")]

#Plot the Stacked Column Chart using ggplot2
ggplot(melt(waste_mgmt_stats), aes(variable, value, fill = measure))
+ geom_bar(stat = "identity")
+ geom_text(position = "stack", aes(x = variable, y = value,label = value, hjust = 0.5, vjust=2.0))
+ xlab(label = "Measure")
+ ylab(label = "Percentage Share") 
+ ggtitle("Waste Management Distribution")
+ theme(text=element_text(family = "xkcd"))


#-----------Q3a) Most Polluting Industry-------------------

#Polluting_Industry is the variable storing the aggregated list wise data of pollution seggregated according to industries
Polluting_Industry=aggregate(data2014$TOTAL_RELEASES,list(data2014$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#******Commands for ggplot*********
ggplot(Polluting_Industry,aes(x=Polluting_Industry$Group.1,y=Polluting_Industry$x,fill=Polluting_Industry$Group.1,width=.50,binwidth=1.5))+geom_bar(position=position_dodge(),stat="identity",size =2)+ coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in pounds")+ggtitle("Industries and their Pollutant Emission")+theme(legend.position = "bottom",legend.background =element_rect(fill="gray90",size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))


#-----------Q3b) Which chemical compound contributes to the most amount of pollution----

#Polluting_Chemical is the variable storing the aggregated list wise data of pollution seggregated according to chemicals
Polluting_Chemical=aggregate(data2014$TOTAL_RELEASES,list(data2014$CHEMICAL),sum,na.rm=TRUE)
#new_Polluting_Chemical is the variable which stores the descending order of chemical values according to the quantity released
new_Polluting_Chemical=Polluting_Chemical[order(-Polluting_Chemical$x),]
#topmost_chemical is the variable used to store the top six highly polluting chemicals.
topmost_chemical =head(new_Polluting_Chemical)
#*******ggplot command******
ggplot(topmost_chemical,aes(x=topmost_chemical$Group.1,y=topmost_chemical$x,fill=topmost_chemical$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+geom_text(size=3.75,aes(label=x),position=position_dodge(width = 0.90),vjust=0.1)+coord_flip()+xlab("Chemical Compund Name")+ylab(" Quantity of Pollutants Released in pounds")+ggtitle("Top six Chemical compounds present in Industrial Effluents")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Chemical Compund Name"))


#------------Q3c)most carcinogenic compound producing industry------------

#Subsetting the data which only contains carceogenic compounds
carceogenic=data2014[which(data2014$CARCINOGEN=='YES'),]
#Carceogenic_industry is the variable storing the aggregated list wise data of industry producing carceogenic compounds
Carceogenic_industry=aggregate(carceogenic$TOTAL_RELEASES,list(carceogenic$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#********ggplot commands*******
ggplot(Carceogenic_industry,aes(x=Carceogenic_industry$Group.1,y=Carceogenic_industry$x,fill=Carceogenic_industry$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab(" Carcinogenic Pollutants Released in pounds")+ggtitle("Industries with Carcinogenic Pollutant Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))


#------------Q4)Which industry has highest stack emission and fugitive emission-------

#*-*-*-*-*-*-stack emission-*--*-*-*-*-*-*

#stack_emission is the variable which stores aggregated list of stack emission segregated basd on industries 
stack_emission=aggregate(data2014$X5.2_STACK_AIR,list(data2014$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#ggplot commands
ggplot(stack_emission,aes(x=stack_emission$Group.1,y=stack_emission$x,fill=stack_emission$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in pounds")+ggtitle("Industries with Stack Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))


#*-*-*-*-*-*-*-*Fugitive emission*-*-*-*-*-*-*-*-*-
fugitive_emission=aggregate(data2014$X5.1_FUGITIVE_AIR,list(data2014$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#******commands for ggplot*******
ggplot(fugitive_emission,aes(x=fugitive_emission$Group.1,y=fugitive_emission$x,fill=fugitive_emission$Group.1,width=.75,binwidth=1.25))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in Pounds")+ggtitle("Industries with Fugitive Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))
