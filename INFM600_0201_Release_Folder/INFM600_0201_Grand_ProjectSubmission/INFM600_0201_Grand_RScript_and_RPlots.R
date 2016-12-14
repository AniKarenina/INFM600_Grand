#**************************************TEAM GRAND*************************************************

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
state_aggregated_data= aggregate(data2014$TOTAL_RELEASES,list(data2014$ST),sum)

#-----DESCRIPTIVE STATISTICS provides minimun, maximum, mean,median,1st and 3rd Quartile
summary(data2014$TOTAL_RELEASES)

#Statemax is the variable storing the data frame obtained from earlier command
statemax=do.call(data.frame,state_aggregated_data)
#Subsetting the data such that "statename" variable stores the subsetted data where the specific condition of x of "statemax" being maximum is met.
statename=statemax[which(statemax$x==max(statemax$x)),]
print(statename)


#plots
#******ggplot commands with xkcd packages********

ggplot(state_aggregated_data,aes(x=state_aggregated_data$Group.1,y=state_aggregated_data$x,fill=state_aggregated_data$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+xlab("States of US")+ylab("Pollutants Released in pounds")+ggtitle("State wise Pollutant Emission")+theme(legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="States of US"))+theme_xkcd()

#******ggplot commands with light theme packages******
ggplot(state_aggregated_data,aes(x=state_aggregated_data$Group.1,y=state_aggregated_data$x,fill=state_aggregated_data$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+xlab("States of US")+ylab("Pollutants Released in pounds")+ggtitle("State wise Pollutant Emission")+theme(legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="States of US"))+theme_light()

#--------Q1b) Most polluted county in each state ------
state_names = unique(data2014$ST)
i=0
state_list = c()
county_list = c()
value_list = c()
for(state in state_names){ 
  i = i + 1
  state_data_subset = data2014[which(data2014$ST==state),]
  subset_hihgest_value = max(state_data_subset$ON.SITE_RELEASE_TOTAL)
  matching_row = state_data_subset[which(state_data_subset$ON.SITE_RELEASE_TOTAL==subset_hihgest_value),]
  county_name = matching_row$COUNTY
  string_s = paste(state,"|",county_name,"|",subset_hihgest_value)
  count_name_text = as.character(unlist(strsplit(string_s,"[|]"))[2])
  state_list = c(state_list,state)
  county_list = c(county_list,count_name_text)
  value_list = c(value_list,subset_hihgest_value)
}
state_county_max_polluted = data.frame(state_list,county_list,value_list)
colnames(state_county_max_polluted) = c("ST","COUNTY","TOTAL_RELEASES")
state_county_max_polluted$sc = paste(state_county_max_polluted$COUNTY,"[",state_county_max_polluted$ST,"]",sep = "")

#PLOTS
ggplot(state_county_max_polluted,aes(x=reorder(sc,-TOTAL_RELEASES),y=TOTAL_RELEASES)) + geom_bar(stat="identity") + xlab("STATE WISE HIGHEST COUNTY") + ylab("TOTAL RELEASE in lbs") + ggtitle("Counties with highest pollution") + theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#-----------Question2--------

#DESCRIPTIVE STATISTICS
summary(data2014$TOTAL_RELEASES)
summary(data2014$X8.2_ENERGY_RECOVERY_ON.SITE)
summary(data2014$X8.3_ENERGY_RECOVERY_OFF.SITE)
summary(data2014$X8.4_RECYCLING_ON.SITE)
summary(data2014$X8.5_RECYCLING_OFF.SITE)
summary(data2014$X8.6_TREATMENT_ON.SITE)
summary(data2014$X8.7_TREATMENT_OFF.SITE)

#-----Q2a)How much waste is being recovered? (in pounds)----
total_recovery_onsite = sum(data2014$X8.2_ENERGY_RECOVERY_ON.SITE,na.rm=TRUE)
total_recovery_offsite = sum(data2014$X8.3_ENERGY_RECOVERY_OFF.SITE,na.rm=TRUE)
total_recovery = total_recovery_onsite + total_recovery_offsite

#Amount of waste being recycled (in pounds)
#Output variable: total_recovery
print(total_recovery) 

#------Q2b)How much waste is being recycled? (in pounds)-----
total_recycling_offsite = sum(data2014$X8.5_RECYCLING_OFF.SITE,na.rm=TRUE)
total_recycling_onsite = sum(data2014$X8.4_RECYCLING_ON.SITE,na.rm=TRUE)
total_recycled = total_recycling_offsite + total_recycling_onsite
#Amount of waste being recycled (in pounds)
#Output variable: total_recycled
print(total_recycled) 


#------Q2c) How much waste is being treated? (in pounds)-----
total_treated_onsite = sum(data2014$X8.6_TREATMENT_ON.SITE,na.rm=TRUE)
total_treated_offsite = sum(data2014$X8.7_TREATMENT_OFF.SITE,na.rm=TRUE)
total_treated = total_treated_onsite + total_treated_offsite
#Amount of waste being treated (in pounds)
#Output variable: total_treated
print(total_treated) 


#------Q2d) Are the industries moving towards better waste management? (in pounds)
#Find the total amount of waste being released in the year 2014
total_releases = sum(data2014$TOTAL_RELEASES,na.rm=TRUE)
#Calculate the total amount of waste generated
total_waste = total_releases + total_recovery + total_recycled + total_treated
#Calculate the percentage share of each 
per_recycled = (total_recycled/total_waste)*100
per_recovered = (total_recovery/total_waste)*100
per_treated = (total_treated/total_waste)*100
per_released = (total_releases/total_waste)*100
#Percentage of waste being recycled 
#Output variable: per_recycled
print(per_recycled) 
#Percentage of waste being recovered 
#Output variable: per_recovered
print(per_recovered) 
#Percentage of waste being recycled 
#Output variable: per_treated
print(per_treated) 
#Percentage of waste being released
#Output variable: per_released
print(per_released)
#On the basis of these value, we conclude that the industries are 
##recycling 46% of the waste
##recovering 10% of the waste
##treating 33% of the waste
##releasing 9.87% of waste
per_waste_managed = per_recovered + per_recycled + per_treated
##total of 90.1% waste is being managed
paste(per_waste_managed/per_released," : ",1) 
## value: 9.1222693313269 : 1, this ratio indicates that waste management is being done in good proportio

##PLOTS*****
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
Polluting_Industry=aggregate(data2014$TOTAL_RELEASES,list(data2014$INDUSTRY_SECTOR),sum)
#Converting the data obtained from previous function into a data frame
most_Polluting_Industry=do.call(data.frame,Polluting_Industry)
#Industryname is the variable in which a subset data is stored which has only 1 entry that is of the highest pollution
Industryname=most_Polluting_Industry[which(most_Polluting_Industry$x==max(most_Polluting_Industry$x)),]
print(Industryname)

#**the same results can be obtained by ANOVA test
av=aov(data2014$TOTAL_RELEASES~data2014$INDUSTRY_SECTOR)
summary(av)
#Null Hypothesis: All industries have similar pollution rate
#Alternate hypothesis: Atleast one industry is significantly different in terms of pollution emitted.
#pair wise t test is done in order to find which particular industry in having highest pollution. Industries with pvalue less than 0.05 are the one with highest pollution
pairwise.t.test(data2014$TOTAL_RELEASES,data2014$INDUSTRY_SECTOR,p.adj="bonferroni")

#******Commands for ggplot*********
ggplot(Polluting_Industry,aes(x=Polluting_Industry$Group.1,y=Polluting_Industry$x,fill=Polluting_Industry$Group.1,width=.50,binwidth=1.5))+geom_bar(position=position_dodge(),stat="identity",size =2)+ coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in pounds")+ggtitle("Industries and their Pollutant Emission")+theme(legend.position = "bottom",legend.background =element_rect(fill="gray90",size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))



#-----------Q3b) Which chemical compound contributes to the most amount of pollution----

#Polluting_Chemical is the variable storing the aggregated list wise data of pollution seggregated according to chemicals
Polluting_Chemical=aggregate(data2014$TOTAL_RELEASES,list(data2014$CHEMICAL),sum)
#Converting the data obtained from previous function into a data frame
most_Polluting_chemical=do.call(data.frame,Polluting_Chemical)
#chemical_name is the the variable in which a subset data is stored which has only 1 entry that is of the highest pollution
chemical_name=most_Polluting_chemical[which(most_Polluting_chemical$x==max(most_Polluting_chemical$x)),]
print(chemical_name)


#DESCRIPTIVE STATISTICS
summary(most_Polluting_chemical$x)


#PLOTS*****
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
Carceogenic_industry=aggregate(carceogenic$TOTAL_RELEASES,list(carceogenic$INDUSTRY_SECTOR),sum)
#Converting to data frame
most_carceogenic_industry=do.call(data.frame,Carceogenic_industry)
#carceogenic_industry_name is the variable in which a subset data is stored which has only 1 entry that is of the highest pollutant which is carceogenic in nature
carceogenic_industry_name=most_carceogenic_industry[which(most_carceogenic_industry$x==max(most_carceogenic_industry$x)),]
print(carceogenic_industry_name)

#Descriptive statistics
summary(most_carceogenic_industry$x)


#PLOTS******
#********ggplot commands*******
ggplot(Carceogenic_industry,aes(x=Carceogenic_industry$Group.1,y=Carceogenic_industry$x,fill=Carceogenic_industry$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab(" Carcinogenic Pollutants Released in pounds")+ggtitle("Industries with Carcinogenic Pollutant Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))




#------------Q4)Which industry has highest stack emission and fugitive emission-------

#*-*-*-*-*-*-stack emission-*--*-*-*-*-*-*

#stack_emission is the variable which stores aggregated list of stack emission segregated basd on industries 
stack_emission=aggregate(data2014$X5.2_STACK_AIR,list(data2014$INDUSTRY_SECTOR),sum)
#Converting to data frame
high_stack_emission=do.call(data.frame,stack_emission)
#high_stack_industry_name is the variable in which a subset data is stored which has only 1 entry that is of the highest stack emission industry
high_stack_industry_name=high_stack_emission[which(high_stack_emission$x==max(high_stack_emission$x)),]
print(high_stack_industry_name)

#**Same operation can be performed in Anova test**
#av2 is the variable used for storing the anova test results for stack air releases.
#Null Htpothesis : All the industries have similar amount of stack air releases
#Alternate Hypothesis: Atleast one industry's stack air release is different from the other

av2=aov(data2014$X5.2_STACK_AIR~data2014$INDUSTRY_SECTOR ,data=data2014)
summary(av2)
#this test gives output in terms of which industries have significantly differnt release from others.p value less than 0.05 means that particular industry has different stack releases
pairwise.t.test(data2014$X5.2_STACK_AIR,data2014$INDUSTRY_SECTOR,p.adj="bonferroni")

#DESCRIPTIVE STATISTICS
summary(high_stack_emission$x)


#*-*-*-*-*-*-*-*Fugitive emission*-*-*-*-*-*-*-*-*-
fugitive_emission=aggregate(data2014$X5.1_FUGITIVE_AIR,list(data2014$INDUSTRY_SECTOR),sum)
#Converting to data frame
high_fugitive_emission=do.call(data.frame,fugitive_emission)
#high_fugitive_industry_name is the variable in which a subset data is stored which has only 1 entry that is of the highest fugitive emission industry
high_fugitive_industry_name=high_fugitive_emission[which(high_fugitive_emission$x==max(high_fugitive_emission$x)),]
print(high_fugitive_industry_name)

#**Same operation can be performed in Anova test**
#av3 is the variable used for storing the anova test results for Fugitive air releases.
#Null Htpothesis : All the industries have similar amount of fugitive air releases
#Alternate Hypothesis: Atleast one industry's fugitive air release is different from the other

av3=aov(data2014$X5.1_FUGITIVE_AIR~data2014$INDUSTRY_SECTOR ,data=data2014)
summary(av3)
#this test gives output in terms of which industries have significantly differnt release from others.p value less than 0.05 means that particular industry has different stack releases
pairwise.t.test(data2014$X5.1_FUGITIVE_AIR,data2014$INDUSTRY_SECTOR,p.adj="bonferroni")
#DESCRIPTIVE STATISTICS
summary(high_fugitive_emission$x)


#PLOTS*****
#*-*-*-*-*-*-stack emission-*--*-*-*-*-*-*

#stack_emission is the variable which stores aggregated list of stack emission segregated basd on industries 
stack_emission=aggregate(data2014$X5.2_STACK_AIR,list(data2014$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#ggplot commands
ggplot(stack_emission,aes(x=stack_emission$Group.1,y=stack_emission$x,fill=stack_emission$Group.1,width=.75,binwidth=0.75))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in pounds")+ggtitle("Industries with Stack Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))


#*-*-*-*-*-*-*-*Fugitive emission*-*-*-*-*-*-*-*-*-
fugitive_emission=aggregate(data2014$X5.1_FUGITIVE_AIR,list(data2014$INDUSTRY_SECTOR),sum,na.rm=TRUE)
#******commands for ggplot*******
ggplot(fugitive_emission,aes(x=fugitive_emission$Group.1,y=fugitive_emission$x,fill=fugitive_emission$Group.1,width=.75,binwidth=1.25))+geom_bar(position=position_dodge(),stat="identity",size =2)+coord_flip()+xlab("Industry Type")+ylab("Pollutants Released in Pounds")+ggtitle("Industries with Fugitive Emission")+theme(legend.position = "bottom",legend.background = element_rect(fill="gray90", size=.01),legend.text=element_text(size=6.5))+guides(fill=guide_legend(keywidth=0.40,keyheight=0.40,default.unit="cm",title="Industry Type"))
