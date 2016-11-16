#**************************************TEAM GRAND*************************************************

#Command to read data,data2014 is the variable used to store the dataset TRI_2014_US_Dataset.csv
data2014=read.csv(file.choose(),header=TRUE)

##########Question1##########

#Q1a) Total Pollution State-wise

#DESCRIPTIVE STATISTICS (provides minimun, maximum, mean,median,1st and 3rd Quartile and NA count)
#TOTAL_RELEASES has scale of measureent of type ratio
#Stats should include the range i.e. min, max and the mean 
summary(data2014$TOTAL_RELEASES)

#State wise data is aggregated based on total realeases(Pollution)
#state_aggregated_data is the variable which stores a statewise list of pollution emitted 
state_aggregated_data= aggregate(data2014$TOTAL_RELEASES,list(data2014$ST),sum)
#Statemax is the variable storing the data frame obtained from earlier command
statemax=do.call(data.frame,state_aggregated_data)
#Subsetting the data such that "statename" variable stores the subsetted data where the specific condition of x of "statemax" being maximum is met.
statename=statemax[which(statemax$x==max(statemax$x)),]
print(statename)

#Q1b) Most polluted county in each state

#DESCRIPTIVE STATISTICS for Alaska
#ON.SITE_RELEASE_TOTAL has scale of measureent of type ratio
#Stats should include the range i.e. min, max and the mean 
summary(alaska_subset$ON.SITE_RELEASE_TOTAL)

#subsetting data for state Alaska
alaska_subset=data2014[which(data2014$ST=='AK'),]
#akhighest is the variable with highest pollution in Alasksa
akhighest=max(alaska_subset$ON.SITE_RELEASE_TOTAL)
alaska_subset1=alaska_subset[which(alaska_subset$ON.SITE_RELEASE_TOTAL==akhighest),]
#county_of_alaska variable gives county name in Alaska with highest pollution
county_of_alaska=alaska_subset1$COUNTY
print(county_of_alaska)

##Similarly for every state the most polluted county can be identified

##########END : Question1##########


##########Question2##########

#DESCRIPTIVE STATISTICS
#All the variables have a scale of measurement of ratio [Refer Note1]
#Stats should include the range i.e. min, max and the mean 
summary(data2014$TOTAL_RELEASES)
summary(data2014$X8.2_ENERGY_RECOVERY_ON.SITE)
summary(data2014$X8.3_ENERGY_RECOVERY_OFF.SITE)
summary(data2014$X8.4_RECYCLING_ON.SITE)
summary(data2014$X8.5_RECYCLING_OFF.SITE)
summary(data2014$X8.6_TREATMENT_ON.SITE)
summary(data2014$X8.7_TREATMENT_OFF.SITE)

#Q2a) How much waste is being recovered? (in pounds)

total_recovery_onsite = sum(data2014$X8.2_ENERGY_RECOVERY_ON.SITE,na.rm=TRUE)
total_recovery_offsite = sum(data2014$X8.3_ENERGY_RECOVERY_OFF.SITE,na.rm=TRUE)
total_recovery = total_recovery_onsite + total_recovery_offsite
#Amount of waste being recycled (in pounds)
#Output variable: total_recovery
print(total_recovery) 

#Q2b) How much waste is being recycled?

total_recycling_offsite = sum(data2014$X8.5_RECYCLING_OFF.SITE,na.rm=TRUE)
total_recycling_onsite = sum(data2014$X8.4_RECYCLING_ON.SITE,na.rm=TRUE)
total_recycled = total_recycling_offsite + total_recycling_onsite
#Amount of waste being recycled (in pounds)
#Output variable: total_recycled
print(total_recycled) 

#Q2c) How much waste is being treated?

total_treated_onsite = sum(data2014$X8.6_TREATMENT_ON.SITE,na.rm=TRUE)
total_treated_offsite = sum(data2014$X8.7_TREATMENT_OFF.SITE,na.rm=TRUE)
total_treated = total_treated_onsite + total_treated_offsite
#Amount of waste being treated (in pounds)
#Output variable: total_treated
print(total_treated) 

#Q2d) Are the industries moving towards better waste management?

#Find the total amount of waste being released in the year 2014
total_releases = sum(data2014$TOTAL_RELEASES,na.rm=TRUE)
#FCalculate the total amount of waste generated
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
## value: 9.1222693313269 : 1, this ratio indicates that waste management is being done properly

##########END : Question2##########


##########Question3##########

#Q3a) Most Polluting Industry

#Polluting_Industry is the variable storing the aggregated list wise data of pollution seggregated according to industries
Polluting_Industry=aggregate(data2014$TOTAL_RELEASES,list(data2014$INDUSTRY_SECTOR),sum)
#Converting the data obtained from previous function into a data frame
most_Polluting_Industry=do.call(data.frame,Polluting_Industry)
#Industryname is the variable in which a subset data is stored which has only 1 entry that is of the highest pollution
Industryname=most_Polluting_Industry[which(most_Polluting_Industry$x==max(most_Polluting_Industry$x)),]
print(Industryname)

#**the same results can be ontained by ANOVA test
av=aov(data2014$TOTAL_RELEASES~data2014$INDUSTRY_SECTOR)
summary(av)
#Null Hypothesis: All industries have similar pollution rate
#Alternate hypothesis: Atleast one industry is significantly different in terms of pollution emitted.
#pair wise t test is done in order to find which particular industry in having highest pollution. Industries with pvalue less than 0.05 are the one with highest pollution
pairwise.t.test(data2014$TOTAL_RELEASES,data2014$INDUSTRY_SECTOR,p.adj="bonferroni")

#Q3b) Which chemical compound contributes to the most amount of pollution----

#DESCRIPTIVE STATISTICS
summary(most_Polluting_chemical$x)

#Polluting_Chemical is the variable storing the aggregated list wise data of pollution seggregated according to chemicals
Polluting_Chemical=aggregate(data2014$TOTAL_RELEASES,list(data2014$CHEMICAL),sum)
#Converting the data obtained from previous function into a data frame
most_Polluting_chemical=do.call(data.frame,Polluting_Chemical)
#chemical_name is the the variable in which a subset data is stored which has only 1 entry that is of the highest pollution
chemical_name=most_Polluting_chemical[which(most_Polluting_chemical$x==max(most_Polluting_chemical$x)),]
print(chemical_name)

#Q3c)most carcinogenic compound producing industry

#DESCRIPTIVE STATISTICS
summary(most_carceogenic_industry$x)

#Subsetting the data which only contains carceogenic compounds
carceogenic=data2014[which(data2014$CARCINOGEN=='YES'),]
#Carceogenic_industry is the variable storing the aggregated list wise data of industry producing carceogenic compounds
Carceogenic_industry=aggregate(carceogenic$TOTAL_RELEASES,list(carceogenic$INDUSTRY_SECTOR),sum)
#Converting to data frame
most_carceogenic_industry=do.call(data.frame,Carceogenic_industry)
#carceogenic_industry_name is the variable in which a subset data is stored which has only 1 entry that is of the highest pollutant which is carceogenic in nature
carceogenic_industry_name=most_carceogenic_industry[which(most_carceogenic_industry$x==max(most_carceogenic_industry$x)),]
print(carceogenic_industry_name)

##########END : Question3##########


##########Question4##########

#Q4)Which industry has highest stack emission and fugitive emission?
##Stack Emission

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


##Fugitive Emission
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

##########END : Question4##########


##Note1 
#All the variables used for calculation acrross all the 4 questions have a scale of measurement of type ratio
#As a result, the descriptive statistics will include the range [minValue,maxValue]
#The distribution of all these variables is not normal, hence we use median as a measure of central tendency rather than mean
#Since the mean isn't a measure of the central tendency, there is no need to calculate the Standard deviation, in case we want to the command is sd(dataset$column_name)