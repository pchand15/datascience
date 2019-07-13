#Create directory that contains the file.

if("etc" %in% getwd()==FALSE){
  dir.create("etc")
}
#check if the directory has been created
list.files(getwd())

fname= list.files("etc")

#Read the csv file and create an intial data frame.
initdf = read.csv("etc\\District_wise_Health_Centres_March_2016_area.csv", header=T)
View(initdf)
colnamesset = colnames(initdf)

#change the colnames of the dataset
newcolname = c("ID","States_UTs","Deathrate","medicalandpublichealthexp","SCs","PHCs","CHCs","SubDivHosps","DistHosps","hwdensity","docdensity","fliteracyrate","oopexp","pcinc","socialstrata","area")
colnames(initdf) = newcolname

#####################################################################################################################################
# DATA CLEANING TO GET THE FINAL DATA FRAME FOR ANALYSIS                                                                            #
# OMIT NAS AND FIND OUTIERS, EXCLUDE OUTLIERS, CHANGE THE EXPENDITURE CALCULATION IN RS. 000 IF REQUIRED                            #
#####################################################################################################################################
UTindset = c(which(initdf$States_UTs=="Chandigarh"), which(initdf$States_UTs=="Dadra & Nagar Haveli"), which(initdf$States_UTs=="Daman & Diu"),
           which(initdf$States_UTs=="A & N Island"),which(initdf$States_UTs=="Lakshadweep"),which(initdf$States_UTs=="Puducherry"),which(initdf$States_UTs=="Telangana"))



#Subtract the indices that contain the Union Terrotories to make the final set #6  8  9  1 19 27
noUTdf = initdf[-UTindset,]
moddf = na.omit(noUTdf)
View(moddf)

#Total PHC centers
moddf$phc = moddf$SCs+moddf$PHCs

#per sq. km how many primary health centers are there
moddf$phcareawise = moddf$area/moddf$phc
which(colnames(moddf)=="phc")
#Model formation based on investment measure.. A linear combination model
lmod = lm(formula= Deathrate ~ hwdensity, data=moddf)
summary(lmod)

lmoddoc = lm(formula= Deathrate ~ docdensity, data=moddf)
summary(lmoddoc)

which(colnames(moddf)=="phcareawise")
#Scatterplot Matrix to understand the data points and co-relation

# if("car" %in% rownames(installed.packages())==FALSE){
#   install.packages("car")
# }

par("mar")
par(mar=c(0.1,0.1,0.1,0.1))
plot(moddf$hwdensity, moddf$Deathrate, main="Scatterplot OOP expenditure", 
     xlab="OOP Expense", ylab="DeathRate", pch=19)
# Add fit lines
abline(lm(Deathrate~hwdensity, data=moddf), col="red") # regression line (y~x) 
lines(lowess(moddf$hwdensity,moddf$Deathrate), col="blue") # lowess line (x,y)

if("gclus" %in% rownames(installed.packages()) == FALSE){
  install.packages("gclus")
} 

library(gclus)
colnames(moddf)
dta <- moddf[c(3,4,5,6,10,11,12,13,14,15,18)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#LOG Transformation of death rate -Y variable
moddf$Deathratelog = log(moddf$Deathrate)
moddf$PHCslog = log(moddf$PHCs)
moddf$SCslog = log(moddf$SCs)
moddf$healthexp = log(moddf$medicalandpublichealthexp)
which(colnames(moddf)=="SCslog")
which(colnames(moddf)=="PHCslog")
#newdt = moddf[c(3,4,5,6,10,11,12,13,14,15,16,17,18,19)]
newdt = moddf[c(3,10,16)]
newdt.r = abs(cor(newdt))
newdt.col = dmat.color(newdt.r)
newdt.o = order.single(newdt.r)
cpairs(newdt, newdt.o, panel.colors = newdt.col,gap=.5,main="Variables Ordered and Colored by Correlation")

#IMP model
logmod = lm(formula= Deathrate ~ phcareawise + hwdensity, data=moddf)
summary(logmod)

par("mar")
par(mar=c(0.1,0.1,0.1,0.1))
plot(moddf$hwdensity, moddf$Deathrate, main="Scatterplot OOP expenditure", 
     xlab="OOP Expense", ylab="DeathRate", pch=19)
# Add fit lines
abline(lm(Deathrate~hwdensity, data=moddf), col="red") # regression line (y~x) 
lines(lowess(moddf$hwdensity,moddf$Deathrate), col="blue") # lowess line (x,y)

moddf$hwdenslog = log(moddf$hwdensity)

library(tidyverse)
ggplot(data=mergeddf, mapping=aes(x=mergeddf$hwdensity, y=mergeddf$drdensity))+
  geom_point()+labs(x="Health Worker Density", y="Death Rate", title="Scatterplot for health worker density on death rate")+
  geom_smooth()



library(cplm)

#Z-transformation
moddf$hwdens = scale(moddf$hwdenslog)
moddf$noofSCs = scale(moddf$SCslog)
moddf$noofPHCs = scale(moddf$PHCslog)
moddf$DR = scale(moddf$Deathratelog)
moddf$hcexp = scale(moddf$medicalandpublichealthexp)
moddf$flr = scale(moddf$fliteracyrate) 
moddf$docdens = scale(log(moddf$docdensity))
moddf$phcareareawise = scale(asin(sqrt(moddf$phcareawise/100)))
moddf$inc = scale(moddf$pcinc)
moddf$oopexpraw = (moddf$pcinc*moddf$oopexp)/100
moddf$oop = scale(moddf$oopexpraw)

moddf = moddf[-2,]

moddf$expperkm2 <- moddf$medicalandpublichealthexp/moddf$area
moddf$govtexpperkm2 <- scale(moddf$expperkm2)

write.csv(moddf, file="etc\\ModifiedHCData.csv")
popdf = read.csv("etc\\PopulationData.csv", header=T)

mergeddf <- merge(moddf,popdf,by="States_UTs")


mergeddf$flitrate <- (mergeddf$fliteracyrate*mergeddf$Female.Population)/100
mergeddf$literacyrate <- scale(mergeddf$flitrate)
mergeddf$expperperson <- mergeddf$medicalandpublichealthexp/mergeddf$Population
mergeddf$perareaspend <- mergeddf$medicalandpublichealthexp/mergeddf$area
mergeddf$expenditure <- scale(mergeddf$expperperson)
mergeddf$drdensity <- mergeddf$Deathrate*mergeddf$Population.Density.km2.
mergeddf$scaspend <- scale(mergeddf$perareaspend)
mergeddf$expperpersonscale <-  scale(mergeddf$expperperson)
mergeddf$drdensityscale <- scale(mergeddf$drdensity)

#Write the final created dataset into a csv file and store it in the etc folder
write.csv(mergeddf, file="etc\\FinalIHCData.csv")

#Descriptive anaysis of all the raw data variables 
summary(mergeddf)
#Get the list of variable names
colnames(mergeddf)
#Get population density in order to find the states with sparsely populated area.
mergeddf$Population.Density.km2.
#State/s with number of SCs =147 alongwith population,area, dealth rate and expenditure of the state.
View(mergeddf)
library(sqldf)
sql_df = data.frame(States_UTs=mergeddf$States_UTs, HExp= mergeddf$medicalandpublichealthexp, MR= mergeddf$Deathrate, Pop=mergeddf$Population, Area=mergeddf$area,
                    OutofPocket=mergeddf$oopexpraw, HW = mergeddf$hwdensity, FemLit=mergeddf$fliteracyrate, PerCapInc=mergeddf$pcinc, NumofSCs = mergeddf$SCs,
                    NumofPHCs=mergeddf$PHCs, PopDens=mergeddf$Population.Density.km2.)

#Query to fetch the State's health care investment and demographic data that has the minimum number of Sub-centers available.
sqldf("SELECT States_UTs, HExp, Pop, Area, OutofPocket, FemLit, PerCapInc, HW, MR from sql_df where NumofSCs=147")
#Query to fetch the State's health care investment and demographic data that has the minimum number of Primary Health Care Centers available.
sqldf("SELECT States_UTs, HExp, Pop, Area, OutofPocket, FemLit, PerCapInc, HW, MR from sql_df where NumofPHCs=24")

#Query to fetch the State's health care investment and demographic data where the number of Sub-centers are less than mean.  
sqldf("SELECT States_UTs, HExp, Pop, Area, OutofPocket, FemLit, PerCapInc,NumofSCs, HW, MR from sql_df where NumofSCs <= 5972 and NumofSCs>147 order by Pop")

#Query to fetch the State's health care investment and demographic data based on population density
sqldf("SELECT States_UTs, HExp, PopDens, Area, OutofPocket, FemLit, PerCapInc,NumofSCs, HW, MR from sql_df where NumofSCs <= 5972 and NumofSCs>147 order by Pop")



lmod1 <- lm(formula= DR ~ hwdens + phcareareawise+oop, data = moddf)
summary(lmod1)


#IMP model
#with scaled data
lmod1 <- lm(formula= hwdens ~  phcareareawise + oop, data = mergeddf)
summary(lmod1)

#IMP model
lmod1 <- lm(formula= hwdensity ~  phcareawise + oopexpraw, data = mergeddf)
summary(lmod1)

#IMP model
lmod1 <- lm(formula= drdensity ~  phcareawise + perareaspend, data = mergeddf)
summary(lmod1)

lmod1 <- lm(formula= drdensity ~  phcareawise + oopexpraw + hwdensity, data = mergeddf)
summary(lmod1)


mergeddf$hwdensity

predictval <- predict(lmod1, type="response")

hwpreddf <- data.frame(moddf$States_UTs,predictval,moddf$hwdens)

#IMP model
cpmod<-cpglm(formula = drdensityscale+10 ~ phcareareawise + oop + scaspend, data = mergeddf)
summary(cpmod)
preddf = data.frame(cpmod$y, moddf$States_UTs, moddf$Deathrate)

mean(moddf$Deathrate)








