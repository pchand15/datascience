#This project deals with investment that should probably be done to continue the Farm to school(F2S) program and the observed investment done for assessment year 2013-14 and 2014-15

#Uncomment if conversion to .csv format is required
# library(rio)
# convert("C:\\Users\\palla\\personal\\courses\\selection\\Summer18\\PAF502\\Group_Project\\2015_F2S_Census_State.xlsx", "etc\\F2S_Census_State.csv")
# convert("C:\\Users\\palla\\personal\\courses\\selection\\Summer18\\PAF502\\Group_Project\\State_CCD_Estimates.xlsx", "etc\\State_CCD_Estimates.csv")
# convert("C:\\Users\\palla\\personal\\courses\\selection\\Summer18\\PAF502\\Group_Project\\State_Dollar_Estimates.xlsx", "etc\\State_Dollar_Estimates.csv")

f2sdf = read.csv("etc\\F2S_Census_State.csv", header = TRUE)
#View(f2sdf)

spenddf = read.csv("etc\\State_Dollar_Estimates.csv", header = TRUE)
#View(spenddf)

ccdest <- read.csv("etc\\State_CCD_Estimates.csv", header = TRUE)
#View(ccdest)
#Eliminate the GU, Hawaii(HI) and PR states as financial data is not available for the same.
f2sspenddf <- merge(f2sdf, spenddf, by = "state")
View(f2sspenddf)
str(f2sspenddf)

print(paste0(f2sspenddf$state, " ", f2sspenddf$totcost))
#variables to include for regression analysis: totcost
#total spending on F2S done

sum(f2sspenddf$totcost) # 6859584955 USD
#total number of schools in a state

sum(f2sspenddf$countf2s2013) #total number of districts that had farm to school activities in SY 2013-2014
sum(f2sspenddf$countf2s2014) #total number of districts that had farm to school activities in SY 2014-2015
f2sspenddf$countf2sfuture #Plan to start activities in the future
f2sspenddf$countf2snoplan # Had no farm to school activities and no plans
f2sfin$totdist <- f2sfin$countf2s2013 + f2sfin$countf2s2014 # total number of districts:5235
sum(f2sfin$totdist) 

str(f2sfin$REGIONAL_FNS_CNT)

#specifying the regions as string
f2sfin$Region <- as.character(f2sfin$REGIONAL_FNS_CNT)
MidAtlantic <- c(which(f2sfin$REGIONAL_FNS_CNT == 1))
f2sfin$Region[MidAtlantic] <- "MidAtlantic"
MountPlains <- c(which(f2sfin$REGIONAL_FNS_CNT == 2))
f2sfin$Region[MountPlains] <- "Mountain Plains"
MidWest  <- c(which(f2sfin$REGIONAL_FNS_CNT == 3))
f2sfin$Region[MidWest] <- "Midwest"
NorthEast <- c(which(f2sfin$REGIONAL_FNS_CNT == 4))
f2sfin$Region[NorthEast] <- "Northeast"
SouthEast <- c(which(f2sfin$REGIONAL_FNS_CNT == 5))
f2sfin$Region[SouthEast] <- "Southeast"
SouthWest <- c(which(f2sfin$REGIONAL_FNS_CNT == 6))
f2sfin$Region[SouthWest] <- "Southwest"
Western <-  c(which(f2sfin$REGIONAL_FNS_CNT == 7))
f2sfin$Region[Western] <- "Western"

#local farm produce will be affected based on region.
f2sfin$Region #regions column specified
f2sfin$Region <-
  as.factor(f2sfin$Region) #create it into factor to act like a continuous predictor variable
str(f2sfin$Region)



  #************Biases and assumptions*****************
  #data frame containing the contf2sschools and countf2sstudents columns. This will eliminate the states that doesn't have total number of schools and students count
  f2sfin <- merge(f2sspenddf, ccdest, by = "state")
#View(f2sfin)

#display total number of f2s schools after elimination of the states that doesn't have the total count of schools and total count of students
f2sfin$countf2s2013
f2sfin$countf2s2014
f2sfin$countf2sfuture
f2sfin$countf2snoplan
f2sfin$countf2sschools
f2sfin$countf2sstudents
f2sfin$districts #total number of districts that participated in the f2s census.
f2sfin$responses #total number of districts responded to f2s census


#**********Biases and assumptions: problems causing obstacle in the implementation of f2s for the year 2013*****************
f2sfin$meanctf2sprob <- f2sfin$countprobbid2013+
f2sfin$countprobseas2013+
f2sfin$countprobcoord2013+
f2sfin$countprobavail2013+
f2sfin$countprobrange2013+
f2sfin$countprobhigh2013+
f2sfin$countprobunstable2013+
f2sfin$countprobreliab2013+
f2sfin$countprobequip2013+
f2sfin$countprobgap2013+
f2sfin$countprobcompl2013+
f2sfin$countprobproc2013+
f2sfin$countprobsupplier2013+
f2sfin$countprobinfo2013+  #hard to get information about the
f2sfin$countproborder2013+ #problem with placing orders with vendors
f2sfin$countprobtime2013+ #problem with getting on-time deliveries
f2sfin$countprobqual2013+ # problem with quality fo products that are delivered
f2sfin$countprobdeliv2013+ # problem in quantity ordered not equal to quantity delivered
f2sfin$countprobresolv2013+ # problem in resolving problem deliveries
f2sfin$countprobpay2013 # count of responding f2s districts that recorded inability to pay farmers
#calculate avg becoz the count can be intersecting. Therefore, to avoid double-read calculate avg based on mean values.
#get the avg number of districts in each state facing the problem in operating f2s. This too will be a deciding factor in local food demand.
f2sfin$avgctprob <- f2sfin$meanctf2sprob/20
sum(f2sfin$avgctprob) # Avg. districts will be higher than the count: 817 out of total 4716 districts(for year 2013-14) are having issues in operating f2s.
#however, biases for milk, poultry and fisheries can not be excluded as the data is insufficient and does not specify if the problem is with local agri produce delivery or for fisheries.

f2sfin$distsuccess <- f2sfin$countf2s2013 - f2sfin$avgctprob 
sum(round(f2sfin$distsuccess, digits = 0)) # Totat disctricts that have been successful are 3880 out of 4716 without any hicups.

#total number of local food supplied/purchased


f2sfin$servfood <- f2sfin$countserv + f2sfin$countsmrtsnack + f2sfin$countaftrschool + f2sfin$countsrvg
f2sfin$avgsrvg <- round(f2sfin$servfood/4, digits = 0)
sum(f2sfin$avgsrvg)  # 1640 avg districts are being served local food out of 4716.
  
sum(f2sfin$countserv)
sum(f2sfin$countsmrtsnack)
sum(f2sfin$countaftrschool)
sum(f2sfin$countsrvg)


f2sfin$nonresp <-
  f2sfin$districts - f2sfin$responses #no of districts invited for f2s census but did not respond.
sum(f2sfin$nonresp)
sum(f2sfin$districts)
sum(f2sfin$responses)




#total number of districts that are invited for f2s but are not part of f2s 2013-2014 and 2014-2015
f2sfin$notf2s = f2sfin$districts - (f2sfin$countf2s2013 + f2sfin$countf2s2014) #misleading as the census is conducted for 2013-14. Therefore take teh response rate.
sum(f2sfin$notf2s)
sum(f2sfin$districts)
resprt85 <- c(which(f2sfin$responserate > 0.85)) # response rate is one of the measure for viewing which region has maximum response rate measured at 85 % -for f2s continuation.
highrespstate <- data.frame(f2sfin$Region[resprt85], f2sfin$state[resprt85], f2sfin$totdolexclmilk[resprt85], f2sfin$responserate[resprt85])
colnames(highrespstate) <- c("Region", "State", "Total cost of food excluding milk(USD)", "Census Response Rate(districts)")

#total amount spent on the farm to school
f2sfin$totcost #reported cost of food of districts doing F2S 2013-14
f2sfin$totdolinclmilk #estimated dollars spent on local food including milk in 2013-14
f2sfin$totdolexclmilk # estimated dollars spent on local food excluding milk in 2013-14
f2sfin$PCT_NEWCOSTMILK #mean food budget spent on local food including fluid milk
f2sfin$PCT_NEWCOSTNOMILK #mean food budget spent on local food excluding fluid milk

#out of these variables we will exclude the milk to take into account only the agricultural produce
f2sfin$totdolexclmilk
f2sfin$totcost

#local food obtained
f2sfin$locfood <- f2sfin$countdirectprod+f2sfin$countdirectcoop+
f2sfin$countdirectfmkt+
f2sfin$countdirectcsa+
f2sfin$countdirectmanu+
f2sfin$countinterdist+
f2sfin$countintercoop+
f2sfin$countinterfood+
f2sfin$countinterfsmc+
f2sfin$countinterdod+
f2sfin$countinterdod+
f2sfin$countinterusda# include USDA food to as the total reported cost is not being measured, just the local food.

f2sfin$avgobtained <- f2sfin$locfood/12
f2sfin$avgobtained <- round(f2sfin$avgobtained, digits = 0)

#local food count based on local food purchase by the responding districts

#Assumptions and biases in local food count
f2sfin$countvegprotyes
f2sfin$countvegprotfuture
f2sfin$countgrainyes
f2sfin$countherbyes
f2sfin$countfruityes
f2sfin$countvegyes

#Since count is misleading calculate the mean of total districts based on local food types

f2sfin$locfoodspec <- f2sfin$countvegprotyes +
  f2sfin$countvegprotfuture +
  f2sfin$countgrainyes +
  f2sfin$countherbyes +
  f2sfin$countfruityes +
  f2sfin$countvegyes
#total avg districts that used defined six types of local food.
f2sfin$meandistlocfood <- f2sfin$locfoodspec / 6


#Z-standarize the values:
f2sfin$savgctprob <- scale(round(f2sfin$avgctprob, digits = 0))  #round the digits before scaling as count can not be float
f2sfin$sspend <- scale(f2sfin$totdolexclmilk)
f2sfin$success <- scale(round(f2sfin$distsuccess, digits = 0))
f2sfin$ssrvg <- scale(round(f2sfin$avgsrvg, digits = 0))
f2sfin$responserate<- scale(f2sfin$responserate)
f2sfin$savgobtained <- scale(f2sfin$avgobtained)

#Remove all the NAs thus VI state will be eliminated.
na.omit(f2sfin) #omit all NAs


#********************Model Selection**************************

f2smod1 <- lm(meandistlocfood ~ ssrvg, data=f2sfin)
#Residuals vs. fitted values
plot(f2smod1, boxplot=T, silent=F)
summary(f2smod1)
predict(f2smod1)

mod1df <- data.frame(Region=f2sfin$Region, State=f2sfin$state, Observed_Value1314=f2sfin$meandistlocfood, Predicted_Value1314=predict(f2smod1))
mod1df
#model including region
f2smod2 <- lm(meandistlocfood ~ savgctprob +sspend+success+ssrvg+responserate+savgobtained+Region, data=f2sfin)
summary(f2smod2)
predict(f2smod2)
f2sfin$meandistlocfood
mod2df <- data.frame(Observed_Value1314=f2sfin$meandistlocfood, Predicted_Value1314=predict(f2smod2, data=f2sfin))

#model without including region-specific
f2smod3 <- lm(meandistlocfood ~ savgctprob +sspend+success+ssrvg+responserate+savgobtained, data=f2sfin)
summary(mod3)
options(scipen=999)
summary(f2smod3)
coef(summary(f2smod3))
mod3pred <- data.frame(Predicted_Value=predict(f2smod3))#predicted value should match the observed values of 2013-14
str(mod3pred)
predict(f2smod3)

plot(
  f2sfin$sspend,
  f2sfin$meandistlocfood,
  pch = 16,
  cex = 1.3,
  col = "blue",
  main = "Effect of spending on local food purchases",
  xlab = "Total cost($)",
  ylab = "districts using local food"
)
abline(lm(meandistlocfood ~ sspend, data = f2sfin))
plot(
  f2sfin$sspend,
  f2sfin$meandistlocfood,
  pch = 16,
  cex = 1.3,
  col = "green",
  main = "Effect of serving locally food on local food purchases",
  xlab = "Total served food(scaled)",
  ylab = "districts using local food"
)
abline(lm(meandistlocfood ~ ssrvg, data = f2sfin))#how much is brought is based on how much is served.

#*********************DEAD CODE- To be removed******************

# locfoodct <-
#   data.frame(
#     f2sfin$Region,
#     f2sfin$state,
#     f2sfin$districts,
#     f2sfin$responses,
#     f2sfin$totdist,
#     f2sfin$countf2s2013,
#     f2sfin$countf2s2014,
#     f2sfin$countfruityes,
#     f2sfin$countvegyes,
#     f2sfin$countvegprotyes,
#     f2sfin$countgrainyes,
#     f2sfin$countherbyes,
#     f2sfin$meanlocfood,
#     f2sfin$totdolexclmilk,
#     f2sfin$totcost
#   )
# 
# colnames(locfoodct) = c(
#   "Region",
#   "State",
#   "Districts",
#   "Responses",
#   "f2sfin$totdist",
#   "No. of F2S Districts t-1",
#   "No. of F2S Districts t-1",
#   "Local Fruit Purchased",
#   "Local veg Purchased",
#   "Protein",
#   "Local Grains",
#   "Local herbs",
#   "Mean value of Local Food",
#   "Actual Spending",
#   "Reported Cost of food"
# )
# locfoodct
# 
# #Remove all the NAs thus VI state will be eliminated.
# na.omit(locfoodct)
# 
# #count of districts that will be
# 
# fit = lm(meandistlocfood ~ totdolexclmilk + Region + responses, data = f2sfin)
# summary(fit)
# 
# mfit = lm(meandistlocfood ~ totdolexclmilk + Region + responses + countf2s2013 ,
#           data = f2sfin)
# summary(mfit)
# 
# lfit = lm(meandistlocfood ~ totdolexclmilk + Region, data = f2sfin)
# summary(lfit)


#state eliminated due to insufficient data
#GU - no data for total number of schools and total number of students

#difference between the food purchased and food suplied.
