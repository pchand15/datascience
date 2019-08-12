library(plotly)
# library(dplyr)
# library(ggplot2)


df_2011 <- read.csv(file.choose(),sep=",", header = T)

#Check if any missing data is there
summary(df_2011)
which(is.na(df_2011)) 
sum(is.na(df_2011))

#Summary of the data
colnames(df_2011)
summary(df_2011$District.name)
summary(df_2011$Muslims)


#Plot the data on Religion and District name
library (sqldf)

religion_sql_df <- data.frame(city=df_2011$District.name, hindu=df_2011$Hindus, muslim=df_2011$Muslims, christian=df_2011$Christians, sikh=df_2011$Sikhs,
                              buddhist=df_2011$Buddhists, jain=df_2011$Jains, pop=df_2011$Population)
rowChennai <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Chennai'")
rowMumbai <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Mumbai'")
rowBangalore <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Bangalore'")
rowHyd <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Hyderabad'")
rowDelhi <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'New Delhi'")
rowGurgaon <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Gurgaon'")
rowKolkata <- sqldf("SELECT * FROM religion_sql_df WHERE city = 'Kolkata'")

single_df_metrocities <- rbind(rowChennai,rowMumbai,rowBangalore,rowHyd,rowDelhi,rowGurgaon,rowKolkata)
single_df_metrocities$city <- factor(c("Chennai","Mumbai","Hyderabad","Bangalore","Kolkata","New Delhi","Gurgaon"))

#Plot of the population vs metro cities
metrocityPopPlot <- ggplot(data = single_df_metrocities, aes(x=city, y=pop, fill=city),size=4) + geom_bar(stat="Identity")+ geom_text(aes(label=pop),size=4) + ggtitle("Metro city Population Data")+xlab("City")+ylab("Population")
HinduMuslimPopPlot <- ggplot(data = single_df_metrocities, aes(x=hindu, y=pop, fill=city),size=4) + geom_bar(stat="Identity")+ geom_text(aes(label=pop),size=4) + ggtitle("Metro city Population Data")+xlab("City")+ylab("Population")

#Grouped BarChart
single_df_metrocities$hindupopPCT <- round((single_df_metrocities$hindu/single_df_metrocities$pop)*100,digits = 0)
single_df_metrocities$muslimpopPCT <- round((single_df_metrocities$muslim/single_df_metrocities$pop)*100, digits = 0)

religionGroupPlot <-
  plot_ly(
    single_df_metrocities,
    x =  ~ city,
    y =  ~ hindupopPCT,
    type = 'bar',
    name = 'Hindus',
    text = single_df_metrocities$hindupopPCT,
    textposition = 'auto'
  ) %>% add_trace(y =  ~ muslimpopPCT, name = 'Muslims', text=single_df_metrocities$muslimpopPCT, textposition = 'auto') %>% layout(yaxis = list(title =
                                                                                   'Population in Percent'),
                                                                    barmode = 'group')
religionGroupPlot

dim(df_2011)
colnames(df_2011)

#rm(list=ls())
  
  
  
  
  
  
  
  
  
















