library(rio)

#convert the excel file into csv file for data frame creation and manipulation
convert("C:\\Users\\palla\\OneDrive\\Documents\\HealthSystemAnalysis\\sampleMeghalaya.xlsx","sampleMeghalaya.csv")
meghalayaMapdf <- read.csv("sampleMeghalaya.csv",header=T, sep = ",", na.strings = c("","NA"))

#Check if the daatset has NAs.
summary(meghalayaMapdf)
sum(is.na(meghalayaMapdf))

#rm(list = ls(all.names = TRUE))
#function definition to install packages
installPackage <- function(pckgeName)
  if(pckgeName %in% rownames(installed.packages()) == FALSE)
    install.packages(pckgeName)


library(geosphere)
meghalayaMapdfcp <- meghalayaMapdf


#meghalayaMapdfcp$dist <- distm(c())
set.seed(20)
phcClusters <- kmeans(meghalayaMapdf[,4:5],11)
#Saving the cluster number in district where phcs should be allocated
meghalayaMapdf$newDist <- phcClusters$cluster
str(phcClusters)
phcClusters$centers
#write to csv file
write.csv(meghalayaMapdf, "MegahlayaPHCsMap.csv", row.names = FALSE)
meghalayaMapdfcp2 <- meghalayaMapdf

installPackage("ggmap")

# Change the colname to District as SQL queries are not supported for names with dot as in District.Name
names(meghalayaMapdfcp2)[1]<- "District"
names(meghalayaMapdf)[1] <- "District"

library(sqldf)

clusterPoints <- sqldf("SELECT * FROM meghalayaMapdfcp2 WHERE newDist = 4 AND District != 'East Khasi Hills'")
#Query to get count of facilities Group by district.
phcDistb4Kmeans <- sqldf("SELECT District, newDist, COUNT(*) FROM meghalayaMapdfcp2 GROUP BY District")
phcDistafterKmeans <- sqldf("SELECT District, newDist, COUNT(*) FROM meghalayaMapdfcp2 GROUP BY newDist")
























