
library(shiny)

##########Create directory to store the files or copy the files###############

getwd()

if(!dir.exists("etc")){
  dir.create("etc")
}
sourcedir <- "C:/Users/palla/personal/read/R101/India_Demographic_data/2001/"
destdir <- "C:/Users/palla/OneDrive/Documents/IndianSocietyStudies/"
listExcelFiles <- list.files(sourcedir,"*2001.xls")
#file.copy(listExcelFiles, getwd())

library(rio)

# Covert .xls files to .csv using file operation and string operation
for (i in listExcelFiles) {
  remFileExt <- substr(i,1,nchar(i)-4)
  newFileName <- paste(remFileExt,".csv",sep="")
  print(newFileName)
 convert(paste(sourcedir,"/",i, sep=""),paste(sourcedir,"/",newFileName, sep="") )
}

#Copy all the .csv files from sourcedir to the current working directory
listCSVfiles <- list.files(sourcedir,"*2001.csv")
for (i in listCSVfiles) {
  print(i)
  file.copy(paste(sourcedir,"/",i,sep=""), destdir)
}

#Check if all the files are in copied successfully in working directory
getwd()
list.files()


###########################Understanding dataset################################

#read in all the csv files
all_dfs <- lapply(list.files(pattern=".csv"),read.csv)

#stack all the data frames together
single_df <- Reduce(rbind, all_dfs)

# Data cleaning and manipulation
View(single_df)
# if("gmodels" %in% rownames(installed.packages()) == FALSE)
#   install.packages("gmodels")
# library(gmodels)
head(single_df)
colnames(single_df)
table(single_df$Area.Name,single_df$Total.Households.With...Male.Head)

#Handling missing values by getting indeces of "NA"
# naIndex <- which(is.na(single_df))
# dfAfterNARemove <- single_df[-c(naIndex),]
df.without.NA <- single_df[complete.cases(single_df),]
View(df.without.NA)
nrow(single_df)
ncol(single_df)
nrow(df.without.NA)
ncol(df.without.NA)

#Group based on household size grouping 1, 2, 3-6, 7-10, 11+ Mean Household Size Total, Extract the one with level : Total
total.pop.ind <- which(df.without.NA$Household.Size=="Total")
final.df <- df.without.NA[c(total.pop.ind),]
View(final.df)
dataset.struct(final.df)

#################Common function to understand the dataset - count the number of rows, cols and display col names#################

dataset.struct <- function(df.var){
  row.cnt <- nrow(df.var)
  col.cnt <- ncol(df.var)
  col.names <- colnames(df.var)
 # view.df - View(final.df)
  df.struct.list <- c("NoofRows"=row.cnt,"NoofCols"=col.cnt,col.names)
  return(df.struct.list)
}


###################SQL Query for Data Manipulation - Descriptive Analysis######################

library(sqldf)

#Query to count total population based on Religion - Rural and Urban.
#Remove the LEVEL: "TOTAL" from the var: "Total.Rural.urban"
ind.with.tot <- which(single_df$Household.Size == "Total")
df.with.hs.tot <- single_df[c(ind.with.tot),]
View(df.with.hs.tot)

test.data <- single_df[(single_df$Household.Size == "Total")& (single_df$Total..Rural..Urban=="URBAN"), ]
View(test.data)

library(ggplot2)

# religious.plot <-
#   ggplot(
#     data = test.data,
#     aes(
#       x = test.data$Area.Name,
#       y = test.data$Hindu...Male.Head,
#       fill = test.data$Area.Name,
#       label = test.data$Hindu...Male.Head
#     )
#   ) + geom_bar(stat = "identity") + ggtitle("Religious Pop of Urban States") +
#   xlab("Urban States") + ylab("Hindu Population")
# religious.plot
# 
# religious.plot <-
#   religious.plot + geom_text(
#     data = test.data,
#     aes(
#       x = test.data$Area.Name,
#       y = test.data$Hindu...Male.Head,
#       label = test.data$Hindu...Male.Head
#     ),
#     size = 4
#   )
# religious.plot <- religious.plot + labs(fill="States")
# help("ggplot2")

test.data1 <- as.data.frame(test.data)
View(test.data1)

test.data1$Area.Name <-
  factor(
    test.data1$Area.Name,
    levels = c(
      "State - ANDHRA PRADESH  (28)",
      "State - DELHI  (07)",
      "State - KARNATAKA  (29)",
      "State - KERALA  (32)",
      "State - MAHARASHTRA  (27)",
      "State - TAMIL NADU  (33)",
      "State - WEST BENGAL  (19)"
    ),
    labels = c(
      "ANDHRA PRADESH",
      "DELHI",
      "KARNATAKA",
      "KERALA",
      "MAHARASHTRA",
      "TAMIL NADU",
      "WEST BENGAL"
    )
  )

#Hindu Population in Indian States
religion.plot <-
  ggplot(
    data = test.data1,
    aes(
      x = test.data1$Area.Name,
      y = test.data1$Hindu...Male.Head,
      fill = test.data1$Area.Name,
      label = test.data1$Hindu...Male.Head
    )
  ) + geom_bar(stat = "Identity") + ggtitle("Religious Pop of Urban States") +
  xlab("Urban States") + ylab("Hindu Population")

religion.plot <- religion.plot + geom_text(
  data = test.data1,
  aes(
    x = test.data1$Area.Name,
    y = test.data1$Hindu...Male.Head,
    label = test.data1$Hindu...Male.Head
  ),
  size = 4
)

religion.plot <- religion.plot + labs(fill="States")
religion.plot

#Muslim Population in Indian(Urban) States
religion.plot <-
  ggplot(
    data = test.data1,
    aes(
      x = test.data1$Area.Name,
      y = test.data1$Muslim...Male.Head,
      fill = test.data1$Area.Name,
      label = test.data1$Muslim...Male.Head
    )
  ) + geom_bar(stat = "Identity") + ggtitle("Religious Pop of Urban States") +
  xlab("Urban States") + ylab("Hindu Population")

religion.plot <- religion.plot + geom_text(
  data = test.data1,
  aes(
    x = test.data1$Area.Name,
    y = test.data1$Muslim...Male.Head,
    label = test.data1$Muslim...Male.Head
  ),
  size = 4
)

religion.plot <- religion.plot + labs(fill="States")
religion.plot

#Calculate in terms of % of the total population; total population = sum(all pops of religions)
dataset.struct(test.data1)
test.data1$totalUrbanPopulation <-
  test.data1$Total.Households.With...Male.Head + test.data1$Total.Households.With...Female.Head
test.data1$totalHinduPopulation <- test.data1$Hindu...Female.Head+test.data1$Hindu...Male.Head
test.data1$totalMuslimPopulation <- test.data1$Muslim...Female.Head+test.data1$Muslim...Male.Head
test.data1$totalChristianPopulation <- test.data1$Christian...Female.Head+test.data1$Christian...Male.Head

test.data1$totalHinduPCT <- round((test.data1$totalHinduPopulation/test.data1$totalUrbanPopulation)*100) 
test.data1$totalMuslimPCT <- round((test.data1$totalMuslimPopulation/test.data1$totalUrbanPopulation)*100)
test.data1$totalChristianPCT <- round((test.data1$totalChristianPopulation/test.data1$totalUrbanPopulation)*100)


#Graph in percentage(%)

religion.plot <-
  ggplot(
    data = test.data1,
    aes(
      x = test.data1$Area.Name,
      y = test.data1$totalHinduPCT,
      fill = test.data1$Area.Name,
      label = test.data1$totalHinduPCT
    )
  ) + geom_bar(stat = "Identity") + ggtitle("Religious Pop of Urban States-Hindu(%)") +
  xlab("Urban States") + ylab("Hindu Population in %")

religion.plot <- religion.plot + geom_text(
  data = test.data1,
  aes(
    x = test.data1$Area.Name,
    y = test.data1$totalHinduPCT,
    label = test.data1$totalHinduPCT
  ),
  size = 4
)

#Get the muslim population in %
religion.plot <- religion.plot + labs(fill="States")
religion.plot

religion.plot <-
  ggplot(
    data = test.data1,
    aes(
      x = test.data1$Area.Name,
      y = test.data1$totalMuslimPCT,
      fill = test.data1$Area.Name,
      label = test.data1$totalMuslimPCT
    )
  ) + geom_bar(stat = "Identity") + ggtitle("Religious Pop of Urban States-Muslim(%)") +
  xlab("Urban States") + ylab("Muslim Population in %")

religion.plot <- religion.plot + geom_text(
  data = test.data1,
  aes(
    x = test.data1$Area.Name,
    y = test.data1$totalMuslimPCT,
    label = test.data1$totalMuslimPCT
  ),
  size = 4
)

religion.plot <- religion.plot + labs(fill="States")
religion.plot

#Get Christian population in %
religion.plot <- religion.plot + labs(fill="States")
religion.plot

religion.plot <-
  ggplot(
    data = test.data1,
    aes(
      x = test.data1$Area.Name,
      y = test.data1$totalChristianPCT,
      fill = test.data1$Area.Name,
      label = test.data1$totalChristianPCT
    )
  ) + geom_bar(stat = "Identity") + ggtitle("Religious Pop of Urban States-Christian(%)") +
  xlab("Urban States") + ylab("Christian Population in %")

religion.plot <- religion.plot + geom_text(
  data = test.data1,
  aes(
    x = test.data1$Area.Name,
    y = test.data1$totalChristianPCT,
    label = test.data1$totalChristianPCT
  ),
  size = 4
)

religion.plot <- religion.plot + labs(fill="States")
religion.plot


test.data1$totalChristianPopulation
sampledf = data.frame(
  "A" = test.data1$Area.Name,
  "Total" = test.data1$totalUrbanPopulation,
  "H" = test.data1$totalHinduPopulation,
  "M" = test.data1$totalMuslimPopulation,
  "C" = test.data1$totalChristianPopulation,
  "H%" = test.data1$totalHinduPCT,
  "M%" = test.data1$totalMuslimPCT,
  "C%" = test.data1$totalChristianPCT
)












# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

