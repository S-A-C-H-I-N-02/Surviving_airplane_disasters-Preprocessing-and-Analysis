# Libraries to import

library ("readr")
library("tidyverse")
library("lubridate")
library("rapportools")
library("ggplot2")
library("repr")
library("RColorBrewer")
library("factoextra")
library("gridExtra")
library("cluster")
library("plyr")
library("dplyr")
library("DT")
#Load the Data

AirCrash=read.csv("/content/sample_data/Airplane_Crashes_and_Fatalities_Since_1908.csv")
head(AirCrash,10)
#Structure of the Dataset
str(AirCrash)
#Data Inspection & Cleaning!

# Check for duplicated data
print(paste('Total duplicated rows: ',nrow(AirCrash[duplicated(AirCrash),])))

# Check for data for NA fields
print(paste("Total AirCrash NA's: ",sum(is.na(AirCrash))))

# List Null Values
Null_Values <- (sapply(AirCrash,function(x) sum(is.na(x))))
t(data.frame(Null_Values))
#Inspect planes with missing Aboard values

AirCrash[is.empty(AirCrash$Aboard),c(1,4,5,7,10,11,12)]
# Keep all records where Aboard values are not NA
AirCrash <- AirCrash[!is.empty(AirCrash$Aboard),]
# Convert Ground NA values to 0
AirCrash$Ground[is.na(AirCrash$Ground)] <- 0
# Check dimensions to make sure 22 records have been dropped (5268-24 = 5244)
dim(AirCrash)
# Confirm there are no NA values in data
print(paste("Total NA values: ",sum(is.na(AirCrash))))
# Check for empty spaces "" as input fields
Missing_Values <- (sapply(AirCrash,function(x) sum(x=="")))
(data.frame(Missing_Values))
# Convert Date field data type from factor to date.
AirCrash$Date <- as.Date(AirCrash$Date, format = "%m/%d/%Y")
# Convert Time field data type from factor to time
AirCrash$LocalTime <- as.POSIXct(AirCrash$Time, format = "%H:%M")

# check if any format changes created NA values
print(paste("Total Date NA's: ",sum(is.na(AirCrash$Date))))
print(paste("Total LocalTime NA's: ",sum(is.na(AirCrash$LocalTime))))
# Add column LocalHour represented as a 24 hour clock in numeric format.
AirCrash$LocalHour <- as.numeric(format(AirCrash$LocalTime,"%H"))

# Temporarily replace Local Hour NA's with 25 to enable cut function
AirCrash$LocalHour <- ifelse(is.na(AirCrash$LocalHour), 25, AirCrash$LocalHour)
# Add discretized dayparts based on Local Hour
AirCrash$Daypart <- cut(AirCrash$LocalHour, breaks = c(-1,5,11,17,24,25),
                              labels = c("Overnight", "Morning","Afternoon","Night", "Unknown"))
#Reset NA's in Local Hour
AirCrash$LocalHour <- ifelse(AirCrash$LocalHour == 25,NA,AirCrash$LocalHour)

# Check first 15 records to see if bins are correctly labeled
head(AirCrash[,15:16],15)
# Check for NA values
print(paste("Total LocalHour NA's: ",sum(is.na(AirCrash$LocalHour))))
print(paste("Total Daypart NA's: ", sum(is.na(AirCrash$Daypart))))
# lubridate package was utilized to extract Year and Month from Date as their own columns
AirCrash$Year <- (year(AirCrash$Date))
AirCrash$Month <- (month(ymd(AirCrash$Date), label = TRUE))
# check column and check for NA's
head(AirCrash[c("Date","Year", "Month")],3)
print(paste("Total Year NA's: ", sum(is.na(AirCrash$Year))))
print(paste("Total Month NA's: ", sum(is.na(AirCrash$Month))))
# Adding Survivors
AirCrash$Survivors <- AirCrash$Aboard - AirCrash$Fatalities
# Adding Survival Rate
AirCrash$SurvivalRate <- AirCrash$Survivors/AirCrash$Aboard

str(AirCrash)
#Summary output provided for full list of stats.
summary(AirCrash)
# Create function to format chart size
fg <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}

# Create consistent chart theme for font sizes
ATheme <- theme(title = element_text(size = 24, face = 'bold'),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 16),
               legend.text = element_text(size = 16))

# Plot number of Airplane Crashes Per Year
fg(16,10)
CrashesPerYear = ggplot(AirCrash, aes(x=Year)) + geom_bar (colour = "mediumorchid4") + 
  xlab("Year") + ylab("Airplane Crashes") + ggtitle("Airplane Crashes Per Year") + ATheme
CrashesPerYear
# Aggregate the Travelers Aboard, Survivors and Fatalities by Year
AC <- rbind(Survivors = aggregate(AirCrash$Survivors,by=list(AirCrash$Year),FUN =sum),
            Aboard = aggregate(AirCrash$Aboard,by=list(AirCrash$Year),FUN =sum),
            Fatalities = aggregate(AirCrash$Fatalities, by = list(AirCrash$Year), FUN = sum))

# Some formatting and cleaning for charting.
AC$Travelers <- rownames(AC)
AC$Travelers <- gsub("[.].*","",AC$Travelers)
AC <- AC %>% dplyr::rename(Year = Group.1, Count = x)
# Plot the number of Travelers Aboard, Survivors and Fatalities by Year
fg(16,10)
TravelersPerYear = ggplot(AC, aes(x=Year, y=Count, group = Travelers )) +
                   geom_line(aes(colour=Travelers))  + geom_point(size = 0.2) +
                    scale_colour_brewer (palette = "Dark2", labels = c("Aboard", "Fatalities", "Survivors"))+
                    xlab("Year") + ylab("Travelers") + ATheme +
                    ggtitle("Travelers Aboard Airplane Crashes with Survivors and Fatalities Per Year")

TravelersPerYear
# Aggregate Survival Rate per year for charting
AC2 <- cbind(Survivors = aggregate(AirCrash$Survivors,by=list(AirCrash$Year),FUN =sum),
            Aboard = aggregate(AirCrash$Aboard,by=list(AirCrash$Year),FUN =sum))

fg(16,10)
SurvivorsPerYear = ggplot(AC2, aes(x=Survivors.Group.1, y=Survivors.x/Aboard.x)) +  
    geom_col(colour = "darkolivegreen4") + 
    xlab("Year") + ylab("Survival Rate as Percent") + 
    ggtitle("Survival Rate of Airplane Crashes Per Year") + ATheme +
    scale_y_continuous(labels = scales::percent)
SurvivorsPerYear
# Preserve all Air Crashes in data set in AllAirCrash (in case needed later)
AllAirCrash <- AirCrash
# Print number of records with blank summaries
print(paste("Number of records with blank entries: ", nrow(AirCrash[AirCrash$Summary == "",])))
# Remove AirCrash data with blank summaries
AirCrash <- AirCrash[!AirCrash$Summary == "",]
# Inspect if removal was successful
print("After removing blank Summary records...")
print(paste("Number of Summary records with blank entries: ",nrow(AirCrash[AirCrash$Summary == "",])))
# Prep a data frame "AirClust" to contain the variables to be used for k-means clustering.

# Extract the key variables from AirCrash into a data frame AirClust.
AirClustx <- AirCrash[,c(10,11,19,20)] 
# Create binominal values by score years in AirScore
AirScore <- data.frame(Year = AirCrash$Year)
AirScore$Y1908_Y1929 <- ifelse(AirScore$Year > 1929,0,1)
AirScore$Y1930_Y1949 <- ifelse(between(AirScore$Year,1930,1949), 1,0 )
AirScore$Y1950_Y1969 <- ifelse(between(AirScore$Year,1950,1969), 1,0 )
AirScore$Y1970_Y1989 <- ifelse(between(AirScore$Year,1970,1989), 1,0 )
AirScore$Y1990_Y2009 <- ifelse(AirScore$Year > 1989 ,1,0)
# Bind AirScore binomial variables to AirClust
AirClust <- data.frame(AirClustx,AirScore[,-1])
head(AirClust)
# Initial test of clusters with k = (3, 4, 5, 6)
set.seed(23)
k1 <- kmeans(AirClust, centers = 3, nstart = 25)
k2 <- kmeans(AirClust, centers = 4, nstart = 25)
k3 <- kmeans(AirClust, centers = 5, nstart = 25)
k4 <- kmeans(AirClust, centers = 6, nstart = 25)


# Visualize the cluster results
p1 <- fviz_cluster(k1, geom = "point", data = AirClust) + ggtitle("K means k=3")
p2 <- fviz_cluster(k2, geom = "point", data = AirClust) + ggtitle("K means k=4")
p3 <- fviz_cluster(k3, geom = "point", data = AirClust) + ggtitle("K means k=5")
p4 <- fviz_cluster(k4, geom = "point", data = AirClust) + ggtitle("K means k=6")

grid.arrange(p1,p2,p3,p4)
# Plot Elbo Method
fviz_nbclust(AirClust, kmeans, method = "wss") + 
geom_vline(xintercept = 3, linetype = 2) + ATheme
# Applying the resulting 3 clusters to airplane crash variables.
AirCrash <- data.frame(AirCrash,Cluster = k1$cluster)
#Aggregating the mean values by Aboard, Fatalities, Survivors and Survival Rate.
# Minimum and Maximum aggregations were also made for Aboard.
AboardClust <- aggregate(AirCrash$Aboard, by=list(Cluster = AirCrash$Cluster),FUN = mean)
AboardClustx <- aggregate(AirCrash$Aboard, by=list(Cluster = AirCrash$Cluster),FUN = max)
AboardClustm <- aggregate(AirCrash$Aboard, by=list(Cluster = AirCrash$Cluster),FUN = min)
DeathClust <- aggregate(AirCrash$Fatalities, by=list(AirCrash$Cluster), FUN = mean)
SurviveClust <- aggregate(AirCrash$Survivors, by = list(AirCrash$Cluster), FUN = mean)
SRateClust <- aggregate(AirCrash$SurvivalRate, by = list(AirCrash$Cluster), FUN = mean)

# Binding the aggregated values into a data frame.
PCluster <- data.frame(cbind(Cluster =AboardClust$Cluster,
                             Plane_Crashes = k1$size,Max_Aboard = AboardClustx$x,
                             Min_Aboard =AboardClustm$x, Mean_Aboard = AboardClust$x,
                             Mean_Fatalities = DeathClust$x, Mean_Survivors = SurviveClust$x, 
                             Mean_SurvivalRate = SRateClust$x))

PCluster
# Adding the Cluster names of MidSizeCrash, LargeSurvivors, LargeFatalities and SmallCrash as ClustID
AirCrash$ClustID <- ifelse(AirCrash$Cluster == 1, "Large Passenger High Survival", 
                    ifelse(AirCrash$Cluster == 2, "Large Passenger High Fatality", "Small-Midsize Crashes"))
# Plane Crashes Per Year by ClusterID
K1CrashPerYear <-  ggplot(AirCrash, aes(x=Year, fill = ClustID)) + geom_bar() + 
  xlab("Year") + ylab("Plane Crashes") + ggtitle("Plane Crashes Per Year by Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("brown","darkolivegreen3","steelblue4"))) + ATheme

K1CrashPerYear
# Aggregate Travelers Aboard Crashes per Year by Cluster groups
Aboard <- aggregate(AirCrash$Aboard,by=list(AirCrash$Year,AirCrash$ClustID),FUN =sum)
Aboard <- Aboard %>% dplyr::rename(Year = Group.1, ClusterID = Group.2, Travelers = x)

# Plot Travelers Aboard per Year by Cluster
fg(16,10)
K1TravelersPerYear = ggplot(Aboard, aes(x=Year, y=Travelers, group = ClusterID)) +
                   geom_line(aes(colour=ClusterID), size = 1.5)  + geom_point(size = 0.5) +
                    scale_color_manual( "Cluster", values = alpha(c("brown","darkolivegreen3","steelblue4")))+
                    xlab("Year") + ylab("Travelers") + ATheme +
                    ggtitle("Travelers Aboard Airplane Crashes Per Year by Cluster")

K1TravelersPerYear
# Aggregate Airplane Crash Fatalities per Year by Cluster groups
Fatalities <- aggregate(AirCrash$Fatalities,by=list(AirCrash$Year,AirCrash$ClustID),FUN =sum)
Fatalities <- Fatalities %>% dplyr::rename(Year = Group.1, ClusterID = Group.2, Deaths = x)

# Plot Travelers Aboard per Year by Cluster
fg(16,10)
K1FatalitiesPerYear = ggplot(Fatalities, aes(x=Year, y=Deaths, group = ClusterID )) +
                   geom_line(aes(colour=ClusterID), size = 1.5)  + geom_point(size = 0.5) +
                    scale_color_manual( "Cluster", values = alpha(c("brown","darkolivegreen3","steelblue4")))+
                    xlab("Year") + ylab("Fatalities") + ATheme +
                    ggtitle("Airplane Crash Fatalities Per Year by Cluster")

K1FatalitiesPerYear
# Aggregate Survivors and Aboard per Year by Cluster group
Survivors <- aggregate(AirCrash$Survivors,by=list(AirCrash$Year,AirCrash$ClustID),FUN =sum)
Survivors <- Survivors %>% dplyr::rename(Year = Group.1, ClusterID = Group.2, Survivors = x)
AC2 <- merge(Survivors, Aboard, by = c("Year","ClusterID"))


# Plot the Percent of Survivors per Year by Cluster
fg(16,10)
K1SurvivalRatePerYear = ggplot(AC2, aes(x=Year, y=Survivors/Travelers, group = ClusterID )) +
                   geom_line(aes(colour=ClusterID), size = 1.5)  + geom_point(size = 0.5) +
                    scale_color_manual("Cluster", values = alpha(c("brown","darkolivegreen3","steelblue4")))+
                    xlab("Year") + ylab("Percent") + ATheme +   scale_y_continuous(labels = scales::percent)+
                    ggtitle("Percent of Airplane Crash Survivors Per Year by Cluster")

K1SurvivalRatePerYear
# Move the Large Plane data into 2 data frames representing each large plane groups
LgSurvive <- AirCrash[AirCrash$ClustID == "Large Passenger High Survival",]
LgFatal <- AirCrash[AirCrash$ClustID == "Large Passenger High Fatality",]

# Take the top 10 crashes by plane model for each group
Top10TypeSurvive <- top_n(dplyr::count(LgSurvive,Type, sort = TRUE), 10)
Top10TypeFatal <- top_n(dplyr::count(LgFatal, Type, sort = TRUE), 10)

# Take the top 10 crashes by operator for each group
Top10OperSurvive <- top_n(dplyr::count(LgSurvive,Operator, sort = TRUE), 10)
Top10OperFatal <- top_n(dplyr::count(LgFatal,Operator, sort = TRUE), 10)
fg(16,12)
# Plot the top ten crashes by plane model for each large plane cluster group
TypeSurvive <- ggplot(Top10TypeSurvive, aes(x=reorder(Type,n), y= n))+ geom_col(fill = "darkgreen") + 
                coord_flip() + ggtitle("Top Ten Large Passenger High Survival Crashes by Model") + 
               xlab("Plane Model") + ylab("Number of Crashes") + ATheme

TypeFatal <- ggplot(Top10TypeFatal, aes(x=reorder(Type,n), y= n))+ geom_col(fill = "brown") + 
                coord_flip() + ggtitle("Top Ten Large Passenger High Fatality Crashes by Model") + 
               xlab("Plane Model") + ylab("Number of Crashes") + ATheme

grid.arrange(TypeSurvive, TypeFatal)
fg(16,12)
# Plot the top ten crashes by plane model for each large plane cluster group
OperSurvive <- ggplot(Top10OperSurvive, aes(x=reorder(Operator,n), y= n))+ geom_col(fill = "darkgreen") + 
                coord_flip() + ggtitle("Top Ten Large Passenger High Survival Crashes by Airline") + 
               xlab("Operator") + ylab("Number of Crashes") + ATheme

OperFatal <- ggplot(Top10OperFatal, aes(x=reorder(Operator,n), y= n))+ geom_col(fill = "brown") + 
                coord_flip() + ggtitle("Top Ten Large Passenger High Fatality Crashes by Airline") + 
               xlab("Operator") + ylab("Number of Crashes") + ATheme

grid.arrange(OperSurvive, OperFatal)
LgAirCrash <- AirCrash[!AirCrash$ClustID == "Small-Midsize Crashes",]

fg(16,12)
# Plot Plane Crashes Per Month by ClusterID
K1CrashPerMonth <-  ggplot(LgAirCrash, aes(x=Month, fill = ClustID)) + geom_bar() + 
  xlab("Month") + ylab("Plane Crashes") + ggtitle("Plane Crashes Per Month by Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("brown","darkolivegreen4"))) + ATheme

K1CrashPercentMonth <-  ggplot(LgAirCrash, aes(x=Month, fill = ClustID)) + geom_bar(position = 'fill') + 
  xlab("Month") + ylab("Percent") + ggtitle("Percent of Plane Crashes Per Month by Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("darkred","darkgreen"))) + ATheme +
  scale_y_continuous(labels = scales::percent)

grid.arrange(K1CrashPerMonth, K1CrashPercentMonth)
# Plot Plane Crashes Per Daypart by ClusterID
K1CrashPerDaypart <-  ggplot(LgAirCrash, aes(x=Daypart, fill = ClustID)) + geom_bar() + 
  xlab("Daypart") + ylab("Number of Crashes") + ggtitle("Large Plane Crashes by Daypart and Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("brown","darkolivegreen4"))) + ATheme 

K1CrashPercentDaypart <-  ggplot(LgAirCrash, aes(x=Daypart, fill = ClustID)) + geom_bar(position = "fill") + 
  xlab("Daypart") + ylab("Percent") + ggtitle("Percent of Large Plane Crashes by Daypart and Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("darkred","darkgreen"))) + ATheme +
  scale_y_continuous(labels = scales::percent)

grid.arrange(K1CrashPerDaypart, K1CrashPercentDaypart)

LgAirCrash$LocalHour <- as.factor(LgAirCrash$LocalHour)

# Plot Plane Crashes Per Local Hour by ClusterID
K1CrashPerLocalHour <-  ggplot(LgAirCrash, aes(x=LocalHour, fill = ClustID)) + geom_bar() + 
  xlab("Local Hour") + ylab("Number of Crashes") + ggtitle("Large Plane Crashes by Local Hour and Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("brown","darkolivegreen4"))) + ATheme 

K1CrashPercentLocalHour <-  ggplot(LgAirCrash, aes(x=LocalHour, fill = ClustID)) + geom_bar(position = 'fill') + 
  xlab("Local Hour") + ylab("Percent") + ggtitle("Percent of Large Plane Crashes by Local Hour and Cluster") +
  scale_fill_manual("Cluster", values = alpha(c("darkred","darkgreen"))) + ATheme +
  scale_y_continuous(labels = scales::percent)

grid.arrange(K1CrashPerLocalHour, K1CrashPercentLocalHour)
