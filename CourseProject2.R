
setwd("D:/Users/TM35460/Desktop/reproducible-research")  

data <- read.csv(bzfile("tmp.bz2"), 
                 header=TRUE,
                 sep=",",
                 stringsAsFactors=FALSE)

str(data)

# Change parameter names to lowercase.
colnames(data) <- tolower(colnames(data))

# Subset on the parameters of interest.
data <- subset(x=data, 
               subset=(evtype != "?" & 
                         (injuries > 0 | fatalities > 0 | propdmg > 0 | cropdmg > 0)),
               select=c("evtype", 
                        "fatalities", 
                        "injuries", 
                        "propdmg", 
                        "propdmgexp", 
                        "cropdmg", 
                        "cropdmgexp"))    


# Change all damage exponents to uppercase.
data$propdmgexp <- toupper(data$propdmgexp)
data$cropdmgexp <- toupper(data$cropdmgexp)

# Map property damage alphanumeric exponents to numeric values.
propDmgKey <-  c("\"\"" = 10^0,
                 "-" = 10^0, 
                 "+" = 10^0,
                 "0" = 10^0,
                 "1" = 10^1,
                 "2" = 10^2,
                 "3" = 10^3,
                 "4" = 10^4,
                 "5" = 10^5,
                 "6" = 10^6,
                 "7" = 10^7,
                 "8" = 10^8,
                 "9" = 10^9,
                 "H" = 10^2,
                 "K" = 10^3,
                 "M" = 10^6,
                 "B" = 10^9)
data$propdmgexp <- propDmgKey[as.character(data$propdmgexp)]
data$propdmgexp[is.na(data$propdmgexp)] <- 10^0

# Map crop damage alphanumeric exponents to numeric values
cropDmgKey <-  c("\"\"" = 10^0,
                 "?" = 10^0, 
                 "0" = 10^0,
                 "K" = 10^3,
                 "M" = 10^6,
                 "B" = 10^9)
data$cropdmgexp <- cropDmgKey[as.character(data$cropdmgexp)]
data$cropdmgexp[is.na(data$cropdmgexp)] <- 10^0

# Aggregate number of fatalities and injuries per evtype into healthData dataframe
healthData <- aggregate(cbind(fatalities, injuries) ~ evtype, data=data, FUN=sum)
# Add total column to healthData
healthData$total <- healthData$fatalities + healthData$injuries


# Remove rows with zero health impact
healthData <- healthData[healthData$total > 0, ]
# Sort health data in descending order
healthData <- healthData[order(healthData$total, decreasing=TRUE), ]
# Re-label the rows
rownames(healthData) <- 1:nrow(healthData)
# Create dataframe of highest health impacting event types and append an "other" event type as a catchall 
# for everything else
healthDataTop <- healthData[1:10, ]

# Combine propdmg and propdmgexp parameters into a single parameter called propertyloss.
data$propertyloss <- data$propdmg * data$propdmgexp
# Combine cropdmg and cropdmgexp parameters into a single parameter called croploss.
data$croploss <- data$cropdmg * data$cropdmgexp
Select the applicable economic columns from the dataset, then calculate the total amount of property loss and crop loss per event type.

# Aggregate amount of proploss and croploss per evtype into economicData dataframe
economicData <- aggregate(cbind(propertyloss, croploss) ~ evtype, data=data, FUN=sum)
# Add total loss column to economicData
economicData$total <- economicData$propertyloss + economicData$croploss
Find the event types corresponding with the highest economic impacts.

# Remove rows with zero economic impact
economicData <- economicData[economicData$total > 0, ]
# Sort the economy data in descending order
economicData <- economicData[order(economicData$total, decreasing=TRUE), ]
# Re-label the rows
rownames(economicData) <- tolower(rownames(economicData))
# Create dataframe of highest economy impacting event types
economicDataTop <- economicData[1:10, ]


# Load necessary libraries
# Load necessary libraries
library(reshape2)
library(ggplot2)

# Melt the data
healthDataTopMelt <- melt(healthDataTop, id.vars="evtype")

# Create chart
healthChart <- ggplot(healthDataTopMelt, aes(x=reorder(evtype, -value), y=value))
# Plot data as bar chart
healthChart = healthChart + geom_bar(stat="identity", aes(fill=variable), position="dodge")
# Format y-axis scale and set y-axis label
healthChart = healthChart + scale_y_sqrt("Frequency Count") 
# Set x-axis label
healthChart = healthChart + xlab("Event Type") 
# Rotate x-axis tick labels 
healthChart = healthChart + theme(axis.text.x = element_text(angle=45, hjust=1))
# Set chart title
healthChart = healthChart + ggtitle("Pareto Chart of Top 10 US Storm Health Impacts")
# Display the chart
print(healthChart)

# Load necessary libraries
library(reshape2)
library(ggplot2)

# Melt the data
economicDataTopMelt <- melt(economicDataTop, id.vars="evtype")

# Create chart
economicChart <- ggplot(economicDataTopMelt, aes(x=reorder(evtype, -value), y=value))
# Add bars                            
economicChart <- economicChart + geom_bar(stat="identity", aes(fill=variable), position="dodge")
# Format y-axis scale and set y-axis label
economicChart <- economicChart + scale_y_sqrt("Damage Impact [$]") 
# Set x-axis label
economicChart <- economicChart + xlab("Event Type") 
# Rotate x-axis tick labels 
economicChart <- economicChart + theme(axis.text.x = element_text(angle=45, hjust=1))
# Set chart title
economicChart <- economicChart + ggtitle("Pareto Chart of Top 10 US Storm Economic Impacts")
# Display the chart
print(economicChart)