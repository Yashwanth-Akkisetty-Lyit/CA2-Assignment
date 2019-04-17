# path that contains all the .csv files
file_path <- "C:/Users/yaswa/Documents/CA2-Assignment/Csv_files/"
file_path

# files folder which contains list of all .csv files 
files <- list.files(path = file_path, pattern = "*.csv") 
files

# creating a data frame with the same name as the csv file and reading in each .csv file in files folder 
for (i in 1:length(files))
{
  assign(files[i], read.csv(paste(file_path, files[i], sep=''))
  )
}

# Amalgamating all of the crime data from each csv file into one dataset
AllNICrimeData <- rbind.data.frame(`2015-01-northern-ireland-street.csv`, `2015-02-northern-ireland-street.csv`, 
                                   `2015-03-northern-ireland-street.csv`, `2015-04-northern-ireland-street.csv`, 
                                   `2015-05-northern-ireland-street.csv`, `2015-06-northern-ireland-street.csv`, 
                                   `2015-07-northern-ireland-street.csv`, `2015-08-northern-ireland-street.csv`, 
                                   `2015-09-northern-ireland-street.csv`, `2015-10-northern-ireland-street.csv`, 
                                   `2015-11-northern-ireland-street.csv`, `2015-12-northern-ireland-street.csv`,
                                   `2016-01-northern-ireland-street.csv`, `2016-02-northern-ireland-street.csv`, 
                                   `2016-03-northern-ireland-street.csv`, `2016-04-northern-ireland-street.csv`,
                                   `2016-05-northern-ireland-street.csv`, `2016-06-northern-ireland-street.csv`,
                                   `2016-07-northern-ireland-street.csv`, `2016-08-northern-ireland-street.csv`,
                                   `2016-09-northern-ireland-street.csv`, `2016-10-northern-ireland-street.csv`,
                                   `2016-11-northern-ireland-street.csv`, `2016-12-northern-ireland-street.csv`,
                                   `2017-01-northern-ireland-street.csv`, `2017-02-northern-ireland-street.csv`,
                                   `2017-03-northern-ireland-street.csv`, `2017-04-northern-ireland-street.csv`,
                                   `2017-05-northern-ireland-street.csv`, `2017-06-northern-ireland-street.csv`,
                                   `2017-07-northern-ireland-street.csv`, `2017-08-northern-ireland-street.csv`,
                                   `2017-09-northern-ireland-street.csv`, `2017-10-northern-ireland-street.csv`,
                                   `2017-11-northern-ireland-street.csv`, `2017-12-northern-ireland-street.csv`)

# Displaying the amalgamated dataset
AllNICrimeData

# Saving the amalgamated dataset as a csv file
write.csv(AllNICrimeData, "AllNICrimeData.csv")

# Counting and showing the number of rows in the dataset
NROW(AllNICrimeData)


# Removing the selected columns in the dataset and creating a modified dataframe
AllNICrimeData <- data.frame(AllNICrimeData$Month, AllNICrimeData$Longitude, AllNICrimeData$Latitude,
                             AllNICrimeData$Location, AllNICrimeData$Crime.type)

# showing the structure of modified dataframe
str(AllNICrimeData)


# Factorising the crime type attribute and then showing the modified structure again
AllNICrimeData$AllNICrimeData.Crime.type <- as.factor(AllNICrimeData$AllNICrimeData.Crime.type)
str(AllNICrimeData)

#Modifying the AllNICrimeData dataset so that 
#the Location attribute contains only astreet name
head(AllNICrimeData, 5)
AllNICrimeData$AllNICrimeData.Location <- gsub("On or near ", "", 
                                               AllNICrimeData$AllNICrimeData.Location)
head(AllNICrimeData, 5)

# Modifying the resultant empty location attributes with a NA identifier
AllNICrimeData$AllNICrimeData.Location[AllNICrimeData$AllNICrimeData.Location == ""] <- NA
sum(is.na(AllNICrimeData$AllNICrimeData.Location)) # Sum of NA's in location column
head(AllNICrimeData, 5)

# Selecting the 1000 random samples of crime data from the AllNICrimeData dataset
#where the location attribute contains location information

new_subset <- subset(AllNICrimeData, !is.na(AllNICrimeData.Location))
new_subset

# Selecting a random sample of size 1000 from the newly created dataset
random_crime_sample <- data.frame(new_subset[sample(nrow(new_subset), 1000), ])
View(random_crime_sample)

library(dplyr)
# Converting the location attributes in random crime sample to upper case
random_crime_sample$AllNICrimeData.Location <- toupper(random_crime_sample$AllNICrimeData.Location)

# creating a new dataset that contains postcode and primary thorfare information from NIPostcodes dataset
new_ds <- postcodes[, c(6, 13)]
head(new_ds, 5)

# deleting the duplicate values in primary thorfare column
new_ds <- new_ds[!duplicated(new_ds$`Primary Thorfare`),]
# column names for new dataset
colnames(new_ds) <- c("Primary Thorfare", "Postcode")
str(new_ds)

# adding a new column to the random crime sample dataset and place the values as NA
random_crime_sample$Postcode <-NA
head(random_crime_sample, 5)

# adding the values for postcode column by matching the location with primary thorfare in new_ds
random_crime_sample$Postcode <- new_ds$Postcode[match(random_crime_sample$AllNICrimeData.Location, 
                                                      new_ds$`Primary Thorfare`)]
#Structure of random set
str(random_crime_sample)
# number of rows
NROW(random_crime_sample)
View(random_crime_sample)


# I had  already appended the postcodes to the random sample set above step
# Now Saving the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, "random_crime_sample.csv")

str(random_crime_sample)

updated_random_sample <- data.frame(random_crime_sample)
colnames(updated_random_sample) <- c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Postcode")
head(updated_random_sample, 3)

chart_data <- updated_random_sample
# Sorting chart_data as per Postcode and crime type
chart_data <- chart_data[order(chart_data$Postcode == "BT1", chart_data$Crime.type), ]
chart_data

# creating a new chart dataset that contains postcode = "BT1"
new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type), ]
str(new_chart)

# Summary of crime type as per postcode and location

crime_type <- data.frame(new_chart$Crime.type)
library(plyr)
crime_type <- ddply(crime_type, .(new_chart$Crime.type), nrow)
str(crime_type)
colnames(crime_type) <- c("crime_type", "count")
crime_type
View(crime_type)

#Creating a bar plot of the crime type from the chart_data data frame 
CrimeData <- table(chart_data$Crime.type)
barplot(CrimeData, main = "Crime Type Frequency", xlab = "Crime Type", ylab = "Frequency", col = "orange", border = "blue")










