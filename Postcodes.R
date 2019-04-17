# This script builds the NIPostcodes dataset
# and populates it with data
postcodes <- read.csv("NIPostcodes.csv")
postcodes <- data.frame(postcodes)
View(postcodes)

#Showing the total number of rows in the dataset
NROW(postcodes)

#Structure of dataframe
str(postcodes)

#Displaying the first 10 rows of the data frame
postcodes[1:10,]

#Adding a title for each attribute of the data
column_names <- c("Organisation Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfare",
                  "Alt Thorfare", "Secondary Thorfare", "Locality", "Townland", "Town", "County", "Postcode"
                  , "x-coordinates","y-coordinates","Primary key")

colnames(postcodes) <- column_names
head(postcodes, 5)
View(postcodes)

#Replacing the missing values with NA
postcodes[postcodes == ""] <- NA
View(postcodes)

#Total number of missing values
na.sum <- sapply(postcodes, function(y) sum(length(which(is.na(y)))))
na.sum

#Mean of the missing values
na.mean <- sapply(postcodes, function(y) mean(is.na(y)))
na.mean                             

#Modifying the County attribute to be a categorising factor
postcodes$County <- as.factor(postcodes$County)
str(postcodes)
View(postcodes)

#Moving the primary key identifier to the start of the dataset
install.packages("dplyr") #installing dplyr for manipulating the data
library(dplyr)

postcodes <- postcodes %>% select('Primary key', everything())
postcodes

#Creating a new dataset 
Limavady <- data.frame(postcodes$Locality, postcodes$Townland, postcodes$Town) %>% 
  filter(postcodes$Town == "LIMAVADY")
Limavady
write.csv(Limavady,'Limavady.csv')

#Saving the modified NIPostcode dataset
write.csv(postcodes,'CleanNIPostcodeData.csv')





