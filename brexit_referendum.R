# Import data from file into R data frame
file_data <- read.csv("data_brexit_referendum.csv")
str(file_data)


#Covert all -1 to NA in Leave
sum(file_data$Leave[file_data$Leave == -1 ])
file_data$Leave[file_data$Leave == -1 ] <- NA
sum(file_data$Leave[file_data$Leave == -1 ])
 #subset(file_data, Leave == -1,select = c(Leave))


# Check missing values
library("VIM")
missing_values <- aggr(file_data, prop=FALSE, numbers=TRUE)

# Find percentage of voted to leave
file_data$percent_leave <- file_data$Leave / file_data$NVotes

# Set Vote
file_data$Vote[file_data$percent_leave <= 0.5] <- "Remain"
file_data$Vote[file_data$percent_leave > 0.5] <- "Leave"

# Short Name
file_data$ShortName[file_data$RegionName == "London"] <- "L"
file_data$ShortName[file_data$RegionName == "North West"] <- "NW"
file_data$ShortName[file_data$RegionName == "North East"] <- "NE"
file_data$ShortName[file_data$RegionName == "South West"] <- "SW"
file_data$ShortName[file_data$RegionName == "South East"] <- "SE"
file_data$ShortName[file_data$RegionName == "East Midlands"] <- "EM"
file_data$ShortName[file_data$RegionName == "West Midlands"] <- "WM"
file_data$ShortName[file_data$RegionName == "East of England"] <- "EE"
file_data$ShortName[file_data$RegionName == "Yorkshire and the Humber"] <- "H"

summary(file_data)

num_var_list <- sapply(file_data, is.numeric)
num_var_list["ID"] <- FALSE
  
num_data <- file_data[num_var_list]
colnames(num_data)

num_data <- num_data[complete.cases(num_data),]

num_summary <- do.call(cbind,lapply(num_data, summary))

# Find max percent leave 
num_summary["Max.","percent_leave"]
num_summary["Min.","percent_leave"]
num_summary["Min.","percent_leave"] - num_summary["Max.","percent_leave"]

display_vars <- c("NoQuals","percent_leave","AdultMeanAge", "L4Quals_plus", "RegionName")

file_data[which.max(file_data$percent_leave),display_vars]
file_data[which.min(file_data$percent_leave),display_vars]

prop.table(table(file_data$ShortName))

barplot(height = prop.table(table(file_data$ShortName)),
        main = "Vote propotion by region",
        ylab = "Frequency",
        xlab = "Region",
        col = "Red")
