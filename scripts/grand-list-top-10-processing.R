library(dplyr)
library(datapkg)
library(readxl)
library(stringi)

##################################################################
#
# Processing Script for Grand List Top 10
# Created by Jenna Daly
# On 05/22/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
gl_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 
gl_data <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=1, skip=0)) 

#omit rows where is.na(Town)
gl_data <- gl_data[!is.na(gl_data$Town),]

#omit first 2 columns, last column
gl_data <- gl_data[,-c(1,2,29)]

#replace "GL#" with "Rank" in all column names
colnames(gl_data) <- sub("GL#", "Rank ", colnames(gl_data))

#Convert from wide to long format
gl_data_long <- data.frame(stringsAsFactors = F)
# iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
for(i in 1:10) {
  cols = c(1, 2, ((2 * i) + 1), (2 * (i + 1)))
  ranked <- gl_data[, cols]
  names(ranked) <- c("Town", "Year", "Entry", "Grand List Value")
  ranked$Rank <- i
  gl_data_long <- rbind(gl_data_long, ranked)
  remove(ranked, cols)
}

#Find towns where grand list value is NA
gl_na <- unique(gl_data_long[is.na(gl_data_long$`Grand List Value`),]$Town)

#Grab the "old" data from these towns
old_gl_data <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=2, skip=0)) 
replace_gl_data <- old_gl_data[old_gl_data$Town %in% gl_na,]
replace_gl_data <- replace_gl_data[,-c(1,2,29, 30)]
colnames(replace_gl_data) <- sub("GL#", "Rank ", colnames(replace_gl_data))

replace_data_long <- data.frame(stringsAsFactors = F)
# iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
for(i in 1:10) {
  cols = c(1, 2, ((2 * i) + 1), (2 * (i + 1)))
  ranked <- replace_gl_data[, cols]
  names(ranked) <- c("Town", "Year", "Entry", "Grand List Value")
  ranked$Rank <- i
  replace_data_long <- rbind(replace_data_long, ranked)
  remove(ranked, cols)
}

#remove towns that need to get replaced
gl_data_long_sub <- gl_data_long[!gl_data_long$Town %in% gl_na,]

#add in replacement df
gl_data_long_complete <- rbind(gl_data_long_sub, replace_data_long)

#Bring back in denominators for calculated variables
gl_denom <- gl_data[,c(1,2,23,25,26)]
gl_denom_for_na <- replace_gl_data[,c(1,2,23,25,26)]
colnames(gl_denom) <- c("Town", "Year", "Top 10 Total", "Total", "Net Total")
colnames(gl_denom_for_na) <- c("Town", "Year", "Top 10 Total", "Total", "Net Total")

gl_denom_sub <- gl_denom[!gl_denom$Town %in% gl_na,]
gl_denom_complete <- rbind(gl_denom_sub, gl_denom_for_na)

gl_data_total <- merge(gl_data_long_complete, gl_denom_complete, by = c("Town", "Year"))

gl_data_total$`Percent of Total Grand List` <- round((gl_data_total$`Grand List Value` / gl_data_total$`Total`) * 100, 2)
gl_data_total$`Percent of Net Grand List` <-  round((gl_data_total$`Grand List Value` /  gl_data_total$`Net Total`) * 100, 2)
gl_data_total$`Percent of Top 10 Total Grand List` <-  round((gl_data_total$`Grand List Value` /  gl_data_total$`Top 10 Total`) * 100, 2)

#Omit columns not needed
gl_data_total <- gl_data_total[,-c(6:8)]

#Convert to long format
cols_to_stack <- c("Grand List Value", 
                   "Percent of Total Grand List", 
                   "Percent of Net Grand List", 
                   "Percent of Top 10 Total Grand List")

long_row_count = nrow(gl_data_total) * length(cols_to_stack)

gl_data_total_long <- reshape(gl_data_total,
                           varying = cols_to_stack,
                           v.names = "Value",
                           timevar = "Variable",
                           times = cols_to_stack,
                           new.row.names = 1:long_row_count,
                           direction = "long"
)

gl_data_total_long$id <- NULL
options(scipen=999)

#Convert town names to correct case
gl_data_total_long$Town <- stri_trans_totitle(gl_data_total_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

gl_data_total_long_fips <- merge(gl_data_total_long, fips, by = "Town", all=T)
#remove CT
gl_data_total_long_fips <- gl_data_total_long_fips[gl_data_total_long_fips$Town != "Connecticut",]

#Assign Measure Type
gl_data_total_long_fips$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
gl_data_total_long_fips$"Measure Type"[which(gl_data_total_long_fips$Variable %in% c("Grand List Value"))] <- "Number"


gl_data_total_long_fips$"Measure Type"[which(gl_data_total_long_fips$Variable %in% c("Percent of Total Grand List", 
                                                                                     "Percent of Net Grand List", 
                                                                                     "Percent of Top 10 Total Grand List"))] <- "Percent"

#Order columns
gl_data_total_long_fips <- gl_data_total_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Entry`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, Rank)

#Set New Canaan year one less than cutoff year (blank data)
gl_data_total_long_fips$Year <- as.numeric(gl_data_total_long_fips$Year)
gl_years <- unique(gl_data_total_long_fips$Year)
latest_year <- max(gl_years[!is.na(gl_years)])
cutoff_year <- latest_year - 2
gl_data_total_long_fips$Year[gl_data_total_long_fips$Town == "New Canaan"] <- (cutoff_year - 1)

#Code "Old" data to -6666 (data before 2014)
gl_data_total_long_fips$Value[gl_data_total_long_fips$Year < cutoff_year] <- -6666

# Write to File
write.table(
  gl_data_total_long_fips,
  file.path(getwd(), "data", "grand_list_top_10_2016.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)

