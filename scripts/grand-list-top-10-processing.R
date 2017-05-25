library(dplyr)
library(datapkg)
library(readxl)
library(stringi)
library(lubridate)

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
gl_xlsx_2016 <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 
gl_csv_2014 <- dir(path_to_raw_data, recursive=T, pattern = "grand") 

#First bring in 2014 data set
gl_2014 <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx_2016), sheet=2, skip=0)) 

#omit rows where is.na(Town)
gl_2014 <- gl_2014[!is.na(gl_2014$Town),]

#omit first 2 columns, last column
gl_2014 <- gl_2014[,-c(1,2,30)]

#Extract year from 'submitted' column
gl_2014$`Date Submitted` <- year(gl_2014$`Date Submitted`)

#replace "GL#" with "Rank" in all column names
colnames(gl_2014) <- sub("GL#", "Rank ", colnames(gl_2014))

#Convert from wide to long format
gl_2014_long <- data.frame(stringsAsFactors = F)
# iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
for(i in 1:10) {
  cols = c(1, 2, 27, ((2 * i) + 1), (2 * (i + 1)))
  ranked <- gl_2014[, cols]
  names(ranked) <- c("Town", "Grand List Year", "Year Submitted", "Entry", "Grand List Value")
  ranked$Rank <- i
  gl_2014_long <- rbind(gl_2014_long, ranked)
  remove(ranked, cols)
}

#Bring back in denominators for calculated variables
gl_denom_2014 <- gl_2014[,c(1,2,23,25,26)]
colnames(gl_denom_2014) <- c("Town", "Grand List Year", "Top 10 Total", "Total", "Net Total")

gl_2014_total <- merge(gl_2014_long, gl_denom_2014, by = c("Town", "Grand List Year"))

gl_2014_total$`Percent of Total Grand List` <- round((gl_2014_total$`Grand List Value` / gl_2014_total$`Total`) * 100, 2)
gl_2014_total$`Percent of Net Grand List` <-  round((gl_2014_total$`Grand List Value` /  gl_2014_total$`Net Total`) * 100, 2)
gl_2014_total$`Percent of Top 10 Total Grand List` <-  round((gl_2014_total$`Grand List Value` /  gl_2014_total$`Top 10 Total`) * 100, 2)

#Omit columns not needed
gl_2014_total <- gl_2014_total[,-c(7:9)]

#Convert to long format
cols_to_stack <- c("Grand List Value", 
                   "Percent of Total Grand List", 
                   "Percent of Net Grand List", 
                   "Percent of Top 10 Total Grand List")

long_row_count = nrow(gl_2014_total) * length(cols_to_stack)

gl_2014_total_long <- reshape(gl_2014_total,
                           varying = cols_to_stack,
                           v.names = "Value",
                           timevar = "Variable",
                           times = cols_to_stack,
                           new.row.names = 1:long_row_count,
                           direction = "long"
)

gl_2014_total_long$id <- NULL
options(scipen=999)

#Convert town names to correct case
gl_2014_total_long$Town <- stri_trans_totitle(gl_2014_total_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

gl_2014_total_long_fips <- merge(gl_2014_total_long, fips, by = "Town", all=T)
#remove CT
gl_2014_total_long_fips <- gl_2014_total_long_fips[gl_2014_total_long_fips$Town != "Connecticut",]

#Assign Measure Type
gl_2014_total_long_fips$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
gl_2014_total_long_fips$"Measure Type"[which(gl_2014_total_long_fips$Variable %in% c("Grand List Value"))] <- "Number"


gl_2014_total_long_fips$"Measure Type"[which(gl_2014_total_long_fips$Variable %in% c("Percent of Total Grand List", 
                                                                                     "Percent of Net Grand List", 
                                                                                     "Percent of Top 10 Total Grand List"))] <- "Percent"

#Check to make sure years make sense (should return FALSE)
gl_2014_total_long_fips$`Grand List Year` <- as.numeric(gl_2014_total_long_fips$`Grand List Year`)
min_year <- min(gl_2014_total_long_fips$`Grand List Year`, na.rm=T)
any(gl_2014_total_long_fips$`Grand List Year` < min_year, na.rm=T)
any(gl_2014_total_long_fips$`Year Submitted` < min_year, na.rm=T)

#Fix any towns with incorrect years
gl_2014_total_long_fips$"Year Submitted"[which(gl_2014_total_long_fips$Town %in% c("North Branford"))] <- 2014

#Set profiles year
gl_2014_total_long_fips$"Town Profile Year" <- 2016

#Order columns
gl_2014_total_long_fips <- gl_2014_total_long_fips %>% 
  select(`Town`, `FIPS`, `Grand List Year`, `Year Submitted`, `Town Profile Year`, `Entry`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, Rank)

#Set towns with no year to cutoff year (blank data)
latest_year <- max(gl_2014_total_long_fips$`Town Profile Year`, na.rm=T)
cutoff_year <- latest_year - 3

NA_towns_gl <- unique(gl_2014_total_long_fips[is.na(gl_2014_total_long_fips$`Grand List Year`),]$Town)

gl_2014_total_long_fips$"Grand List Year"[which(gl_2014_total_long_fips$Town %in% NA_towns_gl)] <- cutoff_year
gl_2014_total_long_fips$"Year Submitted"[which(gl_2014_total_long_fips$Town %in% NA_towns_gl)] <- cutoff_year

#Now find any towns where year submitted is blank
NA_towns_submit <- unique(gl_2014_total_long_fips[is.na(gl_2014_total_long_fips$`Year Submitted`),]$Town)
#set year submitted equal to GL year
gl_2014_total_long_fips <- gl_2014_total_long_fips %>% 
  mutate(`Year Submitted` = ifelse(Town %in% NA_towns_submit, `Grand List Year`, `Year Submitted`))

#Code "Old" data to -6666 (data before cutoff year)
gl_2014_total_long_fips$Value[gl_2014_total_long_fips$`Grand List Year` < cutoff_year] <- -6666

# Write to File
write.table(
  gl_2014_total_long_fips,
  file.path(getwd(), "data", "grand_list_top_10_2014.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)

