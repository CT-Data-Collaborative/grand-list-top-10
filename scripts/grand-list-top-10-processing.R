library(dplyr)
library(datapkg)
library(readxl)
library(stringi)
library(lubridate)

##################################################################
#
# Processing Script for Grand List Top 10
# Created by Jenna Daly
# On 06/01/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
gl_xlsx_2016 <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 

#Isolate years used for profiles
gl_2016 <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx_2016), sheet=1, skip=0)) 
gl_2014 <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx_2016), sheet=2, skip=0)) 

tp_years <- c("2016", "2017")
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
gl_data <- grep("gl_", dfs, value=T)

#omit rows where is.na(Town)
#omit first 2 columns, last column
#Extract year from 'submitted' column
#replace "GL#" with "Rank" in all column names
#Convert from wide to long format
#Iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
#Bring back in denominators for calculated variables

complete_gl <- data.frame(stringsAsFactors = F)
for (j in 1:length(gl_data)) {
  current_year <- get(gl_data[j])
  current_year <- current_year[!is.na(current_year$Town),]
  current_year <- current_year[,c(3:29)]
  current_year$`Date Submitted` <- year(current_year$`Date Submitted`)
  colnames(current_year) <- sub("GL#", "Rank ", colnames(current_year))
  current_year_long <- data.frame(stringsAsFactors = F)
  for(i in 1:10) {
    cols = c(1, 2, 27, ((2 * i) + 1), (2 * (i + 1)))
    ranked <- current_year[, cols]
    names(ranked) <- c("Town", "Year", "Year Submitted", "Entry", "Grand List Value")
    ranked$Rank <- i
    current_year_long <- rbind(current_year_long, ranked)
    remove(ranked, cols)
  } 
  gl_denom_current_year <- current_year[,c(1,2,23,25,26)]
  colnames(gl_denom_current_year) <- c("Town", "Year", "Top 10 Total", "Total", "Net Total")
  current_year_total <- merge(current_year_long, gl_denom_current_year, by = c("Town", "Year"))
  current_year_total$`Town Profile Year` <- tp_years[j]
  complete_gl <- rbind(complete_gl, current_year_total)
}

#Calculated variables
complete_gl$`Percent of Total Grand List` <- round((complete_gl$`Grand List Value` / complete_gl$`Total`) * 100, 2)
complete_gl$`Percent of Net Grand List` <-  round((complete_gl$`Grand List Value` /  complete_gl$`Net Total`) * 100, 2)
complete_gl$`Percent of Top 10 Total Grand List` <-  round((complete_gl$`Grand List Value` /  complete_gl$`Top 10 Total`) * 100, 2)

#Omit columns not needed
complete_gl <- complete_gl[,-c(7:9)]

#Convert to long format
cols_to_stack <- c("Grand List Value", 
                   "Percent of Total Grand List", 
                   "Percent of Net Grand List", 
                   "Percent of Top 10 Total Grand List")

long_row_count = nrow(complete_gl) * length(cols_to_stack)

complete_gl_long <- reshape(complete_gl,
                           varying = cols_to_stack,
                           v.names = "Value",
                           timevar = "Variable",
                           times = cols_to_stack,
                           new.row.names = 1:long_row_count,
                           direction = "long"
)

complete_gl_long$id <- NULL
options(scipen=999)

#Convert town names to correct case
complete_gl_long$Town <- stri_trans_totitle(complete_gl_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

complete_gl_long_fips <- merge(complete_gl_long, fips, by = "Town", all=T)

#remove CT
complete_gl_long_fips <- complete_gl_long_fips[complete_gl_long_fips$Town != "Connecticut",]

#Assign Measure Type
complete_gl_long_fips$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below

complete_gl_long_fips$"Measure Type"[which(complete_gl_long_fips$Variable %in% c("Grand List Value"))] <- "Number"

complete_gl_long_fips$"Measure Type"[which(complete_gl_long_fips$Variable %in% c("Percent of Total Grand List", 
                                                                                 "Percent of Net Grand List", 
                                                                                 "Percent of Top 10 Total Grand List"))] <- "Percent"

#Check to make sure years make sense (should return FALSE)
complete_gl_long_fips$`Year` <- as.numeric(complete_gl_long_fips$`Year`)
complete_gl_long_fips$`Town Profile Year` <- as.numeric(complete_gl_long_fips$`Town Profile Year`)
min_year <- min(complete_gl_long_fips$`Year`, na.rm=T)
any(complete_gl_long_fips$`Year` < min_year, na.rm=T)
any(complete_gl_long_fips$`Year Submitted` < min_year, na.rm=T)

#Fix any towns with incorrect years (North Branford from 2014 only)
complete_gl_long_fips$"Year Submitted"[which(complete_gl_long_fips$Town %in% c("North Branford") & (complete_gl_long_fips$`Town Profile Year` == 2016))] <- 2014

#Set towns with blank years
#Two scenarios:
#1) GL year present, submitted year blank - set submitted year to GL year (Naugatuck, Voluntown)
blank2 <- complete_gl_long_fips[is.na(complete_gl_long_fips$`Year Submitted`),]
blank2 <- unique(blank2[!is.na(blank2$`Year`),]$Town)

#2) GL year blank, submitted year blank - set both years to cutoff year (Morris (only 2016), New Canaan)
blank3 <- complete_gl_long_fips[is.na(complete_gl_long_fips$`Year Submitted`),]
blank3 <- unique(blank3[is.na(blank3$`Year`),]$Town)

#Set years accordingly
complete_gl_long_fips <- complete_gl_long_fips %>%
  mutate(`Year Submitted` = ifelse(Town %in% blank2, `Year`, `Year Submitted`), 
         `Year` = ifelse((Town %in% blank3) & (is.na(`Year`)) & (is.na(`Year Submitted`)), (`Town Profile Year` - 3), `Year`), #fills it in only if both years are blank
         `Year Submitted` = ifelse((Town %in% blank3) & (is.na(`Year Submitted`)), (`Town Profile Year` - 3), `Year Submitted`))  #now that year is filled in, only need to check on year submitted is blank
  
#Populate missing 2017 values with 2016 values
fix_2017 <- complete_gl_long_fips[is.na(complete_gl_long_fips$Value) & 
                                    complete_gl_long_fips$Rank == 1 & 
                                    complete_gl_long_fips$Variable == "Grand List Value" & 
                                    complete_gl_long_fips$`Town Profile Year` == 2017,]

towns_2017 <- unique(fix_2017$Town)
bring_from_2016_again <- complete_gl_long_fips[!is.na(complete_gl_long_fips$Value) & 
                                                 complete_gl_long_fips$Town %in% towns_2017 & 
                                                 complete_gl_long_fips$`Town Profile Year` == 2016,]

bring_from_2016_again$`Town Profile Year` <- 2017
fix_towns_2017 <- unique(bring_from_2016_again$Town)
complete_gl_long_fips <- complete_gl_long_fips[!(complete_gl_long_fips$Town %in% fix_towns_2017 & complete_gl_long_fips$`Town Profile Year` == 2017),]
complete_gl_long_fips <- rbind(bring_from_2016_again, complete_gl_long_fips)

#Order columns
complete_gl_long_fips <- complete_gl_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Year Submitted`, `Town Profile Year`, `Entry`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, `Town Profile Year`, Rank)

# Write to File
write.table(
  complete_gl_long_fips,
  file.path(getwd(), "data", "grand_list_top_10_2014_2016.csv"),
  sep = ",",
  na = "-666666",
  row.names = F
)

