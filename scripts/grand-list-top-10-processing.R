library(dplyr)
library(datapkg)
library(readxl)
library(stringi)
library(lubridate)
library(tidyr)
library(stringr)

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
gl_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 

#Isolate years used for profiles
gl_2017df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=1, skip=0)) 
gl_2016df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=2, skip=0)) 
gl_2014df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=3, skip=0)) 

tp_years <- c("2016", "2017")
#dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
gl_data <- c("gl_2014df", "gl_2016df")

#First process 2017, then use 2016 and 2014 to backfill if necessary
x2017_towns <- unique(gl_2017df$`What's your Town name?`)[!is.na(unique(gl_2017df$`What's your Town name?`))]
backfill_towns <- gl_2017df$Town[is.na(gl_2017df$`What's your Town name?`)]
backfill_towns2 <- gl_2017df$Town[is.na(gl_2017df$`Top 10 Business Names`)]

gl_2017 <- gl_2017df[gl_2017df$Town %in% x2017_towns,]

#set up business names list
gl_2017bus <- data.frame(gl_2017$Town, gl_2017$`Grand List Notes`, do.call('rbind', strsplit(as.character(gl_2017$`Top 10 Business Names`),';',fixed=TRUE)))
colnames(gl_2017bus) <- c("Town", "Year", 1,2,3,4,5,6,7,8,9,10)
gl_2017bus <- gather(gl_2017bus, Rank, Entry, 3:12, factor_key=FALSE)
gl_2017bus$Entry <- trimws(gl_2017bus$Entry)

#set up gl values list
gl_2017val <- data.frame(gl_2017$Town, gl_2017$`Grand List Notes`, do.call('rbind', strsplit(as.character(gl_2017$`Top 10 Grand List Values`),';',fixed=TRUE)))
colnames(gl_2017val) <- c("Town", "Year", 1,2,3,4,5,6,7,8,9,10)
gl_2017val <- gather(gl_2017val, Rank, Entry, 3:12, factor_key=FALSE)
gl_2017val$Entry <- trimws(gl_2017val$Entry)

gl_2017 <- merge(gl_2017bus, gl_2017val, by = c("Town", "Rank", "Year"), all=T)

#format years
gl_2017$Year <- str_sub(gsub("[^0-9.]", "", gl_2017$Year), -4, -1)
gl_2017$Year[!grepl("201", gl_2017$Year)] <- NA
gl_2017$Year[is.na(gl_2017$Year)] <- "2017"
gl_2017$`Year Submitted` <- "2018"
gl_2017$`Town Profile Year` <- "2018"
gl_2017$Rank <- factor(gl_2017$Rank, levels = c(1,2,3,4,5,6,7,8,9,10))

#preliminary formatting
gl_2017 <- gl_2017 %>% 
  select(Town, Year, `Year Submitted`, `Town Profile Year`, Entry.x, Rank,  Entry.y) %>% 
  rename(Entry = Entry.x, `Grand List Value` = Entry.y) %>% 
  arrange(Town, Rank)

#calculate % of totals
gl_2017_totals <- gl_2017df
gl_2017_totals <- gl_2017df[gl_2017df$Town %in% x2017_towns,]
gl_2017_totals <- gl_2017_totals %>% 
  select(Town, `Top 10 Total Grand List`, `Total Grand List`, `Net Grand List`)

gl_2017_calc <- merge(gl_2017, gl_2017_totals, by = "Town", all=T)

#reformat numeric columns for calculations
#function to gsub multiple characters in string
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

#apply function
gl_2017_calc$`Grand List Value` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017_calc$`Grand List Value`)
gl_2017_calc$`Top 10 Total Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017_calc$`Top 10 Total Grand List`)
gl_2017_calc$`Total Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017_calc$`Total Grand List`)
gl_2017_calc$`Net Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017_calc$`Net Grand List`)

#convert to numeric
cols <- c("Top 10 Total Grand List", "Total Grand List", "Net Grand List", "Grand List Value")
gl_2017_calc[cols] <- sapply(gl_2017_calc[cols],as.numeric)

#calculate percents
gl_2017_calc$`Percent of Net Grand List` <- round((gl_2017_calc$`Grand List Value` / gl_2017_calc$`Net Grand List`)*100,2)
gl_2017_calc$`Percent of Top 10 Total Grand List` <- round((gl_2017_calc$`Grand List Value` / as.numeric(gl_2017_calc$`Top 10 Total Grand List`))*100,2)
gl_2017_calc$`Percent of Total Grand List` <- round((gl_2017_calc$`Grand List Value` / as.numeric(gl_2017_calc$`Total Grand List`))*100,2)

#remove totals
gl_2017_calc <- gl_2017_calc %>% select(-c(8:10))

#correct case in gl entries
gl_2017_calc$Entry <- gsub("Llc", "LLC", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("Iii", "III", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("Ii", "II", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("Ct ", "CT ", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub(" Ct", " CT", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub(" Lp", " LP", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("Llp", " LLP", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("At& ", "AT&", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("Et Al", "et al", gl_2017_calc$Entry)
gl_2017_calc$Entry <- gsub("ET AL", "et al", gl_2017_calc$Entry)

#reformat variables
gl_final_long <- gather(gl_2017_calc, Variable, Value, 7:10, factor_key=F)

#set MT
gl_final_long$`Measure Type` <- "Percent"
gl_final_long$`Measure Type`[gl_final_long$Variable == "Grand List Value"] <- "Number"

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

gl_final_long_fips <- merge(gl_final_long, fips, by = "Town", all.x=T)

gl_final_long_fips <- gl_final_long_fips %>% 
  filter(!is.na(Entry))

#################################################################################################################################
#Get data from previous years to backfill 2017 gl
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
complete_gl_long_fips$"Measure Type" <- "Percent"
complete_gl_long_fips$"Measure Type"[which(complete_gl_long_fips$Variable %in% c("Grand List Value"))] <- "Number"

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

#################################################################################################################################
#Backfill 2017 gl values with data from 2016 gl
bf_from_2016 <- complete_gl_long_fips[complete_gl_long_fips$Town %in% backfill_towns2 & complete_gl_long_fips$`Town Profile Year` == "2017",]
bf_from_2016$`Town Profile Year` <- "2018"

final_gl <- rbind(gl_final_long_fips, bf_from_2016, complete_gl_long_fips)

final_gl$`Year` <- as.integer(final_gl$`Year`)
final_gl$`Year Submitted` <- as.integer(final_gl$`Year Submitted`)
final_gl$`Town Profile Year`<- as.integer(final_gl$`Town Profile Year`)

final_gl$Town <- as.character(final_gl$Town)
final_gl$Rank <- as.integer(final_gl$Rank)
final_gl$FIPS <- as.integer(final_gl$FIPS)

final_gl <- final_gl %>% 
  select(Town, FIPS, Year, `Year Submitted`, `Town Profile Year`, Entry, Rank, Variable, `Measure Type`, Value) %>% 
  arrange(Town, Variable, `Town Profile Year`, Rank) 

# Write to File
write.table(
  final_gl,
  file.path(getwd(), "data", "grand_list_top_10_2014_2017.csv"),
  sep = ",",
  na = "-666666",
  row.names = F
)
