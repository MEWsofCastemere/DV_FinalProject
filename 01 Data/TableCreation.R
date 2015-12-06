require(tidyr)
require(dplyr)
require(ggplot2)

setwd("~/DataVisualization/DV_FinalProject/01 Data")

<<<<<<< HEAD
file_path <- "StormEvents_details-ftp_v1.0_d2012_c20150917.csv"
=======
  file_path <- "StormEvents_details-ftp_v1.0_d2012_c20150917.csv"
>>>>>>> ac06e3ae33f968d29cbb8803a03451eea51835be

df <- read.csv(file_path, stringsAsFactors = FALSE)

#str(df)

measures <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "BEGIN_TIME", "END_YEARMONTH", "END_DAY", "END_TIME", "EPISODE_ID", "EVENT_ID", "STATE_FIPS", "YEAR", "CZ_FIPS", "INJURIES_DIRECT", "INJURIES_INDIRECT", "DEATHS_DIRECT", "DEATHS_INDIRECT", "DAMAGE_PROPERTY", "DAMAGE_CROPS", "MAGNITUDE", "TOR_LENGTH", "TOR_WIDTH", "BEGIN_RANGE", "END_RANGE", "BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON", "BEGIN_HOUR", "BEGIN_MIN", "END_MIN", "END_HOUR")

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
#for(n in names(df)) {
#  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
#}

dimensions <- setdiff(names(df), measures)
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
}

library(lubridate)
# Fix date columns, this needs to be done by hand because | needs to be correct.
#                                                        \_/
#df$Order_Date <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(df$Order_Date), tz="UTC")))
#df$Ship_Date  <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(df$Ship_Date),  tz="UTC")))

# The following is an example of dealing with special cases like making state abbreviations be all upper case.
# df["State"] <- data.frame(lapply(df["State"], toupper))

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
  }
}

write.csv(df, paste(gsub(".csv", "", file_path), ".reformatted2.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)
