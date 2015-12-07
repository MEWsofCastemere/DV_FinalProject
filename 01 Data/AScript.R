require(jsonlite)
require(RCurl)
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(gridExtra)
require(lubridate)
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS2011"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df2011 <- df

df2012 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS2012"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df2013 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df2014 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS2014"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df2015 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS2015"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df2011 <- df2011 %>% select(STATE_FIPS, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME, YEAR, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, MAGNITUDE) %>% mutate(END = ((as.numeric(END_TIME) %% 100)/60*100 + (as.numeric(END_TIME) %/% 100 * 100)), BEGIN = ((as.numeric(BEGIN_TIME) %% 100)/60*100 + (as.numeric(BEGIN_TIME) %/% 100 * 100))) %>% mutate (DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (END - BEGIN)) %>% filter(DAMAGE_KPI != "Inf", DAMAGE_KPI > 0)

df2012 <- df2012 %>% select(STATE_FIPS, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME, YEAR, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, MAGNITUDE) %>% mutate(END = ((as.numeric(END_TIME) %% 100)/60*100 + (as.numeric(END_TIME) %/% 100 * 100)), BEGIN = ((as.numeric(BEGIN_TIME) %% 100)/60*100 + (as.numeric(BEGIN_TIME) %/% 100 * 100))) %>% mutate (DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (END - BEGIN)) %>% filter(DAMAGE_KPI != "Inf", DAMAGE_KPI > 0)

df2013 <- df2013 %>% select(STATE_FIPS, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME, YEAR, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, MAGNITUDE) %>% mutate(END = ((as.numeric(END_TIME) %% 100)/60*100 + (as.numeric(END_TIME) %/% 100 * 100)), BEGIN = ((as.numeric(BEGIN_TIME) %% 100)/60*100 + (as.numeric(BEGIN_TIME) %/% 100 * 100))) %>% mutate (DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (END - BEGIN)) %>% filter(DAMAGE_KPI != "Inf", DAMAGE_KPI > 0)

df2014 <- df2014 %>% select(STATE_FIPS, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME, YEAR, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, MAGNITUDE) %>% mutate(END = ((as.numeric(END_TIME) %% 100)/60*100 + (as.numeric(END_TIME) %/% 100 * 100)), BEGIN = ((as.numeric(BEGIN_TIME) %% 100)/60*100 + (as.numeric(BEGIN_TIME) %/% 100 * 100))) %>% mutate (DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (END - BEGIN)) %>% filter(DAMAGE_KPI != "Inf", DAMAGE_KPI > 0)

df2015 <- df2015 %>% select(STATE_FIPS, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME, YEAR, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, MAGNITUDE) %>% mutate(END = ((as.numeric(END_TIME) %% 100)/60*100 + (as.numeric(END_TIME) %/% 100 * 100)), BEGIN = ((as.numeric(BEGIN_TIME) %% 100)/60*100 + (as.numeric(BEGIN_TIME) %/% 100 * 100))) %>% mutate (DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (END - BEGIN)) %>% filter(DAMAGE_KPI != "Inf", DAMAGE_KPI > 0)

dfState <- read.csv("Fips.csv", stringsAsFactors = FALSE)
dfState <- dfState %>% mutate(STATE_FIPS = as.numeric(STATE_FIPS))

df_comp <- bind_rows(df2011, df2012, df2013, df2014, df2015)

df_comp <- df_comp %>% mutate(DAMAGE_KPI = as.numeric(DAMAGE_KPI), STATE_FIPS = as.numeric(STATE_FIPS)) %>% filter(DAMAGE_KPI > 1)
str(df_comp)

df_comp2 <- left_join(df_comp, dfState, by = "STATE_FIPS")

require(tidyr)
require(dplyr)
require(ggplot2)


df <- df_comp2

measures <- c("STATE_FIPS", "DAMAGE_CROPS", "DAMAGE_PROPERTY", "BEGIN_TIME", "END_TIME", "YEAR", "INJURIES_DIRECT", "INJURIES_INDIRECT", "DEATHS_DIRECT", "DEATHS_INDIRECT", "MAGNITUDE", "END", "BEGIN", "DAMAGE_KPI")

dimensions <- setdiff(names(df), measures)


library(lubridate)

if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="null",replacement= "0"))
  }
}


write.csv(df, paste(gsub(".csv", "", "AdamData"), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", "AdamData")))
sql <- paste("CREATE TABLE", "AdamData", "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    sql <- paste(sql, paste(d, "varchar2(4000),\n"))
  }
}
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)





