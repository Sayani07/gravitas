library(tidyverse)
library(lubridate)
library(tsibble)
library(gravitas)
library(readr)
library(HadoopStreaming)
library(SparkR)
library(sparklyr)

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/home/spark")
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sc <- sparkR.session(enableHiveSupport = FALSE,sparkConfig = list(spark.driver.memory = "4g",spark.executor.memory = "4g",spark.driver.cores="8",spark.driver.maxResultSize="4g",spark.master="spark://10.100.11.72:7077"))
sparkR.session()
setLogLevel(newLevel)
aa<-read.df("hdfs://10.100.11.70:9000/ data/CD_INTERVAL_READING_ALL_NO_QUOTES-3.csv",source = "com.databricks.spark.csv",header="true",na.strings='0',inferSchema="true")


# tz(smart_meter$READING_DATETIME) # UTC
con = file(description="stdin",open="r")
str <- "data/CD_INTERVAL_READING_ALL_NO_QUOTES-3"
cat(str)
con <- textConnection(str, open = "r")
cat(str,file="datafile.txt")
con <- file("datafile.txt",open="r")
numlines = 2
con <- pipe(paste("head -n",numlines,'datafile.txt'), "r")
# Loading the data from Scratch

smart_meter_data <- fread("data/CD_INTERVAL_READING_ALL_NO_QUOTES-3.csv")



#
# datafile$`Timestamp UTC` <- ymd_hms(datafile$`Timestamp UTC`)
#
# datafile <- datafile %>% filter(lubridate::second(datafile$`Timestamp UTC`)== 0)
#
# smart_meter_MRS <- datafile %>%
#   as_tsibble(index = `Timestamp UTC`, key = Source)
#
# save(smart_meter_MRS, file = "data/smart_meter_MRS.Rds")
#
#
# load(file = "data/smart_meter_MRS.Rds")
#

# smart_meter_MRS
#
# `smart-meter13` <- readRDS("data-raw/smart-meter13.rds")
#
# smart_meter_13 <-`smart-meter13` %>% select(customer_id, reading_datetime, event_key, general_supply_kwh)
#
# smart_meter_13_ts <- smart_meter_13 %>% as_tsibble(index = reading_datetime, key = customer_id)
#
#
# smart_meter_13_cust1 <-  smart_meter_13_ts %>% filter(customer_id == 8143667)
#
# save(smart_meter_13_cust1, file = "data/smart_meter_13_cust2.rda")
#
# smart_meter_13_cust2 <-  smart_meter_13_ts %>% filter(customer_id == 8144679)
#
# save(smart_meter_13_cust2, file = "data/smart_meter_13_cust2.rda")
#
#
# smart_meter_13_cust3 <-  smart_meter_13_ts %>% filter(customer_id == 11468208)
#
# save(smart_meter_13_cust3, file = "data/smart_meter_13_cust3.rda")
#
#
# #
# #
# # load(file = "data/smart_meter_MRS.Rds")
# #
# # `smart_meter_MRS` <- `smart_meter_MRS` %>%
# #   group_by(Source) %>%
# #   mutate(`Timestamp UTC` = case_when(
# #     duplicated(`Timestamp UTC`) ~ `Timestamp UTC` + hours(1),
# #     TRUE ~ `Timestamp UTC`
# #   ))
# #


## to be run in di's computer

smart_meter_data <- read_csv("data/CD_INTERVAL_READING_ALL_NO_QUOTES-3.csv")
smart_meter_ts <- as_tsibble(smart_meter_data, index = ` READING_DATETIME `, key = `CUSTOMER_ID`)

