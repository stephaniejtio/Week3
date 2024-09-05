#read dataset for research 
mRes <- read.csv("accepted_2007_to_2018Q4v2.csv")

#large data from Dr Saikat
mRes1 <- read_csv("P2P_Macro_Data.csv")

install.packages("data.table")
library(readr)
library(data.table)
library(dplyr)
library(lubridate)

df <- fread("P2P_Macro_Data.csv")

#Convert date column to Date format
df$issue_d <- dmy(paste0("01-", df$issue_d))

#Filter rows
df_filtered <- df %>%
  filter(issue_d >= as.Date("2018-12-12") & issue_d <= as.Date("2019-1-13"))

#see the last 10 rows
last_10_rows <- tail(df, n = 10)

# Read first 100 rows
data <- read.csv("P2P_Macro_Data.csv", nrows = 1000)


