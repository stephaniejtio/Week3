#TOPIC 4

install.packages("tidyverse")
library(tidyverse)
view(pew)

pew <- read.csv("/Users/stio/week 3/pew.csv")

pew_long <- pivot_longer(data = pew, 
                         col = -c('Religious.tradition','Sample.size'),
                         names_to = "income_level",
                         values_to = "sample_pc")
view(pew_long)



activities <- read_csv("/Users/stio/week 3/activities.csv")

ac_w1 <- pivot_longer(
  data = activities,
  cols = c(work.T1:talk.T2),
  names_to = "variable",
  values_to = "value"
)
view(ac_w1)

ac_w2 <- separate(data = ac_w1, col = variable, 
                  into = c("action", "time"))

activities_wider <-  pivot_wider(data = ac_w2, names_from = "action", 
                                 values_from = "value")
view(activities_wider)






  library(RPostgres)
  wrds <- dbConnect(Postgres(),
                    host='wrds-pgdata.wharton.upenn.edu',
                    port=9737,
                    dbname='wrds',
                    sslmode='require',
                    user='stephaniejtio',
                    password='Stefanitio88')


res <- dbSendQuery(wrds, "select gvkey,conm,fyear,revt,ni,at,dltt,teq 
                   from comp.funda
                   where fyear between '2000' and '2020'and exchg = '11' 
                   and datafmt = 'STD' and consol = 'C' and indfmt = 'INDL'")
nyse_df <- dbFetch(res, n=-1)
dbClearResult(res)
view(nyse_df)


#new way - dplyr
library(tidyverse)
library(dbplyr)

nyse_tbl<- tbl(wrds, sql("select * from comp.funda")) |>
  filter(fyear >= '2000' & fyear <= '2020', 
         exchg == '11',datafmt == 'STD',consol == 'C',indfmt == 'INDL') |>
  select(gvkey,conm,fyear,revt,ni,at,dltt,teq, naicsh) |> collect()
view (nyse_tbl)


#Subset Data based on specific values
nyse_tbl |> filter(fyear == '2005' , revt >='100')

nyse_tbl |> 
  filter(fyear == '2005', revt >= 100) |> 
  arrange(desc(at))

# $ end of the string
nyse_tbl |> 
  filter(fyear == '2005', revt >= 100,!(str_detect(naicsh, "^52"))) |> 
  arrange(desc(at))

#Change or Create Variables
nyse_ratio <- nyse_tbl |> 
  filter(!(str_detect(naicsh, "^52"))) |>
  mutate(npm = ni/revt, ato = revt/at, de = dltt/teq) |>
  arrange(fyear, desc(at))


#Variable Creation - Checks
nyse_tbl |> 
  filter(revt == 0,!(str_detect(naicsh, "^52"))) |> 
  arrange(desc(at))

nyse_ratio <- nyse_tbl |> 
  filter(!(str_detect(naicsh, "^52"))) |>
  mutate(npm = ni/revt, ato = revt/at, de = dltt/teq) |>
  mutate(npm  = replace(npm ,revt == 0,NA))|>
  arrange(fyear, desc(at))

nyse_ratio |> summarise(AvgRev = mean(revt,  na.rm = TRUE))


#median 
nyse_ratio |> summarise(MedianAssets = median(npm, na.rm = TRUE))

#The maximum value of debt to equity ratio (de)
nyse_ratio |> summarise(MaxDebtEquity = max(de, na.rm = TRUE))












nyse_ratio |> group_by(fyear) |> 
  summarise(AvgRev = mean(revt, na.rm = TRUE))



nyse_ratio |> filter(fyear =="2012") |> 
  mutate(ind_cd = str_extract(naicsh, "^.{2}"))|>
  group_by(ind_cd) |> 
  summarise(AvgNPM = mean(npm, na.rm = TRUE)) |>
  arrange(desc(AvgNPM))



nyse_ratio |>
  summarise(across(
    .cols = c(revt,ni, at, dltt, teq, npm, ato, de), 
    .fns = list(Mean = mean, Median = median, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))



Modes <- function(x, na.rm = FALSE) {
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

nyse_ratio |>
  summarise(across(
    .cols = c(revt, at, npm, ato, de), 
    .fns = list(Mean = mean, Median = median, 
                Mode = Modes, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) |> 
  pivot_longer(everything(), names_sep = "_", 
               names_to = c( "variable", ".value"))


#Managing Dates
#ymd("2000 Feb 24th")


#CHALLENGE TASK - 4 Sept 

# Load necessary libraries
library(tidyr)
library(dplyr)

#read csv 
datax <- read.csv("/Users/stio/week 3/RR20221230-001-SSDailyYTD.csv")
datax

#change col names to sec name and sec code
colnames(datax)[1:2] <- c("security_name", "security_code")

# Perform the gather operation
df_long <- datax %>%
  gather(key = "trade_date", value = "value", -security_name, -security_code)

# Split the 'value' column into 'short_volume' and 'short_percent'
df_long <- df_long %>%
  mutate(
    short_volume = as.numeric(sub(" .*", "", value)),  # Take everything before the space as short_volume
    short_percent = as.numeric(sub(".* ", "", value))  # Take everything after the space as short_percent
  )



# Drop the original 'value' column
df_long <- df_long %>%
  select(-value)

# Convert the 'short_volume' and 'short_percent' columns to numeric types
df_long <- df_long %>%
  mutate(
    short_volume = as.numeric(short_volume),
    short_percent = as.numeric(short_percent)
  )


# Drop rows with NA in short_volume or short_percent
df_long <- df_long %>%
  drop_na(short_volume, short_percent)

# Display the first few rows of the cleaned and tidy data
print(head(df_long, 10))

df_long <- df_long[-1, ]


# Rename columns if needed
colnames(df_long) <- c("security_name", "security_code", "trade_date", "short_volume", "short_percent")

df_long$trade_date <- dmy(gsub("X", "", df_long$trade_date))

# Convert short_volume and short_percent to numeric values (removing NA)
df_long$short_volume <- as.numeric(df_long$short_volume)
df_long$short_percent <- as.numeric(df_long$short_percent)

# Filter out rows with NA in short_volume or short_percent
df_long <- df_long %>% filter(!is.na(short_volume) & !is.na(short_percent))

# ##################################################################################


# Load necessary libraries
library(tidyr)
library(dplyr)
library(lubridate)


#read csv 
df <- read.csv("/Users/stio/week 3/RR20221230-001-SSDailyYTD.csv", skip = 1, header = TRUE)

# Convert wide format data into long format
df_long <- df %>%
  gather(key = "trade_date", value = "value", -security_name, -security_code)

# Separate the 'value' column into 'short_volume' and 'short_percent'
df_long <- df_long %>%
  separate(value, into = c("short_volume", "short_percent"), sep = " ", fill = "right")

# Convert 'short_volume' and 'short_percent' to numeric types
df_long <- df_long %>%
  mutate(
    short_volume = as.numeric(short_volume),
    short_percent = as.numeric(short_percent)
  )

# Convert 'trade_date' to a proper date format
df_long <- df_long %>%
  mutate(trade_date = dmy(gsub("X", "", trade_date)))

# Filter out rows with NA in short_volume or short_percent
df_tidy <- df_long %>%
  drop_na(short_volume, short_percent)

# Sort by the first column (security_name)
df_tidy <- df_tidy %>%
  arrange(security_name)
