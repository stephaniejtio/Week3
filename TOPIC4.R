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
