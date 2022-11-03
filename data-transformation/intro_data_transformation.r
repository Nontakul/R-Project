# install.packages("glue")

library(tidyverse) 
library(sqldf)
library(jsonlite)
library(RSQLite)
library(RPostgreSQL)
library(glue)

## Data Transformation +50% upskill
## dplyr => grammar of data manipulation

View(mtcars)

## 5 functions
## 1. select() => SELECT
## 2. filter() => WHERE
## 3. arrange() => ORDER BY
## 4. mutate() => SELECT .. AS newColumn
## 5. summarise() + group_by()

select(mtcars, 1:5)

select(mtcars, 
       mpg, wt, hp, 
       10:11)

select(mtcars, starts_with("h"))
select(mtcars, ends_with("p"))
select(mtcars, contains("a"))
select(mtcars, carb, everything())

# create new column
mtcars$model <- rownames(mtcars)
rownames(mtcars) <- NULL
mtcars <- select(mtcars, model, everything())

## Data Pipeline %>% (Pipe operator)
m1 <- mtcars %>%
    select(mpg, hp, wt) %>%
    filter(hp < 100 | wt < 2) %>%
    arrange(desc(hp))

## filter data == WHERE SQL
mtcars %>%
  select(model, mpg, hp, wt, am) %>%
  filter( between(mpg, 25, 30) ) # between

mtcars %>%
  select(model, cyl) %>%
  filter( cyl %in% c(6,8) )
  
mazda_data <- mtcars %>%
  select(model, mpg, hp, wt, am) %>%
  filter(grepl("^Mazda", model))

## mutate() create new column
m2 <- mtcars %>%
  select(model, mpg, hp, wt, am) %>%
  mutate(hp_segment = if_else(hp<100, "low", "high"),
         hp_segment2 = case_when(
           hp < 100 ~ 'low',
           hp < 200 ~ 'medium',
           TRUE ~ 'high'
         ))

## glimpse data structure
glimpse(mtcars)

## am => 0=Auto, 1=Manual
mtcars <- mtcars %>%
  mutate(am = if_else(am == 0, "Auto", "Manual"),
         vs = if_else(vs == 0, "V-Shaped", "Straight"))

View(mtcars)

## count
mtcars %>%
  count(am)

mtcars %>%
  count(vs)

m3 <- mtcars %>%
  count(am, vs) %>%
  mutate(percent = n/ nrow(mtcars))

## Read Write CSV Files
library(tidyverse)
write_csv(m3, "summary_mtcars.csv")
m3 <- read_csv("summary_mtcars.csv")


## Change Data Types
mtcars <- mtcars %>%
  mutate(vs = as.factor(vs),
         am = as.factor(am)) 

## summarise() + group_by()
mtcars %>%
  group_by(am, vs) %>%
  summarise(
    avg_mpg = mean(mpg),
    sum_mpg = sum(mpg),
    min_mpg = min(mpg),
    max_mpg = max(mpg),
    var_mpg = var(mpg),
    sd_mpg  = sd(mpg),
    median_mpg = median(mpg),
    n = n()
  )

## JOIN TABLES
band_members
band_instruments

inner_join(band_members,
           band_instruments,
           by = "name")

left_join(band_members,
          band_instruments,
          by = "name")

right_join(band_members,
          band_instruments,
          by = "name") 

full_join(band_members,
          band_instruments,
          by = "name")

## refactor
band_members %>%
    full_join(band_instruments, 
              by="name") %>%
    filter(name %in% c("John", "Paul")) %>%
    mutate(hello = "OK")

## library load package
library(nycflights13)

glimpse(flights)

flights %>%
  filter(month == 9) %>%
  count(origin, dest)

### carriers most flights 
### in March-May 2013
### origin == JFK

df <- flights %>%
  filter(origin == "JFK" & 
         month %in% c(3,4,5)) %>%
  count(carrier) %>%
  arrange(desc(n)) %>%
  left_join(airlines, by="carrier")

write_csv(df, "requested_data.csv")

## Mock up data
## One-to-One
student <- data.frame(
    id = 1:5,
    name = c("toy","joe","anna","mary","kevin"),
    cid = c(1,2,2,3,2),
    uid = c(1,1,1,2,2)
)

course <- data.frame(
    course_id = 1:3,
    course_name = c("Data", "R", "Python")
)

university <- data.frame(
    uid = 1:2,
    uname = c("University of London", "Chula University")
)

## JOIN more than two tables
student %>%
    left_join(course, by = c("cid" = "course_id")) %>%
    left_join(university, by = "uid")

## Wide -> Long format transformation
long_worldphones <- WorldPhones %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Year") %>%
    pivot_longer(N.Amer:Mid.Amer, 
                 names_to = "Region",
                 values_to = "Sales")

long_worldphones %>%
  filter(Region == "Asia")

long_worldphones %>%
  group_by(Region) %>%
  summarise(total_sales = sum(Sales))


## long -> wide format
wide_data <- long_worldphones %>%
    pivot_wider(names_from = "Region",
                values_from = "Sales")

write_csv(wide_data, "data.csv")












## Connect SQL databse
## 1. SQLite
## 2. PostgreSQL server
library(RSQLite)
library(RPostgreSQL)

## steps to connect database
## create connection > query > close con

con <- dbConnect(SQLite(), "chinook.db")

dbListTables(con)
dbListFields(con, "customers")

dbGetQuery(con, "
         SELECT 
           firstname, 
           lastname, 
           country 
         FROM customers
         WHERE country 
           IN ('France', 'Austria','Belgium') ")

query01 <- "
  SELECT * FROM artists
  JOIN albums ON artists.artistid = albums.artistid
  JOIN tracks ON tracks.albumid = albums.albumid
"

tracks <- dbGetQuery(con, query01)
View(tracks)

tracks %>%
    select(Composer, Milliseconds, Bytes, UnitPrice) %>%
    filter(Milliseconds > 200000,
           grepl("^C", Composer)) %>%
    summarise(
      sum(Bytes),
      sum(UnitPrice)
    )

dbDisconnect(con)

## sample data n=10
library(janitor)
tracks_clean <- clean_names(tracks)

set.seed(15)
tracks_clean %>%
    select(1:2) %>%
    sample_n(10)


### R connect to PostgreSQL
## username, password, host (server), port, dbname
library(RPostgreSQL)
con <- dbConnect(PostgreSQL(),
                 user = "pdvecmfz",
                 password = "09xtSsKXybZTd_QiajJo5FY1V1FI950a",
                 host = "rosie.db.elephantsql.com",
                 port = 5432,
                 dbname = "pdvecmfz")

dbListTables(con)

course <- data.frame(
  id = 1:3,
  name = c("Data Science", "Software", "R")
)

dbWriteTable(con, "course", course)
dbListTables(con)

dbDisconnect(con)












