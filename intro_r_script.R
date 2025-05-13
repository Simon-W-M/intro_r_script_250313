#  call tidyverse library
install.packages("tidyverse")
library(tidyverse)

# import data
library(readr)
capacity_ae <- read_csv("capacity_ae.csv")
View(capacity_ae)

# import messy data
beds_data <- read_csv("beds_data.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                      skip = 3)
View(beds_data)

# pipe operator |>  or %>% means THEN

# Q1. Which organisation provided the highest number of Mental Health beds?

# arrange
beds_data |>
  arrange(beds_av)

# arrange but in decending order

beds_data |>
  arrange(desc(beds_av))

# alternative way using maths
beds_data |>
  arrange(-beds_av)

#  Q2. Which 2 organisations provided the highest number of MH beds in September 2018?

# add a filter for date
beds_data |>
  arrange(desc(beds_av)) |>
  filter(date == '2018-09-01')

# add a filter on another column by operator
beds_data |>
  filter(date ==  max(date)) |>
  arrange(desc(beds_av)) |>
  filter(beds_av > 700) 

# add a slice to get the actual top 2
beds_data |>
  arrange(desc(beds_av)) |>
  filter(date == '2018-09-01') |>
  slice_head(n = 2)

# Q3. Which organisations had the highest percentage bed occupancy in September 2018?

# mutate a new column into the data
# filter on the new column
beds_data |>
  filter(date == '2018-09-01',
         org_name == 'Devon Partnership') |>
  mutate(perc_occ = occ_av / beds_av) |>
  arrange(desc(perc_occ))

# Q4. What was the mean number of beds (for the dataset)?

# calculate the mean
# note the na.rm = T, because we have nulls, R will give spurious result without it
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE))

# can add additional summaries such as median
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE))

# add sum total
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE))

# now we do summary and a group by date
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE),
            .by = date)

# or we could group by org code
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE),
            .by = org_code)

# demo of an older version of group_by 
# having it as a seperate function
beds_data |>
  group_by(date) |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE)) |>
  ungroup()

# a more dynamic filter to filter by each orgs codes max date
# kinda gives us latest data per organisaion
beds_data |>
  filter(date ==  max(date),
         .by = org_code) 


# Q5. Which organisations have the highest mean % bed occupancy?

# need to create total beds and occupancy by org with summary
# then mutate this to create percentage
# then arrange
beds_data |>
  summarise(total_beds = sum(beds_av, na.rm = TRUE),
            total_occupancy = sum(occ_av, na.rm = TRUE),
            .by = org_name) |>
  mutate(perc_occ = total_occupancy/ total_beds) |>
  arrange(desc(perc_occ))

# question is a little ambigious, but mean works as well
beds_data |>
  summarise(total_beds = mean(beds_av, na.rm = TRUE),
            total_occupancy = mean(occ_av, na.rm = TRUE),
            .by = org_name) |>
  mutate(perc_occ = total_occupancy/ total_beds) |>
  arrange(desc(perc_occ))


beds_data  |>
  summarise(sum_beds = sum (beds_av, na.rm = TRUE),
            sum_occ = sum (occ_av, na.rm = TRUE),
            .by=org_name) |>
  mutate(perc_occ= sum_occ/sum_beds) |>
  arrange(desc(perc_occ))
                      

beds_data |>
  summarise(sum_beds = sum(beds_av, na.rm = TRUE),
            sum_occ = sum(occ_av, na.rm = TRUE),
            .by = org_name) |>
  mutate(perc_occ = sum_occ/sum_beds) |>
  arrange(desc(perc_occ))


# selecting a couple of columns
beds_data |>
  select(org_code,
         org_name)

# selects everything apart from org_code
beds_data |> 
  select(-org_code)

# select reorders columns
beds_data |>
  select(org_name,
         org_code)

# select org_name and then everything
beds_data |>
  select(org_name,
         everything())

# select starts with
beds_data |>
  select(starts_with('org'))

# select ends with
beds_data |>
  select(ends_with('av'))

# select contains
beds_data |>
  select(contains('s_a'))

# add in count and count of distinct names
beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE),
            count = n(),
            distinct_names = n_distinct(org_name),
            .by = org_code) |>
  arrange(desc(distinct_names))

# checking if RDE does in fact have multiple org names
beds_data |>
  filter(org_code == 'RDE') |>
  distinct(org_name)


# create objects
# use the assignment operator <-
bed_occupancy <- beds_data |>
  summarise(mean_beds = mean(beds_av, na.rm = TRUE),
            median_beds = median(beds_av, na.rm = TRUE),
            total_beds = sum(beds_av, na.rm = TRUE),
            count = n(),
            distinct_names = n_distinct(org_name),
            .by = org_code) |>
  arrange(desc(distinct_names))

# overwrite a dataframe with filter by assigning it to itself
bed_occupancy <- bed_occupancy |> 
  filter(median_beds >0)

# create a vector
# a variable with multiple values
org_lookup <- c("Bradford District Care", "Bradford District Care Trust")

# we can use this in several ways, such as in a filter
beds_data |>
  filter(org_name %in% org_lookup)

# create a single variable
org_lookup_single  <- "Bradford District Care"

# use that in a filter
beds_data |>
  filter(org_name == org_lookup_single)

# negative 'in' - filter all those apart from the lookup
beds_data |>
  filter(!org_name %in% org_lookup)

# or can use a != to denote not equals to
beds_data |>
  filter(org_name != org_lookup_single)


## joining data 

# read in the data
tb_cases <- read_csv("tb_cases.csv")
tb_new_table <- read_csv("tb_new_table.csv")
tb_pop <- read_csv("tb_pop.csv")

# a join on one feature, *creates duplicates
tb_joined <- tb_cases |>
  left_join(tb_pop,
            by = "country")

# join on two features
tb_joined <- tb_cases |>
  left_join(tb_pop,
            by = c("country", "year"))

# more 'modern' way of joining - does not require vector or quotations
tb_joined_alt <- tb_cases |>
  left_join(tb_pop,
            join_by(country, year))

# auto join searches for matching keys - 
# personally I prefer to be explicit we me joins
tb_cases |>
  left_join(tb_pop)

# a join where variables match but are called different things
tb_joined_new <- tb_cases |>
  left_join(tb_new_table,
            join_by(country == Place, 
                    year == Year))

# create a sub table of just those with first letter A
lookup_table <- tb_new_table |>
  filter(FirstLetter == 'A')

# do a filtering semi join so that it only 
# joins and returns those that start with A
tb_joined_new <- tb_cases |>
  semi_join(lookup_table,
            join_by(country == Place, 
                    year == Year))

# anti join, filtering join
# joins and returns those that dont start with A
# really useful to identify on matches in datasets
tb_joined_new_anti <- tb_cases |> 
  anti_join(lookup_table, 
            join_by(country == Place, 
                    year == Year))

# introduction to styler package
install.packages("styler")

# make messy code and highlight and use addin to tidy
tb_joined_new_anti <- tb_cases |>
  anti_join(
    lookup_table,
    join_by(country==Place, year == Year)
  )


# showing examples of inbuilt datasets and help functions
mtcars

?mean  # shows function help
??mean # shows the library help

# you can also click on a function and press f1

# or use the help tab and manually search

mean(mtcars$mpg)

# example of mutate function taken from the mutate help screen
starwars %>%
  select(name, mass) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )
