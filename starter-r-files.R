
# creating the object
object1 <- c(1,2,3,4,5,6,7,8,9,10)

# computing the mean
mean(object1)

# computing the median
median(object1)

# 1 qu, 3 qu
quantile(object1, .25)
quantile(object1, .75)

# min, max
min(object1)
max(object1)

# oveall
summary(object1)

# installing packages
# install.packages('epitools') #  make sure to install

# loading the package
library(tidyverse)

# data
dat1 <- read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

# head and tail
head(dat1)
tail(dat1)

# str - structure
str(dat1)

# check the class and data types:
# data types
# integer, numeric, character, factor, dates.
class(dat1)
class(dat1$admit)
class(dat1$rank)

# mean of a specific column
mean(dat1$admit) # the mean of admit
mean(dat1$rank)  # the mean of rank


# Modern R ----------------------------------------------------------------

# recoding using pipe |> (0: deny, 1: accept)
dat2 <- dat1 |> # 'and then...'
  mutate(admit=recode(admit, '0'='deny', '1'='accept'))
  
head(dat2)

# rename columns
dat2 |>
  setNames(c('Admission', 'GRE', 'GPA', 'RANK'))

dat3 <- dat2 |>
  rename(position = rank)
head(dat3)

# dataset
ideal1 <- read.csv('https://raw.githubusercontent.com/cema-uonbi/R_course/main/ideal1.csv')

# summary and structure
summary(ideal1)
str(ideal1)

# data cleaning
# change calf_date_of_birth column name,  change the data type
# CalfSex to Gender-labels(1-male,2-female)

ideal1b <- ideal1 |>
  rename(calf_date_of_birth = CADOB) |>
  mutate(CalfSex = recode(CalfSex, '1'='Male', '2'='Female'),
         calf_date_of_birth = as.Date(calf_date_of_birth, '%d/%m/%Y'))
                              # dmy(calf_date_of_birth)

# column names in R:
# good habits: use snake_case, CamelCase
colnames(ideal1)

ideal1 |>
  janitor::clean_names()

# NA Values:

# date formats:
# 1-31: d
# 01-31: D
# 1-12: m
# 01-12: M
# Jan-Dec: b
# January-December: B
# 21-50: y
# 2021-2050: Y

# dates in R
library(lubridate)

ideal1 |>
  rename(calf_date_of_birth = CADOB) |>
  mutate(CalfSex = recode(CalfSex, '1'='Male', '2'='Female'),
         calf_date_of_birth = dmy(calf_date_of_birth)) |>
         arrange(calf_date_of_birth)
# arranging by date

class(ideal1b$calf_date_of_birth)

# Subsetting:
(ideal1b$sublocation) |> table() |> barplot()

# using subset function
idealkidera <- subset(ideal1b, ideal1b$sublocation == 'Kidera')

# using filter
idealkidera <- ideal1b |>
  filter(sublocation == 'Kidera')


# Dataset 2 ---------------------------------------------------------------

ideal2 <- read.csv('https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal2.csv')

ideal2a <- ideal2 |>
  mutate(VisitDate = dmy(VisitDate)) |>
  arrange(VisitDate)

# summary and str
View(ideal2)
summary(ideal2)
glimpse(ideal2)

class(ideal2$VisitDate)
class(ideal2a$VisitDate)


# merging: left, right, full, inner join ----------------------------------

# left
a=merge(ideal1, ideal2, by = 'CalfID', all.x = T) 
ideal3 <- left_join(x = ideal1, y = ideal2, by='CalfID')

# right
merge(ideal1, ideal2, by = 'CalfID', all.y = T) 
right_join(ideal1, ideal2, by = 'CalfID')

# full join
merge(ideal1, ideal2, by = 'CalfID', all=T)     # full
full_join(ideal1, ideal2, by='CalfID')

# inner
inner_join(ideal1, ideal2, by='CalfID')
merge(ideal1, ideal2, by = 'CalfID', all = F)

# subset columns using select
ideal3 |>
  dplyr::select(CalfID, VisitID, VisitDate, Theileria.spp.,
                ELISA_mutans, ELISA_parva, Q.Strongyle.eggs)


# pivot: make data longer -------------------------------------------------

ideal3a <- ideal3 |>
  dplyr::select(CalfID, VisitID, VisitDate, Theileria.spp.,
                ELISA_mutans, ELISA_parva, Q.Strongyle.eggs) |>
  pivot_longer(cols = c('Theileria.spp.', 'ELISA_mutans', 'ELISA_parva',
                                 'Q.Strongyle.eggs'),
             names_to = 'tests', 
             values_to = 'outcome')
# use when those columns have similar values - like the elisa columns

View(ideal3a)

# Grouping data: group_by - function
ideal3 |>
  group_by(CalfID) |>
  summarise(
    min = min(Weight,na.rm = T),
    mean = mean(Weight,na.rm = T),
    max = max(Weight,na.rm = T))

ideal3c <- ideal3 |>
  group_by(CalfID) |>
  mutate(avrg_weight = mean(Weight, na.rm = T)) |>
  select(CalfID, Weight, avrg_weight)

# Test --------------------------------------------------------------------

dogdemography <- read.csv('https://raw.githubusercontent.com/cema-uonbi/R_course/main/DogcohortDemographics.csv')

# Data wrangling ------
# 1. Rename columns
# 2. Recode columns
# 3. 

# type conversion
dogdemography <- dogdemography |>
  mutate(IntDate = mdy(IntDate),
         VillageID = as.character(VillageID),
         OwnDogs = recode(OwnDogs, '0'='no', '1'='yes') |> as.factor(),
         DogDied = recode(DogDied, '0'='no', '1'='yes') |> as.factor(),
         DogBite = recode(DogBite, '0'='no', '1'='yes') |> as.factor()
         ) |>
  setNames(
    c("InterviewDate", "HouseholdID", "VillageID", "HouseholdMembers", "ownDogs", 
      "numDogsOwned", "AdultDogsOwned", "puppiesOwned", "dogDiedPastMonth",
      "numDogsDiedPastMonth", "dogBitesPastMonth"
    )
  )
# ifelse(test, yes, ifelse(test, yes, no))


View(dogdemography)
glimpse(dogdemography)

# Questions ------------------------------------

# average number of household members per village
dogdemography |>
  group_by(VillageID) |>
  summarise(
    min = min(HouseholdMembers,na.rm = T),
    mean = mean(HouseholdMembers,na.rm = T) |> as.integer(),
    max = max(HouseholdMembers,na.rm = T))

# average number of dogs owned per village
dogdemography |>
  group_by(VillageID) |>
  summarise(
    AverageNumDogs = mean(numDogsOwned) |> round(0)
  )


# Test 2 ------------------------------------------------------------------


ideal <- read.csv('https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv')

glimpse(ideal)
View(ideal)

table(ideal$ReasonsLoss, ideal$ReasonsLoss1)

ideal |>
  ggplot(aes(x = ReasonsLoss1))+
  geom_bar(width = .5, col = 'gray1', fill = 'gray1', alpha = .2, lwd=1.2)+
  labs(title = 'Reasons Lost to Study',
       x = 'Calf status at End of Day',
       y = 'Number of Calves (Total = 270)')+
  coord_flip()+
  theme_bw()

# Plot the number of calves per subloc
ideal |>
  group_by(sublocation, CalfID) |>
  count()

ideal |>
  group_by(sublocation) |>
  summarise(freq = n())

ideal |>
  group_by(sublocation) |>
  summarise(freq = n()) |>
  ggplot(aes(x = reorder(sublocation, freq)))+
  geom_col(aes(y = freq),
           col = 'darkslategray', fill = 'darkslategray', alpha = .4)+
  geom_text(aes(y = freq-.8, label = freq), col='black')+
  labs(title = 'Calves population per sublocation',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  theme_classic()


# unique calves subloc
ideal |>
  select(CalfID, sublocation) |>
  distinct() |>
  group_by(sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n),
           col = 'darkslategray', fill = 'darkslategray', alpha = .4)+
  labs(title = 'Calves population per sublocation',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  theme_classic()

# Plotting unique calves by gender:
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n, fill = CalfSex))+
  labs(title = 'Calves population per sublocation',
       x = 'Sub-Location', y = 'Number of calves')+
  # scale_fill_manual(values = c('yellow', 'pink'))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  theme_classic()

# dumbell plot
install.packages('ggalt')
  
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n))+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  facet_grid(.~CalfSex, scales = 'free_y')+
  theme_bw()

# using manual color scale
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n))+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  facet_grid(CalfSex~., scales = 'free_y')+
  theme_bw()

# adding colors:
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n, fill=CalfSex),
           show.legend = F)+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  facet_grid(cols = vars(CalfSex))+
  theme_bw()

# adding colors:
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n),
           fill = 'red')+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  facet_grid(cols = vars(CalfSex))+
  theme_bw()

# facet wrap
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, n), y = n),
           fill = 'thistle4')+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  facet_wrap(~CalfSex)+
  theme_light()



# using proportions
ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  group_by(sublocation) |>
  mutate(proportion = n/sum(n)) |>
  ungroup() |>
  ggplot()+
  geom_col(aes(x = reorder(sublocation, proportion), 
               y = proportion, fill=CalfSex))+
  #geom_text(aes(x = proportion-.08, label = proportion), col='black')+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = 'Calves population per sublocation - by gender',
       x = 'Sub-Location', y = 'Number of calves')+
  coord_flip()+
  theme_classic()


ideal |>
  select(CalfSex, sublocation) |>
  mutate(CalfSex = ifelse(CalfSex == 1, 'Male', 'Female')) |>
  group_by(CalfSex, sublocation) |>
  count() |>
  ungroup() |>
  group_by(sublocation) |>
  mutate(proportion = n/sum(n)) |>
  ungroup() |>
  ggplot()+
  geom_mosaic(aes(x = product(CalfSex,
                              sublocation), fill = proportion))



# ------------------------------------
# seperate one columns into several columns:

dogdemography |>
  select(InterviewDate) |>
  separate(InterviewDate, into = c('y',  'm', 'd'),
           sep = '-')

dogdemography |>
  select(InterviewDate) |>
  transmute(
    y = year(InterviewDate),
    m = month(InterviewDate),
    d = day(InterviewDate)
  )

# joining back
dogdemography |>
  select(InterviewDate) |>
  separate(InterviewDate, into = c('y',  'm', 'd'),
           sep = '-') |>
  mutate(InterviewDate = paste0(y, '-', m, '-', d))



