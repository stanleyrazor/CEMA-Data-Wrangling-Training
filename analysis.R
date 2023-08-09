
# dir
setwd('/home/ncworkforce/Documents/CEMA')

# libs
require(pacman)
p_load(dplyr, stringr, lubridate, ggplot2, janitor, readxl, gridExtra,
       tidyr, tidyverse)


# data --------------------------------------------------------------------

# data dictionary
data_dict <- read_xlsx('data/sample_data/data dictionary_L4H_sample.xlsx')

# Individual household baseline
ib <- read.csv('data/sample_data/L4H_individual_baseline_sample.csv') |>
  clean_names()

# Household baseline sample
hb <- read_csv('data/sample_data/L4H_household_baseline_sample.csv', na='---') |>
  clean_names()

# Mother baseline sample
mb <- read.csv('data/sample_data/L4H_mother_baseline_sample.csv') |>
  clean_names()

# summary and structure
View(hb)
glimpse(hb)

# Filtering for eligible
hb_eligible <- hb |>
  filter(hh_eligible == '1')

# Merge the datasets
fd <- merge(ib, mb, by.x = 'number', by.y = 'number_0') |>
  merge(hb, by = 'household_id') |>
  select(!c('number.y'))

# Recoding variables
fd1 <- fd |> 
  mutate(
    reason_for_ineligibility = case_when(
      reason_for_ineligibility == '1' ~ 'No adult occupier >16 years',
      reason_for_ineligibility == '2' ~ 'Withdrawal',
      reason_for_ineligibility == '3' ~ 'Other Reason',
      TRUE ~ NA
    ),
    rspntgndr = ifelse(rspntgndr == '1', 'Male', 'Female'),
    maincme = case_when(
      maincme == '1' ~ 'Sale of livestock & livestock products',
      maincme == '2' ~ 'Sale of crops',
      maincme == '3' ~ 'Trading/business',
      maincme == '4' ~ 'Employment (salaried income)',
      maincme == '5' ~ 'Sale of personal assets',
      maincme == '6' ~ 'Remittance',
      maincme == '7' ~ 'Other',
      TRUE ~ NA
    ),
    rspndtmarital = case_when(
      rspndtmarital == '1' ~ 'Single',
      rspndtmarital == '2' ~ 'Married monogamous',
      rspndtmarital == '3' ~ 'Married polygmous',
      rspndtmarital == '4' ~ 'Divorced/ separated',
      rspndtmarital == '5' ~ 'Widow(er)',
      TRUE ~ NA
    ),
    h_hfrml_eductn = case_when(
      h_hfrml_eductn == '1' ~ 'Not completed Primary school',
      h_hfrml_eductn == '2' ~ 'Primary school',
      h_hfrml_eductn == '3' ~ 'Secondary school',
      h_hfrml_eductn == '4' ~ 'College/graduate',
      h_hfrml_eductn == '5' ~ 'Madrassa',
      h_hfrml_eductn == '6' ~ 'Other',
      TRUE ~ NA
      ),
    rspndt_edctn = case_when(
      rspndt_edctn == '1' ~ 'No formal education',
      rspndt_edctn == '2' ~ 'Primary School',
      rspndt_edctn == '3' ~ 'Secondary school',
      rspndt_edctn == '4' ~ 'College-graduate',
      rspndt_edctn == '5' ~ 'Madrassa',
      rspndt_edctn == '6' ~ 'Other',
      TRUE ~ NA
    )
  )


# Separation of the columns: lvstckown & ------------------------------------

fd2 <- fd1 |>
  separate(lvstckown,
           into = str_c('lvstckown_', 1:15),
           sep = ' ') |>
  separate(herdynamics,
           into = str_c('herdynamics_', 1:15))

missing_full <- apply(fd2, 2, function(z) sum(is.na(z))) |>
  as.numeric()

fd3 <- fd2[, -which(missing_full == nrow(fd2))]


# Adding study arm data ---------------------------------------------------

fd4 <- fd3 |>
  mutate(
    study_arm = 
      ifelse(village.x %in% c('Lependera', 'Gobb Arbelle', 'Nahgan-ngusa', 'Sulate', 
                              'Saale-Sambakah', 'Namarei', 'Manyatta Lengima',
                              'Lokoshula', 'TubchaDakhane', 'Rengumo-Gargule'), 
             yes = 'Study arm 1',
             no = ifelse(
               test = village.x %in% c('Galthelian-Torrder', 'Uyam village', 'Galthelan Elemo',
                                       'Nebey', 'Rongumo_kurkum', 'Urawen_Kurkum', 
                                       'Eisimatacho', 'Manyatta K.A.G', 'Ltepes Ooodo',
                                       'Lorokushu', 'Marti', 'Manyatta Juu West/East',
                                       'Lbaarok1'),
               yes = 'Study arm 2',
               no = 'Study arm 3'
             ))
  )


# Generating herd_dynamics ------------------------------------------------

herd_dynamics <- fd4 |>
  select(interview_date.x, household_id, study_arm, cwsbrth, shpbrth, goatsbrth,
         cmlsbrth, calves_death, bulls_death, cows_death, sheep_death,
         msheep_death, fsheep_death, goats_death, mgoats_death, fgoats_death,
         camels_death, mcamels_death, fcamels_death, cowsgft, sheepgfts, goatsgft,
         cmlsgft)

herd_dynamics1 <- herd_dynamics |>
  mutate(
    monthyear = paste0(year(interview_date.x), '-', month(interview_date.x))
  )

# species count:
num_cols <- c('cwsbrth', 'shpbrth', 'goatsbrth', 'cmlsbrth', 'calves_death', 
              'bulls_death', 'cows_death', 'sheep_death',
              'msheep_death', 'fsheep_death', 'goats_death', 'mgoats_death', 'fgoats_death',
              'camels_death', 'mcamels_death', 'fcamels_death', 'cowsgft', 'sheepgfts', 'goatsgft',
              'cmlsgft')

herd_dynamics2 <- herd_dynamics1 |>
  mutate(
    across(
      .cols = num_cols,
      .fns = function(z) ifelse(z == '---', NA, z) |> as.numeric()
    )
  ) |>
  group_by(study_arm, monthyear) |>
  summarise(
    across(
      .cols = num_cols,
      .fns = sum,
      na.rm = T
    )
  ) |>
  mutate(goats_death = goats_death + mgoats_death + fgoats_death,
         sheep_death = sheep_death + fsheep_death + msheep_death,
         camels_death = camels_death + mcamels_death + fcamels_death,
         cattle_death = cows_death + bulls_death + calves_death) |> 
  arrange(ym(monthyear))

  
herd_dynamics2 <- herd_dynamics1 |>
    mutate(
      across(
        .cols = everything(),
        .fns = function(z) ifelse(z == '---', NA, z)
      )
    ) |>
    pivot_longer(
      cols = contains('brth'),
      names_to = 'Birth', 
      values_to = 'BirthCount') |>
    pivot_longer(
      cols = contains('death'),
      names_to = 'Death', 
      values_to = 'DeathCount') |>
    pivot_longer(
      cols = contains('gft'),
      names_to = 'Gifted', 
      values_to = 'GiftedCount') |>
    mutate(
      BirthCount = as.numeric(BirthCount),
      DeathCount = as.numeric(DeathCount),
      GiftedCount = as.numeric(GiftedCount)
    ) 
herd_dynamics2 |>
    group_by(study_arm, monthyear) |>
    summarise(
      BirthTotal = sum(BirthCount, na.rm = T),
      DeathTotal = sum(DeathCount, na.rm = T),
      GiftedTotal = sum(GiftedCount, na.rm = T)
    ) |>
    ungroup() |>
    mutate(monthyear = ym(monthyear)) |>
    arrange(monthyear)
  
  
#   
#   Data Visualization Challenge
#   
#   Using the new wrangled dataset herd_dynamics from 1) above, develop 
# a single graphic to show the frequencies and changes over time
# in animal births, deaths, gifts in and given out for the different species
# 
# Hint 1 - you may need the data in long format (e.g., using function 
#                                                pivot_longer)

# Hint 2 - geom_col() function in ggplot2 package and facet_grid() 
# may be useful . Use monthyear as the x-axis and fill color to show 
# the different animal species
  

herd_dynamics3 <- herd_dynamics2 |>
  select(-c(msheep_death, fsheep_death, mgoats_death, fgoats_death,
            bulls_death, calves_death, cows_death, fcamels_death, 
            mcamels_death)) |>
  pivot_longer(
    cols = contains(c('death', 'brth', 'gft')),
    names_to = 'animal',
    values_to = 'measure'
  ) |>
  mutate(
    event = ifelse(
      test = str_detect(animal, 'brth'),
      yes = 'Birth',
      no = ifelse(
        test = str_detect(animal, 'death'),  
        yes = 'Death',
        no = 'Gifted'
      )
    ),
    animal = case_when(
      animal == 'camels_death' ~ 'Camel',
      animal == 'cattle_death' ~ 'Cattle',
      animal == 'goats_death' ~ 'Goats',
      animal == 'sheep_death' ~ 'Sheep',
      animal == 'cmlsbrth' ~ 'Camel',
      animal == 'cwsbrth' ~ 'Cow',
      animal == 'goatsbrth' ~ 'Goats',
      animal == 'shpbrth' ~ 'Sheep',
      animal == 'cowsgft' ~ 'Cow',
      animal == 'goatsgft' ~ 'Goats',
      animal == 'sheepgfts' ~ 'Sheep',
      animal == 'cmlsgft' ~ 'Camel'
    )
  ) 

herd_dynamics3 |>
  ggplot()+
  geom_col(aes(x = monthyear, y = measure, fill = animal),
           position = 'dodge')+
  labs(title = 'Animal statistics across time',
       x = 'Date', y = 'Count')+
  #coord_flip()+
  theme_bw()+
  facet_grid(event~.)

herd_dynamics3 |>
  ggplot()+
  geom_col(aes(x = ymd(paste0(monthyear, '-01')), y = measure, fill = animal),
           position = 'dodge')+
  labs(title = 'Animal statistics across time',
       x = 'Date', y = 'Count')+
  coord_flip()+
  theme_bw()+
  facet_grid(~event)

