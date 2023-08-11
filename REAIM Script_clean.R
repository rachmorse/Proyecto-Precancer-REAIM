#### ANALYSIS FOR REACH DATA ###

#TLDR 
#this script contains a number of barcharts 
#dividing HPV, VIA & Pap screening data over time by rural, periurban, urban

#packages
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}

#load data
getwd()  
setwd("/Users/rachelmorse/Documents/2021:2022/Proyecto Precáncer/REAIM")
library(readxl)
 
#This data is in the lucidchart F_REACH_BB tab: https://lucid.app/lucidchart/c9da3806-e98d-4190-a2c0-785571349302/edit?invitationId=inv_dd5f65df-8d46-42f6-b7ec-0cd99215e3a4&page=EX3M9d0i50y5#
data <- read_excel("/Users/rachelmorse/Documents/2021:2022/Proyecto Precáncer/REAIM/Adoption data.xlsx") 

#format dates correctly
data$dates <- format(as.Date(data$dates), "%y/%m")

#CURRENT BARCHART HPV&PAP/VIA TESTS PRESENTED AS PERSENTAGE SCREEN ELIGIBLE WOMEN SCREENED / MONTH ----

# Define the population sizes for each location
rural_population <- 3374
periurban_population <- 6549
urban_population <- 10140

#determine what percentage of the total population should be screened each month to meet HPV and Pap/VIA goals
# HPV goal = number of screen eligible women / 3 years / 12 months * 0.7 (for 70% of goal)
# Pap/VIA goal = number of screen eligible women in area / 5 years / 12 months * 0.7 (for 70% of goal)
# Percent of women to screen each month to meet Pap/VIA goal = (number of screen eligible women in area / 5 years / 12 months * 0.7 for 70% of goal) / total population * 100

#I used rural pop below as the percentages for the goals are the same regardless of area

papvia_goal = (rural_population/3/12)*0.7

papvia_percent = papvia_goal/rural_population*100
head(papvia_percent)

hpv_goal = (rural_population/5/12)*0.7

hpv_percent = hpv_goal/rural_population*100
head(hpv_percent)

#summary = screen 1.944444% of women women with Pap or VIA each month and screen 1.166667% of women each month with HPV 
#to meet goal of screening 70% of women with screening test

#normalize the data by population to be able to compare changes by area
#take the total number of tests (HPV+Pap+VIA) divided by population * 100
#gives you percentage of the screen-eligible population screened each month by location

data <- data %>%
  mutate(rural_tests = `Rural VIA` + `Rural PAP` + `Rural HPV`) %>%
  mutate(periurban_tests = `Periurban VIA` + `Periurban PAP` + `Periurban HPV`) %>%
  mutate(urban_tests = `Urban VIA` + `Urban PAP` + `Urban HPV`) %>%
  mutate(rural_tests_bypop = (rural_tests / rural_population*100)) %>%
  mutate(periurban_tests_bypop = (periurban_tests / periurban_population*100)) %>%
  mutate(urban_tests_bypop = (urban_tests / urban_population*100))

subset_bypop <- data[, c("rural_tests_bypop", "periurban_tests_bypop", "urban_tests_bypop", "dates")]

# Reshape the data from wide to long format and rename variables
data_long_bypop <- subset_bypop %>%
  pivot_longer(cols = -dates, names_to = "Location", values_to = "Number of Tests") %>%
  mutate(Location = case_when(
    startsWith(Location, "rural") ~ "Rural",
    startsWith(Location, "periurban") ~ "Periurban",
    startsWith(Location, "urban") ~ "Urban",
    TRUE ~ Location
  ))

# Create the bar chart 
#unhash to add regression lines
plot_bar_chart <- ggplot(data_long_bypop, aes(x = dates, y = `Number of Tests`, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Date", y = "Percentage of Screen Eligible Population Screened per Month", fill = "Location") +
  ggtitle("Tests by Location") +
  theme_minimal() +
  scale_fill_manual(values = c("Rural" = "#C74B50", "Periurban" = "#0AA1DD", "Urban" = "#F8B400")) +  # Assign specific colors to locations
  scale_x_discrete(labels = function(x) ifelse(x %in% c(first(x), last(x)), x, "")) +  # Keep only first and last date values
  geom_smooth(aes(group = Location, color = Location), method = "lm", se = FALSE) +  # Add line of best fit with matching color
  scale_color_manual(values = c("Rural" = "#E97777", "Periurban" = "#89C4E1", "Urban" = "#FFD495")) +  # Assign specific colors to best fit lines
  geom_vline(xintercept = c(which(data$dates == "19/07")), linetype = "dashed", color = "black") + # Add vertical dotted line
  geom_hline(yintercept = c(1.166667, 1.944444), linetype = "solid", color = "grey") +  # Add dashed lines
  geom_text(aes(x = 0.5, y = 1.166667, label = "HPV Screening Goal"), color = "black", vjust = -1, hjust = 0, size = 3) +  # Add text for HPV Screening Goal
  geom_text(aes(x = 0.5, y = 1.944444, label = "Pap/VIA Screening Goal"), color = "black", vjust = -1, hjust = 0, size = 3)   # Add text for Pap/VIA Screening Goal

ggsave("REAIM_bar_chart.png", plot_bar_chart, width = 8, height = 6, units = "in", dpi = 300)

# TO SEE THE SLOPES
# Create a new variable 'date_order' to change the date variable from a character to numeric 
#this reassigns the date variable a number from 1-26 where Jan 2018 = 1 and Feb 2020 = 26 
#in other words it number the months as month 1, month 2, month 3, etc

data_long_bypop <- data_long_bypop %>%
  mutate(dates_numeric = rep(1:(nrow(data_long_bypop) %/% 3), each = 3)) #its by groups of 3 to account for the data being split by 3 locations

# Slice the data to by location
data_urban <- data_long_bypop %>%
  filter(Location == "Urban")

data_periurban <- data_long_bypop %>%
  filter(Location == "Periurban")

data_rural <- data_long_bypop %>%
  filter(Location == "Rural")


# Calculate slopes for each location using lm
slope_rural <- lm(`Number of Tests` ~ dates_numeric, data = data_rural)
slope_periurban <- lm(`Number of Tests` ~ dates_numeric, data = data_periurban)
slope_urban <- lm(`Number of Tests` ~ dates_numeric, data = data_urban)


summary(slope_rural) #slope = 0.03269
summary(slope_periurban) #slope = 0.053310
summary(slope_urban) #slope = 0.027398

#### ANALYSIS FOR IMPLEMENTATION DATA - SELF SAMPLE vs CLINICIAN ####----

#This data is here: https://docs.google.com/spreadsheets/d/1T2wY3viu5sesj0Y_jL8SDAXpjGN-h8oE/edit?usp=sharing&ouid=102523393703592875338&rtpof=true&sd=true
df <- read_excel("/Users/rachelmorse/Documents/2021:2022/Proyecto Precáncer/REAIM/Data (TVT-TA, self sampling).xlsx") 
attach(df)

hc <- (`Establec. Salud`)
type <- (`TAM_TIPOMUESTRA_A`) #1 is autotoma and 2 is obstetra

# HEALTH CENTER STRATIFICATION 

#group 1 = urban
#group 2 = periurban
#group 3 = rural
#group 4 = unknown

df <- df %>% 
  mutate(hc_rurality = case_when((hc == "San Juan") ~ 1,
                                 (hc == "Progreso") ~ 1,
                                 (hc == "América") ~ 1,
                                 (hc == "Modelo") ~ 1,
                                 (hc == "ProgresoSan Juan") ~ 1, #1 participant had 2HCs but they are both urban
                                 (hc == "Peña Negra") ~ 3,
                                 (hc == "San Pablo de Cuyana") ~ 3,
                                 (hc == "Moralillo") ~ 3,
                                 (hc == "Cahuide") ~ 3,
                                 (hc == "Villa Buen Pastor") ~ 3,
                                 (hc == "Paujil") ~ 3,
                                 (hc == "Quistococha") ~ 2,
                                 (hc == "San JuanQuistococha") ~ 2, #making the assumption that if she put Quistococha thats the main one other than for referrals
                                 (hc == "Rumococha") ~ 2,
                                 (hc == "Delfines") ~ 2,
                                 (hc == "Zungarococha") ~ 2,
                                 (hc == "Varillal") ~ 2,
                                 (hc == "Santo Tomás") ~ 2,
                                 (hc == "Santa Clara") ~ 2,
                                 TRUE ~ 4))

## description of autotoma vs obstetra ##
table(type)
typetable <- table(type)
prop.table(typetable)

## description of autotoma vs obstetra drop NAs ##
df_noNA <- filter(df, type > 0)
attach(df_noNA)
df_noNA$hc <- (`Establec. Salud`)
df_noNA$type <- (`TAM_TIPOMUESTRA_A`)

table(df_noNA$type)
typetable_noNA <- table(df_noNA$type)
prop.table(typetable_noNA)

#stratify data by health center location

if (!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}

attach(df)
sjPlot::tab_xtab(var.row = hc_rurality, 
                 var.col = type, 
                 title = "HPV Test Type by Rurality", 
                 show.row.prc = TRUE)

#remove missing data 
sjPlot::tab_xtab(var.row = df_noNA$hc_rurality, 
                 var.col = df_noNA$type, 
                 title = "HPV Test Type by Rurality", 
                 show.row.prc = TRUE)

# Replace the numeric values with corresponding labels
df_noNA <- df_noNA %>%
  mutate(hc_rurality = recode(hc_rurality,
                              `1` = 'Urban',
                              `2` = 'Periurban',
                              `3` = 'Rural')) %>%
  mutate(type = recode(type,
                       `1` = 'Self-Sample',
                       `2` = 'Clinician Sample'))

# Create the crosstab using tab_xtab
tab_xtab(var.row = df_noNA$hc_rurality, 
         var.col = df_noNA$type, 
         title = "HPV Test Type by Rurality", 
         show.row.prc = TRUE)

# Slice the data to by location
selfsample_urban <- df_noNA %>%
  filter(hc_rurality == "Urban")

selfsample_periurban <- df_noNA %>%
  filter(hc_rurality == "Periurban")

selfsample_rural <- df_noNA %>%
  filter(hc_rurality == "Rural")

wilcox.test(selfsample_rural$`TAM_TIPOMUESTRA_A`,selfsample_periurban$`TAM_TIPOMUESTRA_A`)
wilcox.test(selfsample_periurban$`TAM_TIPOMUESTRA_A`,selfsample_urban$`TAM_TIPOMUESTRA_A`)
wilcox.test(selfsample_rural$`TAM_TIPOMUESTRA_A`,selfsample_urban$`TAM_TIPOMUESTRA_A`)


#### ANALYSIS FOR IMPLEMENTATION DATA - TVT-TA REFERRAL REASONS #### ----
library(readxl)
df_tvtta <- read_excel("/Users/rachelmorse/Documents/2021:2022/Proyecto Precáncer/REAIM/Data (TVT-TA, self sampling).xlsx", 
                       sheet = "TVT-TA")
attach(df_tvtta)

reason <- (`Reason`)
eligibile <- (`TA_RECIBIO_C`) #1=eligible, 2=not eligible - this is whether they received it. it was more complicated to note whether they were eligible because of missing data



## description of eligible vs not for TA ##
table(eligibile)
eligibiletable <- table(eligibile)
prop.table(eligibiletable)

#one woman was missing all data and one had abdominal pain and they did not go ahead with TA so unclear if was eligible

#filter data to only include women who were not eligible for TA 
df_inelig <- df_tvtta %>%
  filter(eligibile == 2)
attach(df_inelig)

reason <- (`Reason`)

#rename data
df_inelig <- df_inelig %>% 
  mutate(reason_eng = case_when((`reason` == "1. IVAA(+) LESION INGRESA AL CANAL CERVICAL") ~ "Lesions which extend into the endocervical canal",
                                (`reason` == "2. UNION ESCAMO COLUMNAR NO VISIBLE") ~ "Lack of visible transformation zone",
                                (`reason` == "3. POLIPO CERVICAL NO/SI SANGRANTE") ~ "Cervical polyp",
                                (`reason` == "4. SOSPECHA DE CANCER") ~ "Suspected cancer case",
                                TRUE ~ "Other"))

attach(df_inelig)

table(reason_eng)
reasontable <- table(reason_eng)
prop.table(reasontable)

