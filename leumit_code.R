library(readr)
library(dplyr)
library(readxl)
library(stringi)

locale("he")
###### Leumit project ######

##### Leumit's players data set ####
### import files from excel ### using readxl package 
players_info <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/players/players_info.xlsx")
players_stat <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/players/players_stat.xlsx")

### statistic parameters processing ### using dplyr package
## remove irrelevant column
players_stat <- players_stat %>% select(-get_blocked_neg)

# create per minutes parameters base on relevant parameters
stat_per_minute <- players_stat %>%
  select(-per2, -per3, -per_ft, -games, -rank)

# convert parameters to per minute parameters by divide it by minutes
stat_per_minute <- stat_per_minute/stat_per_minute$minutes

#round the results
stat_per_minute <- round(stat_per_minute, 2)

#redefine id column
stat_per_minute$id <- players_stat$id

#remove irrelevant column 
stat_per_minute <- stat_per_minute %>% select(-minutes)

#change columns' names of per minutes parameters
colnames(stat_per_minute)[2:12] <- paste(colnames(stat_per_minute)[2:12], "per_minute", sep = "_")

#merge with all the statistic info
stat <- merge(x = players_stat, y = stat_per_minute, by = "id", all = TRUE)

## merge files by player's id
players <- merge(x = players_info, y = stat, by = "id", all.x = TRUE)

## remove irrelevant columns
players <- players %>% select(-`height(meter)`)

# create new parameters 
#points to TO ratio
players$points_TO <- round((players$points/players$TO), 2)

#assists to TO ratio
players$as_TO <- round((players$as/players$TO), 2)

#relocate new paramentes
players <- players %>% relocate(points_TO, .after = TO)
players <- players %>% relocate(TO, .after = points_TO)

write_csv(players, "C:/Users/danie/Desktop/data practice/R/Shomron/players/players.csv")

##### Leumit's teams dataset ####

### import files from excel ### using readxl package since Hebrew involved
## processed from https://ibasketball.co.il/ edited on excel first

afula <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/afula.xlsx")
ashdod <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/ashdod.xlsx")
ashkelon <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/ashkelon.xlsx")
ata <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/ata.xlsx")
haifa <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/haifa.xlsx")
hh <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/hh.xlsx")
maale <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/maale.xlsx")
migdal <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/migdal.xlsx")
modiin <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/modiin.xlsx")
nahariya <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/nahariya.xlsx")
nataniya <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/nataniya.xlsx")
raanana <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/raanana.xlsx")
ramat_hasharon <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/ramat_hasharon.xlsx")
rg <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/rg.xlsx")
rg_givataim <- read_excel("C:/Users/danie/Desktop/data practice/R/Shomron/teams/rg_givataim.xlsx")

## combine all data together ##
teams <- rbind(ashdod, ashkelon, ata, haifa, hh, maale, migdal, modiin, nahariya,
               nataniya, raanana, ramat_hasharon, rg, rg_givataim, afula)

# create new columns # using stringi package
teams$opponent_team <- stri_sub(teams$text, 1, -6)
teams$game_location <- stri_sub(teams$text, -4, -4)
teams$game_result <- stri_sub(teams$text, -1)

#remove irrelevant column # using dplyr package
teams <- teams %>% select(-text)

#change columns location # using dplyr package
teams <- teams %>% relocate(opponent_team, .after = team)
teams <- teams %>% relocate(game_location, .after = opponent_team)
teams <- teams %>% relocate(game_result, .after = game_location)

# editing "attempts" columns # using stringi package
teams$attempt_2 <- stri_sub(teams$attempt_2, -2, -1)
teams$attempt_3 <- stri_sub(teams$attempt_3, -2, -1)
teams$attempt_ft <- stri_sub(teams$attempt_ft, -2, -1)

# change values over data frame #using r base
teams[teams == "ח"] <- "away"
teams[teams == "ב"] <- "home"
teams[teams == "נ"] <- "win"
teams[teams == "ה"] <- "loss"

#change teams names from hebrew to english
teams[teams == "מכבי עירוני רמת גן"] <- "Maccabi Ironi Ramat-Gan"
teams[teams == "אליצור אשקלון"] <- "Elizur Ashkelon"
teams[teams == "הפועל רמת גן גבעתיים"] <- "Hapoel Ramat-Gan Givatayim"
teams[teams == "מכבי נקסט אורבן חיפה"] <- "Maccabi Haifa"
teams[teams == "עירוני רעננה"] <- "Ironi Raanana"
teams[teams == "הפועל מגה אור חבל מודיעין"] <- "Hapoel Hevel Modiin"
teams[teams == "עירוני נהריה"] <- "Ironi Nahariya"
teams[teams == "הפועל מגדל העמק יזרעאל"] <- "Hapoel Migdal Haemeq Izrael"
teams[teams == "מכבי מעלה אדומים" ]<- "Maccabi Maale Adumim"
teams[teams == "עירוני קריית אתא"] <- "Ironi Kiryat Ata"
teams[teams == "א.ס רמת השרון"] <- "AS Ramat Hasharon"
teams[teams == "מכבי סנו הוד השרון"] <- "Maccabi Hod Hasharon"
teams[teams == "הפועל עפולה"] <- "Hapoel Afula"
teams[teams == "אליצור BRIGA נתניה"] <- "Elitzur Nataniya"

### export file ### using readr library
write.csv(teams, "C:/Users/danie/Desktop/data practice/R/Shomron/teams//teams.csv")
