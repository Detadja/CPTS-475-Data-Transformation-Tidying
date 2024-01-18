#Question 1
library(dplyr)
WNBA = read.csv("WNBA_Stats_21.csv", sep = ",", header = TRUE)
head(select(WNBA, contains("FG")))


#a.)
count(filter(WNBA, FTM > 50, AST > 75))

#b.)
WNBA %>%
  select(PLAYER, TEAM, FGM, TO, PTS) %>%
  arrange(desc(PTS)) %>%
  head(10)

#c.)
WNBA = WNBA %>% mutate(FGP = (FGM / FGA) * 100)
WNBA$FGP = round(WNBA$FGP, digits = 2)
WNBA = WNBA %>% mutate(FTP = (FTM / FTA) * 100)
WNBA$FTP = round(WNBA$FTP, digits = 2)
head(WNBA)

WNBA %>%
  filter(PLAYER == "Tina Charles") %>%
  select(FGP, FTP)

#d.)
WNBA %>%
  group_by(TEAM) %>%
  summarise(avg_REB = mean(REB, na.rm = TRUE), min_REB = min(REB, na.rm = TRUE), 
            max_REB = max(REB, na.rm = TRUE)) %>%
  arrange(desc(avg_REB))

#e.)
WNBA = WNBA %>%
  group_by(TEAM) %>%
  mutate(FTP_fix = ifelse(is.na(FTP), FGP * mean(FTP, na.rm = TRUE), FTP))
WNBA$FTP_fix = round(WNBA$FTP_fix, digits = 2)

WNBA2 = WNBA %>%
  group_by(TEAM) %>% 
  mutate(FTP_fix = ifelse(is.na(FTP), mean(FTP, na.rm = TRUE), FTP))
WNBA2$FTP_fix = round(WNBA2$FTP_fix, digits = 2)



#Question 2
library(tidyverse)
who = read.csv("who.csv", sep = ",", header = TRUE)
who1 = who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )

who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)



who5 = who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "type", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#b.)
sum(is.na(who))

#d.)
str(who5)

#e.)
who5$year = as.factor(who5$year)

who6 = aggregate(who5$cases, by = list(group = who5$year), FUN = sum)

ggplot(who6, aes(x = group, y = x)) +
  geom_point() +
  labs(x = "Year", y = "Number of Cases", title = "Total Global Tuberculosis Cases by Year") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(who6, aes(x = group, y = x)) +
  geom_bar(stat="identity") +
  labs(x = "Year", y = "Number of Cases", title = "Total Global Tuberculosis Cases by Year") +
  theme(plot.title = element_text(hjust = 0.5))

#f.)
schqtr = read.csv("SchQtr.csv")

schqtr1 = schqtr %>%
  pivot_longer(
    cols = Qtr.1:Qtr.4,
    names_to = "Quarter", 
    values_to = "Student_Count", 
    values_drop_na = TRUE
  ) %>%
  mutate(Quarter = stringr::str_replace(Quarter, "Qtr_2", "Qtr.2")) %>%
  separate(Quarter, c("Interval_Type", "Interval_ID"))

str(schqtr1)
