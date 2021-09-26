rm(list = ls(all.names = TRUE))
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(scales)
library(tidyr)
library(data.table)

categories <- c("Politics", "Business", "Education", "Geography", "Health", "Mobility", "Science")
quiz_df_education <- data.frame()
category <- categories[3]

# Category: Education
# 
Link <- "https://opendata.swiss/en/dataset/ubersicht-uber-alle-lernenden-im-kanton-zurich"
ZH_Lernende <- read_excel("HackZurich2021/data/ZH_Uebersicht_alle_Lernende.xlsx")

variable_1 <- c("female", "male") #sex
variable_2 <- c("private", "public") #school type
variable_3 <- c("local", "foreign") #nationality

### Group 1
group <- 1
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# plot 1
jnk <- ZH_Lernende %>% filter(Jahr==2020) %>%
  group_by(Geschlecht, Finanzierung) %>%
  summarise(Value = sum(Anzahl, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  mutate(Geschlecht= ifelse(Geschlecht=="M", "male", "female"),
         Finanzierung = ifelse(Finanzierung=="oef", "public", "private"))

jnk %>% ggplot() + 
  geom_bar(aes(x = Finanzierung, y = Value, fill = Geschlecht), stat = "identity") + 
  theme_bw() + 
  labs(x = "school type", y = "number of students", 
       title = "Number of students in canton Zurich in 2020") + 
  theme(text = element_text(size=16), legend.title = element_blank(), legend.position = "top") + 
  scale_y_continuous(labels = comma)

ggsave(paste0("C:/Users/marin/Documents/HackZurich2021/graphs/", category, "_", group, ".png"), 
       width = 12, height = 12)


tmp <- expand.grid(variable_1, variable_2) %>% 
  setNames(c("Geschlecht", "Finanzierung")) %>%
  mutate(Question = paste0("What was the share of ", Geschlecht, " students in ", Finanzierung," schools in canton Zurich in 2020?"),
         Category = category,
         Group = group,
         Plot = paste0(category, "_", group, ".png"),
         Answer = NA)

tmp <-  merge(tmp, jnk) %>% group_by(Finanzierung) %>% mutate(Value_fin = sum(Value)) %>% ungroup() %>%
  mutate(Answer = paste0(round(Value/Value_fin*100,1),"%"), A1 = NA, A2 = NA, A3 = NA) %>% 
  rowwise() %>% mutate(Answer_ID = sample(c(1:3),1))

answers <- tmp$Answer

for (i in 1:nrow(tmp)) {
  tmp$A1[i] <- if(tmp$Answer_ID[i]==1) tmp$Answer[i] else sample(answers[!answers %in% tmp$Answer[i]],1)
  tmp$A2[i] <- if(tmp$Answer_ID[i]==2) tmp$Answer[i] else sample(answers[!answers %in% c(tmp$Answer[i],tmp$A1[i])],1)
  tmp$A3[i] <- if(tmp$Answer_ID[i]==3) tmp$Answer[i] else sample(answers[!answers %in% c(tmp$Answer[i],tmp$A1[i], tmp$A2[i])],1)
  
}

tmp <- tmp %>% ungroup() %>% # unnest(c(Answer_ID)) %>%
  mutate(Plot = paste0(category, "_", group, ".png"), 
         Link = Link) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link) 

quiz_df_education <- rbind(quiz_df_education, tmp)

### Group 2
group <- 2


# plot 2
jnk <- ZH_Lernende %>% #filter(Jahr==2020) %>%
  group_by(Nationalitaet, Jahr) %>%
  summarise(Value = sum(Anzahl, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  mutate(Nationalitaet= ifelse(Nationalitaet=="CH", "local", "foreign"))

jnk %>% ggplot() + 
  geom_area(aes(x = Jahr, y = Value, fill = Nationalitaet), stat = "identity", alpha = 0.5) + 
  theme_bw() + 
  labs(x = element_blank(), y = "number of students", 
       title = "Number of students in canton Zurich") + 
  theme(text = element_text(size=16), legend.position = "top") + labs(fill = "Nationality:") +
  scale_y_continuous(labels = comma)

ggsave(paste0("C:/Users/marin/Documents/HackZurich2021/graphs/", category, "_", group, ".png"), 
       width = 12, height = 12)

tmp <- expand.grid(variable_3, c(2000,2010,2020)) %>% 
  setNames(c("Nationalitaet", "Jahr")) %>%
  mutate(Question = paste0("What was the share of ", Nationalitaet, " students in year ", Jahr," in canton Zurich?"),
         Category = category,
         Group = group,
         Plot = paste0(category, "_", group, ".png"),
         Answer = NA)

tmp <-  merge(tmp, jnk, all.x = TRUE) %>% group_by(Jahr) %>% mutate(Value_fin = sum(Value)) %>% ungroup() %>%
  mutate(Answer = paste0(round(Value/Value_fin*100,1),"%"), A1 = NA, A2 = NA, A3 = NA) %>% 
  rowwise() %>% mutate(Answer_ID = sample(c(1:3),1))

answers <- tmp$Answer

for (i in 1:nrow(tmp)) {
  tmp$A1[i] <- if(tmp$Answer_ID[i]==1) tmp$Answer[i] else sample(answers[!answers %in% tmp$Answer[i]],1)
  tmp$A2[i] <- if(tmp$Answer_ID[i]==2) tmp$Answer[i] else sample(answers[!answers %in% c(tmp$Answer[i],tmp$A1[i])],1)
  tmp$A3[i] <- if(tmp$Answer_ID[i]==3) tmp$Answer[i] else sample(answers[!answers %in% c(tmp$Answer[i],tmp$A1[i], tmp$A2[i])],1)
  
}

tmp <- tmp %>% ungroup() %>% unnest(c(Answer_ID)) %>%
  mutate(Link = Link) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link) 

quiz_df_education <- rbind(quiz_df_education, tmp)

write.csv(quiz_df_education[sample(nrow(quiz_df_education)), ], 
          "C:/Users/marin/Documents/HackZurich2021/quiz_db/quiz_df_education.csv",
          row.names = FALSE)
