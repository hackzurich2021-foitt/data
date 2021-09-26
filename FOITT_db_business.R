rm(list = ls(all.names = TRUE))
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(scales)
library(tidyr)
library(data.table)

categories <- c("Politics", "Business", "Education", "Geography", "Health", "Mobility", "Science")
quiz_df_business <- data.frame()
category <- categories[2]


# Category: Business
# New business 
Link <- "https://opendata.swiss/en/dataset/neu-gegrundete-unternehmen-nach-kantonen4/"
new_business_CH <- read_csv("HackZurich2021/data/new_business_CH.csv")
GEO <- read_excel("HackZurich2021/data/new_business_CH_desc.xlsx", sheet = "GEO")
NOGA <- read_excel("HackZurich2021/data/new_business_CH_desc.xlsx", sheet = "NOGA")
CLASS <- read_excel("HackZurich2021/data/new_business_CH_desc.xlsx", sheet = "CLASS")

new_business_CH <- new_business_CH %>% 
  merge(GEO %>% rename(GEO=CODE, Location = LABEL_EN) %>% select(GEO, Location)) %>%
  merge(CLASS %>% rename(CLASS=CODE, Size = LABEL_EN) %>% select(CLASS, Size)) %>%
  merge(NOGA %>% rename(NOGA=CODE, Industry = LABEL_EN) %>% select(NOGA, Industry))

variable_1 <- GEO$LABEL_EN[grep("CH", GEO$CODE)][2:8] #regions
variable_2 <- GEO$LABEL_EN[2:27] #cantons
variable_3 <- NOGA$LABEL_EN[2:16] #sectors

### Group 1
group <- 1

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
# plot 1
jnk <- new_business_CH %>% filter(Location %in% variable_2,
                                  INDICATOR=="Jobs created by New businesses") %>%
  group_by(Location) %>%
  summarise(Value = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(-Value)
  
jnk %>% mutate(Location = factor(Location, levels = jnk$Location)) %>%
  ggplot() + 
  geom_bar(aes(x = Location, y = Value), fill = "lightblue",stat = "identity", position = position_dodge()) +
  theme_bw() + 
  labs(x = "Canton", y = element_blank(), 
       title = "Number of jobs created by new business 2013-2018") + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous(labels = comma)
  #scale_x_discrete(labels = addline_format(variable_3)) + 
  #geom_hline(yintercept = 0.5, color = "black") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) 

ggsave("C:/Users/marin/Documents/HackZurich2021/graphs/Business_1.png", width = 12, height = 12)

tmp <- data.frame(Question = "Which canton had the highest number of jobs created by new business in the period 2013-2018?",
         Category = category,
         Group = group,
         Answer = jnk$Location[1]) %>% 
  rowwise() %>% 
  mutate(Answer_ID = sample(c(1:3),1),
          A1 = NA, A2 = NA, A3 = NA,
                     A1 =ifelse(Answer_ID==1, Answer, jnk$Location[6]),
                     A2 = ifelse(Answer_ID==2, Answer, jnk$Location[21]),
                     A3 = ifelse(Answer_ID==3, Answer, jnk$Location[3])) %>% 
                       mutate(Plot = "Business_1.png", 
                                  Link = Link) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link) %>%
    rbind(data.frame(Question = "Which three cantons had the highest number of jobs created by new business in the period 2013-2018?",
                     Category = rep(category,10),
                     Group = group,
                     Answer = paste(as.list(jnk$Location[1:3]), collapse = ", ")) %>% 
            rowwise() %>% 
            mutate(Answer_ID = sample(c(1:3),1),
                   A1 = NA, A2 = NA, A3 = NA,
                   A1 =ifelse(Answer_ID==1, Answer, paste(sample(jnk$Location,3), collapse = ", ")),
                   A2 = ifelse(Answer_ID==2, Answer, paste(sample(jnk$Location,3), collapse = ", ")),
                   A3 = ifelse(Answer_ID==3, Answer, paste(sample(jnk$Location,3), collapse = ", "))) %>% 
            mutate(Plot = "Business_1.png", 
                   Link = Link) %>% select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link) 
      
    )

tmp <- tmp %>% ungroup()

quiz_df_business <- rbind(quiz_df_business, tmp)

### group 2
group <- 2
jnk <- new_business_CH %>% filter(Location %in% variable_1,
                                  INDICATOR=="New businesses") %>%
  group_by(Location, TIME_PERIOD) %>%
  summarise(Value = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(-Value)

jnk %>% mutate(Location = factor(Location, levels = unique(jnk$Location))) %>%
  ggplot() + 
  geom_line(aes(x = TIME_PERIOD, y = Value, color = Location, group = Location)) +
  theme_bw() + 
  labs(x = element_blank(), y = "number of companies", 
       title = "Number of new businesses created between 2013-2018") + 
  theme(text = element_text(size=16)) + 
  scale_y_continuous(labels = comma) + labs(color = "Area")
#scale_x_discrete(labels = addline_format(variable_3)) + 
#geom_hline(yintercept = 0.5, color = "black") +
#scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) 

ggsave(paste0("C:/Users/marin/Documents/HackZurich2021/graphs/", category, "_", group, ".png"), 
       width = 12, height = 12)


tmp <- data.frame(Year = 2013:2018) %>% mutate(
  Question = paste0("Which area had the highest number of new businesses created in year ", Year,"?"),
                  Category = category,
                  Group = group,
                  Answer = NA,  A1 = NA, A2 = NA, A3 = NA) %>% 
  rowwise() %>% mutate(Answer_ID = sample(c(1:3),1))

for (i in 1:nrow(tmp)) {
  tmp$Answer[i] <- jnk %>% group_by(TIME_PERIOD) %>% mutate(Value_max = max(Value)) %>% 
    filter(Value==Value_max, TIME_PERIOD==tmp$Year[i]) %>% ungroup() %>% select(Location)
  tmp$A1[i] <- if(tmp$Answer_ID[i]==1) tmp$Answer[i] else sample(variable_1[!variable_1 %in% tmp$Answer[i]],1)
  tmp$A2[i] <- if(tmp$Answer_ID[i]==2) tmp$Answer[i] else sample(variable_1[!variable_1 %in% c(tmp$Answer[i],tmp$A1[i])],1)
  tmp$A3[i] <- if(tmp$Answer_ID[i]==3) tmp$Answer[i] else sample(variable_1[!variable_1 %in% c(tmp$Answer[i],tmp$A1[i], tmp$A2[i])],1)

}


tmp <- tmp %>% unnest(c(Answer, A1, A2, A3, Answer_ID)) %>%
  mutate(Plot = paste0(category, "_", group, ".png"), 
         Link = Link) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link) 

quiz_df_business <- rbind(quiz_df_business, tmp)


write.csv(quiz_df_business[sample(nrow(quiz_df_business)), ], 
          "C:/Users/marin/Documents/HackZurich2021/quiz_db/quiz_df_business.csv",
          row.names = FALSE)
