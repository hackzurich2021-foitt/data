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
category <- categories[4]

# Category: Geography
# Share of residential area (excluding transport area) in the total area
Link <- "https://opendata.swiss/en/dataset/anteil-siedlungsflache"
KANTON_ZUERICH_population <- read_excel("HackZurich2021/data/KANTON_ZUERICH_population.xlsx")

variable_1 <-unique(KANTON_ZUERICH_population$GEBIET_NAME[697:744]) #bezirk
variable_2 <-unique(KANTON_ZUERICH_population$GEBIET_NAME[745:788]) #region
variable_3 <- c(1985, 1997, 2009, 2018, 1983, 1996, 2007) #year

### Group 1
group <- 1
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# plot 1
jnk <- KANTON_ZUERICH_population %>% filter(GEBIET_NAME %in% variable_1,
                                  INDIKATOR_JAHR==2018) %>%
  arrange(-INDIKATOR_VALUE) %>% mutate(Location = sub("Bezirk ", "", GEBIET_NAME))

jnk %>% mutate(Location = factor(Location, levels = jnk$Location)) %>%
  ggplot() + 
  geom_bar(aes(x = Location, y = INDIKATOR_VALUE/100), fill = "lightblue",stat = "identity", position = position_dodge()) +
  theme_bw() + 
  labs(x = "district", y = element_blank(), 
       title = "Share of residential area (excl. transport area) in year 2018") + 
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy =1))

ggsave(paste0("C:/Users/marin/Documents/HackZurich2021/graphs/", category, "_", group, ".png"), 
       width = 12, height = 12)

answers_1

tmp <- data.frame(Question = "Which district of canton Zurich had the highest share of residential area?",
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