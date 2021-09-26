rm(list = ls(all.names = TRUE))
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)
library(tidyr)


quiz_df <- data.frame()
#colnames(quiz_df) <- c("Category", "Question", "A1", "A2", "A3", "Answer", "Plot", "Link")
categories <- c("Politics", "Business", "Education", "Geography", "Health", "Mobility", "Science")


# Category: Politics
quiz_df_politics <- data.frame()
category <- categories[1]
#colnames(quiz_df_politics) <- c("Category", "Question", "A1", "A2", "A3", "Answer", "Plot", "Link")

## Abstimmung 13. Juni 2021 Details

Link <- "https://data.bs.ch/explore/dataset/100144/table/"
Basel_voting <- read_excel("HackZurich2021/data/Basel_voting.xlsx")

### Group 1
group <- 1
variable_1 <- c("Yes", "No")
variable_2 <- c("", paste0(" in ", unique(Basel_voting$Gemeinde)))
variable_3 <- unique(Basel_voting$Titel)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
  }

# plot 1
Basel_voting  %>% group_by(Titel, Gemeinde) %>% 
  summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
            `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
            `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
  rbind(Basel_voting %>% group_by(Titel) %>% 
          summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
                    `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
                    `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
          mutate(Gemeinde = "all")) %>% #tail(10)
  mutate(Yes_share = `Ja-Stimmen`/`Gültige Stimmzettel`,
         No_share = `Nein-Stimmen`/`Gültige Stimmzettel`,
         Size = ifelse(Gemeinde=="all", 1, 0),
         Titel = factor(Titel, levels = variable_3)) %>%
  ggplot() + geom_line(aes(x = Titel, y = Yes_share, color = Gemeinde, size = Size, group = Gemeinde)) + 
  theme_bw() + guides(size = FALSE) + 
  labs(x = "Initiative", y = "share of yes", 
       title = "Results of referendum in Kanton Basel Stadt 13.June 2021") + 
  theme(legend.position = "top", text = element_text(size=16)) + 
  scale_x_discrete(labels = addline_format(variable_3)) + 
  geom_hline(yintercept = 0.5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) 

  
ggsave("C:/Users/marin/Documents/HackZurich2021/graphs/Politics_1.png", width = 12, height = 12)

  
tmp <- expand.grid(variable_1, variable_2, variable_3) %>% 
  setNames(c("variable_1", "variable_2", "variable_3")) %>%
  mutate(Question = paste0("What is the share of '", variable_1, "' votes in Kanton-Basel Stadt 13. June 2021",
                           variable_2," for ", variable_3, "?"),
         Category = category,
         Group = group,
         Plot = "Politics_1",
         Answer = NA)


for (i in 1:nrow(tmp)){
  if (tmp$variable_2[i]=="") {
    jnk <- Basel_voting %>% 
      filter(Titel == tmp$variable_3[i]) %>% 
      group_by(Titel) %>% 
      summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
                `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
                `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
      mutate(Yes_share = `Ja-Stimmen`/`Gültige Stimmzettel`, No_share = `Nein-Stimmen`/`Gültige Stimmzettel`)
    if (tmp$variable_1[i]=="Yes") {
      tmp$Answer[i] = jnk$Yes_share[1]*100
    } else {tmp$Answer[i] = jnk$No_share[1]*100}
  } else {
    jnk <- Basel_voting  %>% filter(Gemeinde==sub(" in ", "", tmp$variable_2[i]),
                                    Titel == tmp$variable_3[i]) %>%
      group_by(Titel) %>% 
      summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
                `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
                `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
      mutate(Yes_share = `Ja-Stimmen`/`Gültige Stimmzettel`, No_share = `Nein-Stimmen`/`Gültige Stimmzettel`)
    if (tmp$variable_1[i]=="Yes") {
      tmp$Answer[i] = jnk$Yes_share[1]*100
    } else {tmp$Answer[i] = jnk$No_share[1]*100}
  }
  #print(i)
}

answers <- unique(as.numeric(tmp$Answer))

tmp <- tmp %>% rowwise() %>% mutate(Answer_ID = sample(c(1:3),1),
                             A1 = NA, A2 = NA, A3 = NA,
                      A1 = as.numeric(ifelse(Answer_ID==1, Answer, sample(answers,1))),
                      A2 = as.numeric(ifelse(Answer_ID==2, Answer, sample(answers,1))),
                      A3 = as.numeric(ifelse(Answer_ID==3, Answer, sample(answers,1))),
                      Check = is.na(A1 + A2 + A3) | A1==A2 | A2==A3 | A1==A3) %>%
  filter(Check==FALSE) %>% mutate(Plot = "Politics_1.png", 
                                  Link = Link,
                                  A1 = paste0(round(A1, 1), "%"),
                                  A2 = paste0(round(A2, 1), "%"),
                                  A3 = paste0(round(A3, 1), "%")) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link)

quiz_df_politics <- rbind(quiz_df_politics, tmp)

### Group 2
group = 2
variable_4 <- c("by post", "in person")


# plot 2
Basel_voting  %>% mutate(Post = ifelse(grepl("brieflich", Wahllokal, fixed = TRUE), "by post", "in person")) %>%
  group_by(Post, Titel) %>% 
  summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
            `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
            `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
  mutate(Yes_share = `Ja-Stimmen`/`Gültige Stimmzettel`,
         No_share = `Nein-Stimmen`/`Gültige Stimmzettel`,
         Titel = factor(Titel, levels = variable_3)) %>% #select(Post, Yes_share, No_share) %>%
  #gather(Variable, Value, -Post) %>%
  ggplot() + geom_line(aes(y = Yes_share, x = Titel, color = Post, group = Post)) + 
  theme_bw() + #guides(size = FALSE) + 
  labs(x = element_blank(), y = "share, %", 
       title = "Results of referendum in Kanton Basel Stadt 13.June 2021") + 
  theme(legend.position = "top", text = element_text(size=16), legend.title = element_blank()) + 
  scale_x_discrete(labels = addline_format(variable_3)) + 
  geom_hline(yintercept = 0.5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) 


ggsave("C:/Users/marin/Documents/HackZurich2021/graphs/Politics_2.png", width = 12, height = 12)

tmp <- expand.grid(variable_1, variable_3, variable_4) %>% 
  setNames(c("variable_1","variable_3", "variable_4")) %>%
  mutate(Question = paste0("What is the share of '", variable_1, "' votes in Kanton-Basel Stadt 13. June 2021 made ",
                           variable_4," for ", variable_3, "?"),
         Category = category,
         Group = group,
         Answer = NA)


for (i in 1:nrow(tmp)){
  jnk <- Basel_voting  %>% mutate(Post = ifelse(grepl("brieflich", Wahllokal, fixed = TRUE), "by post", "in person")) %>%
    group_by(Post, Titel) %>% 
    summarise(`Ja-Stimmen`= sum(`Ja-Stimmen`, na.rm = TRUE), 
              `Nein-Stimmen` = sum(`Nein-Stimmen`, na.rm = TRUE),
              `Gültige Stimmzettel`= sum(`Gültige Stimmzettel`, na.rm = TRUE)) %>%
    mutate(Yes_share = `Ja-Stimmen`/`Gültige Stimmzettel`,
           No_share = `Nein-Stimmen`/`Gültige Stimmzettel`,
           Titel = factor(Titel, levels = variable_3))
    if (tmp$variable_1[i]=="Yes") {
      tmp$Answer[i] = jnk$Yes_share[jnk$Titel == tmp$variable_3[i] & jnk$Post == tmp$variable_4[i]]*100
    } else {tmp$Answer[i] = jnk$No_share[jnk$Titel == tmp$variable_3[i] & jnk$Post == tmp$variable_4[i]]*100}
print(i)
  }

answers <- unique(as.numeric(tmp$Answer))

tmp <- tmp %>% rowwise() %>% mutate(Answer_ID = sample(c(1:3),1),
                                    A1 = NA, A2 = NA, A3 = NA,
                                    A1 = as.numeric(ifelse(Answer_ID==1, Answer, sample(answers,1))),
                                    A2 = as.numeric(ifelse(Answer_ID==2, Answer, sample(answers,1))),
                                    A3 = as.numeric(ifelse(Answer_ID==3, Answer, sample(answers,1))),
                                    Check = is.na(A1 + A2 + A3) | A1==A2 | A2==A3 | A1==A3) %>%
  filter(Check==FALSE) %>% mutate(Plot = "Politics_2.png", 
                                  Link = Link,
                                  A1 = paste0(round(A1, 1), "%"),
                                  A2 = paste0(round(A2, 1), "%"),
                                  A3 = paste0(round(A3, 1), "%")) %>%
  select(Group, Question, A1, A2, A3, Answer_ID, Plot, Link)

quiz_df_politics <- rbind(quiz_df_politics, tmp)
# randomise questions

write.csv(quiz_df_politics[sample(nrow(quiz_df_politics)), ], 
          "C:/Users/marin/Documents/HackZurich2021/quiz_db/quiz_df_politics.csv",
          row.names = FALSE)



# Category: Business
# Category: Education
# Category: Geography
# Category: Health
# Category: Mobility
# Category: Science
