library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

star_wars_data <- read_csv("week7_starwars.csv", col_names = FALSE, skip = 2)

# ----- Clean up column names manually: probably not optimal... 
star_wars_data <- rename(star_wars_data, "RespondentID" = X1, "SeenAny" = X2, "Fan" = X3, 
                         "Seen1" = X4, "Seen2" = X5, 
                         "Seen3" = X6, "Seen4" = X7, "Seen5" = X8, "Seen6" = X9, "Rank1" = X10, "Rank2" = X11, 
                         "Rank3" = X12, "Rank4" = X13, "Rank5" = X14, "Rank6" = X15, "Han Solo" = X16, "Luke Skywalker" = X17,
                         "Princess Leia Organa" = X18, "Anakin Skywalker" = X19, "Obi Wan Kenobi" = X20, 
                         "Emperor Palpatine" = X21, "Darth Vader" = X22, "Lando Calrissian" = X23, "Boba Fett" = X24, 
                         "C-3P0" = X25, "R2 D2" = X26, "Jar Jar Binks" = X27, "Padme Amidala" = X28, "Yoda" = X29, 
                         "ShotFirst" = X30, "ExpandedUniverse" = X31, "FanExpandedUniverse" = X32, "FanStarTrek" = X33, 
                         "Gender" = X34, "Age" = X35, "Income" = X36, "Education" = X37, "Location" = X38)

# ----- Figure 1: Which star wars movies have you seen
# Replace NA and names with 0 and 1
star_wars_data <-
  star_wars_data %>% mutate(Seen1 = ifelse(is.na(Seen1), 0, 1), 
                            Seen2 = ifelse(is.na(Seen2), 0, 1), 
                          Seen3 = ifelse(is.na(Seen3), 0, 1), Seen4 = ifelse(is.na(Seen4), 0, 1),
                          Seen5 = ifelse(is.na(Seen5), 0, 1), Seen6 = ifelse(is.na(Seen6), 0, 1))

star_wars_plot_theme <- theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                              plot.background = element_rect(fill = "gray89"),
                              panel.background = element_rect(fill = "gray89"),
                              axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title = element_blank(),
                              text = element_text(size=16, colour = "gray25"),
                              plot.title = element_text(face = "bold", hjust = 1), 
                              strip.background = element_blank(), 
                              plot.subtitle = element_text(hjust = 1))

# Consider just the people who have seen any film, take just the columns we're interested in, gather into tidy format, plot
star_wars_data %>% 
  filter(SeenAny == "Yes") %>%
  select(Seen1, Seen2, Seen3, Seen4, Seen5, Seen6) %>%
  gather(Seen1, Seen2, Seen3, Seen4, Seen5, Seen6, key = Film, value = Seen) %>% 
  mutate(Film = factor(Film, labels = c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith", "A New Hope", 
                          "The Empire Strikes Back", "Return of the Jedi"), ordered = TRUE)) %>%
  ggplot(aes(reorder(Film, desc(Film)), Seen)) + stat_summary(fun.y = sum, geom = "bar", fill = "dodgerblue2", width = 0.7) +
  stat_summary(aes(label = paste0(round(..y../936, 2)*100,"%")), fun.y = sum, geom = "text", position = position_nudge(y = 60)) + 
  labs(title = "Which 'Star Wars' Movies Have You Seen?      ", subtitle = "Of 936 respondents who have seen any film                                    ") +
  coord_flip() +
  star_wars_plot_theme

ggsave("star_wars_figure_1.pdf", width = 15, height = 7, units = "cm")
ggsave("star_wars_figure_1.png", width = 15, height = 7, units = "cm")


# ---- Figure 2: What's the best star wars movie?
star_wars_data %>% 
  filter(Seen1 == 1, Seen2 == 1, Seen3 == 1, Seen4 == 1, Seen5 == 1, Seen6 == 1) %>%
  select(Rank1, Rank2, Rank3, Rank4, Rank5, Rank6) %>%
  mutate(Best = apply(., 1, function(x) names(x)[which.min(x)])) %>%
  mutate(Best = factor(Best, labels = c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith", "A New Hope", 
                                        "The Empire Strikes Back", "Return of the Jedi"), ordered = TRUE)) %>%
  ggplot(aes(x=reorder(Best, desc(Best)))) + geom_bar(fill = "dodgerblue2", width = 0.7) + 
  geom_text(stat = "count", aes(label = paste0(round(..count../471*100),"%"), y = ..count..), position = position_nudge(y = 10)) +
  labs(title = "What's the Best 'Star Wars' Movie?                   ", subtitle = "Of 471 respondents who have seen all six films                               ") + 
  coord_flip() + star_wars_plot_theme

ggsave("star_wars_figure_2.pdf", width = 15, height = 7, units = "cm")
ggsave("star_wars_figure_2.png", width = 15, height = 7, units = "cm")



# ---- Figure 3:



# ---- Figure 4: character favorability ratings

### TO DO 
# Reorder bars

character_names <- c("Han Solo", "Luke Skywalker", "Princess Leia Organa", "Anakin Skywalker", "Obi Wan Kenobi", "Emperor Palpatine",
           "Darth Vader", "Lando Calrissian", "Boba Fett", "C-3P0", "R2 D2", "Jar Jar Binks", "Padme Amidala", "Yoda")

star_wars_data %>%
  filter(SeenAny == "Yes") %>%
  filter(!is.na(`Han Solo`) | !is.na(`Luke Skywalker`) | !is.na(`Princess Leia Organa`) | !is.na(`Anakin Skywalker`) | !is.na(`Obi Wan Kenobi`) | !is.na(`Emperor Palpatine`) |
         !is.na(`Darth Vader`) | !is.na(`Lando Calrissian`) | !is.na(`Boba Fett`) | !is.na(`C-3P0`) | !is.na(`R2 D2`) | !is.na(`Jar Jar Binks`) | !is.na(`Padme Amidala`) |
         !is.na(`Yoda`)) %>%
  select(character_names) %>%
  gather(character_names, key = "character", value = "ranking") %>%
  group_by(character, ranking) %>%
  summarise(count=n()) %>%
  spread(key = ranking, value = count) %>%
  mutate(Favorable = `Somewhat favorably` + `Very favorably`, Neutral = `Neither favorably nor unfavorably (neutral)`,
         Unfavorable = `Somewhat unfavorably` + `Very unfavorably`, Unfamiliar = `<NA>` + `Unfamiliar (N/A)`) %>%
  select(character, Favorable, Neutral, Unfavorable, Unfamiliar) %>%
  gather(Favorable, Neutral, Unfavorable, Unfamiliar, key = "Ranking", value = "Count") %>%
  mutate(Ranking = factor(Ranking, levels = c("Favorable", "Neutral", "Unfavorable", "Unfamiliar"))) %>%
  ungroup() %>%
  mutate(character = factor(character)) %>%
  ggplot() + geom_bar(aes(x = character, y=Count, fill = Ranking), stat = "identity", show.legend = FALSE) + facet_wrap(~Ranking, nrow = 1) +
  geom_text(aes(x = character, y = 0, label = paste0(round(Count/834*100), "%")), position = position_nudge(y = 150)) + 
  labs(title = "'Star Wars' Character Favorability Ratings       ", 
       subtitle = "By 834 respondents                                                            ") + 
  coord_flip() +
  star_wars_plot_theme +
  scale_fill_manual(values = c("chartreuse4", "dodgerblue2", "red", "gray55"))


ggsave("star_wars_figure_4.pdf", width = 15, height = 12, units = "cm")








