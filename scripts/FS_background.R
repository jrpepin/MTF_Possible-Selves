library(jsonlite)
library(ggrepel) 

# Fetch the data
df.union <- read.csv("https://ourworldindata.org/grapher/share-of-women-aged-1549-who-are-married-or-in-a-union.csv?v=1&csvType=filtered&useColumnShortNames=true&tab=chart&time=earliest..2021&country=USA~CAN")

df.mar <- read.csv("https://ourworldindata.org/grapher/marriage-rate-per-1000-inhabitants.csv?v=1&csvType=filtered&useColumnShortNames=true&time=1970..latest&country=USA~CAN")

df.birth <- read.csv("https://ourworldindata.org/grapher/children-born-per-woman.csv?v=1&csvType=filtered&useColumnShortNames=true&time=1970..latest&country=CAN~USA")

df.mar$label <- NA
df.mar$label[which(df.mar$Year == max(df.mar$Year))] <- df.mar$Entity[which(df.mar$Year == max(df.mar$Year))] 

data_ends.mar <- df.mar %>%
  group_by(Entity) %>%
  top_n(1, Year) 

p1 <- df.mar %>%
  ggplot(aes(x = Year, y = marriage_rate, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_text_repel(aes(label = Code), data = data_ends.mar, size = 5, nudge_x = 1) +
  theme_minimal() +
  theme(
    legend.position     = "none",
    legend.title        = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    plot.subtitle       = element_text(color = "grey50", size = 14),
    plot.caption        = element_text(color = "grey70", size = 12)) +
  scale_y_continuous(
    breaks = seq(0, 12, by=2), 
    limits = c(0, 12)) +
  labs( x        = " ", 
        y        = " ",
        title    = "Marriage rates",
        subtitle = "Number of marriages during a given year per 1,000 people",
        caption  = "ourworldindata.org/grapher/marriage-rate-per-1000-inhabitants")
p1

data_ends.birth <- df.birth %>%
  group_by(Entity) %>%
  top_n(1, Year) 

p2 <- df.birth %>%
  ggplot(aes(x = Year, y = fertility_rate_hist, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_text_repel(aes(label = Code), data = data_ends.birth, size = 5, nudge_x = 1) +
  theme_minimal() +
  theme(
    legend.position     = "none",
    legend.title        = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    plot.subtitle       = element_text(color = "grey50", size = 14),
    plot.caption        = element_text(color = "grey70", size = 12)) +
  scale_y_continuous(
    breaks = seq(0, 2.5, by=.5), 
    limits = c(0, 2.5)) +
  labs( x        = " ", 
        y        = " ",
        title    = "Total fertility rate",
        subtitle = "Number of births per woman",
        caption  = "ourworldindata.org/grapher/children-born-per-woman")
p2

rates <- p1 + p2
