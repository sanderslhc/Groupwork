lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

install.packages('')
tinytex::install_tinytex()

extract <- income_distribution[income_distribution$race == "All Races",]
dim(extract)
data1 <- extract[,c(1,4,6)]
data1 <- data1[!duplicated(data1$year),]
library(ggplot2)
library(tidyverse)
library(reshape2)
data1$year <- as.factor(data1$year)
data2 <- melt(data1, id.vars = "year")
ggplot(data2, aes(x=factor(year), y=value, colour=variable, group=variable)) + 
  geom_line(size=2) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 6),
        axis.title = element_text(size = 18),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size=20),
        text = element_text("serif"))

data3 <- extract[,c(1,8,9)]
colour=c("#DC143C","#0000FF","#20B2AA","#FFA500","#9370DB","#98FB98","#F08080","#1E90FF","#7CFC00","#FFFF00",  
         "#808000","#FF00FF","#FA8072","#7B68EE","#9400D3","#800080","#A0522D","#D2B48C","#D2691E","#87CEEB","#40E0D0","#5F9EA0",
         "#FF1493","#0000CD","#008B8B","#FFE4B5","#8A2BE2","#228B22","#E9967A","#4682B4","#32CD32","#F0E68C","#FFFFE0","#EE82EE",
         "#FF6347","#6A5ACD","#9932CC","#8B008B","#8B4513","#DEB887")
data3$income_bracket <- factor(data3$income_bracket,levels=unique(data3$income_bracket))
ggplot(data3,aes(x=year, y=income_distribution,fill=factor(income_bracket))) + 
  geom_col(position = "fill", width = 0.6) +
  labs(x = "Year",y = "Bracket") +
  scale_fill_manual(values = colour) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size=20),
        text = element_text("serif"))

data4 <- income_distribution[,c(1,2,8,9)]
data5 <- data4[which(data4$year >= 2000),]
data5$income_bracket <- factor(data5$income_bracket,levels=unique(data5$income_bracket))
ggplot(data5,aes(x=year, y=income_distribution,fill=factor(income_bracket))) + 
    geom_col(position = "fill", width = 0.6) +
    labs(x = "Year",y = "Bracket") +
    scale_fill_manual(values = colour) +
    theme_classic() +
    theme_bw(base_size = 6) +
    facet_grid(. ~ race) + 
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size=20),
        text = element_text("serif"))


ggplot(lifetime_earn,aes(x=race, y=lifetime_earn,fill=factor(gender))) + 
  geom_col( width = 0.6, stat="identity",position=position_dodge(0.75)) +
  labs(x = "race",y = "lifetime_earn") +
  scale_fill_manual(values = colour) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size=20),
        text = element_text("serif"))

