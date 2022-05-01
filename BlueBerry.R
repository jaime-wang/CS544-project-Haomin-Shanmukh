

data <- read.csv("https://raw.githubusercontent.com/jaime-wang/CS544-project-Haomin-Shanmukh/main/canadian_immegration_data.csv")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggrepel")
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(sampling)
library(rgl)
library(ggrepel)
library(tidyverse)
################# Categorical data
head(data)
table(data$Continent)
df_Continent <- aggregate(data$Total,data["Continent"],sum)
colnames(df_Continent)[2] <- "Total"

whole <- sum(df_Continent$Total);whole
df2 <- df_Continent %>% 
  mutate(csum = rev(cumsum(rev(Total))), 
         pos = Total/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Total/2, pos))

ggplot(df_Continent, aes(x = "" , y = Total, fill = fct_inorder(Continent))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Total/whole*100,2), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Continent")) +
  theme_void()

p <- ggplot(data = df_Continent,
            mapping = aes(
              x = Continent, 
              y = Total, 
              fill = Continent))
p + geom_col() +
  guides(fill=FALSE)+coord_flip()+labs(y="immigration")


df_top <- data[order(data$Total,decreasing = TRUE),]
df_top[1:5,"Country"]
colnames(data)

df_year <- data %>% select(-Total) %>% gather(key = "Year", value = "No. of Immigrants", -c(Country, Continent, Region, DevName));df_year
head(df_year)
df_top[1:5,]

library(ggthemes)
data_2 <- data %>% select(-Total) %>% gather(key = "Year", value = "No_of_Immigrants", -c(Country, Continent, Region, DevName))
x <- data_2 %>% filter(Country == c("India")|Country == c("China")|Country == c("United Kingdom of Great Britain and Northern Ireland"))

ggplot(x, aes(Year, No_of_Immigrants, color= Country, group= Country)) + 
  geom_smooth(se= FALSE) + 
  labs(title = "Immigration pattern of Top 3 Countries", y = "No. of Immigrants") +
  theme_economist(base_size = 22 ) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.3)) + 
  theme(axis.text.y = element_text(hjust = 1.6)) +
  theme(axis.title.y = element_text(family = "serif", size = 14, face= "italic", hjust = 0.3 ,vjust= 0.5), legend.position = "right") +
  theme(axis.title.x = element_text(family = "serif", size = 14, face= "italic")) +
  theme(plot.title = element_text(family = "serif", size = 18, hjust = 0.5)) +
  theme(legend.title = element_text(family = "serif", size = 14, face = "bold")) +
  theme(legend.text = element_text(family = "serif", size = 12, face = "italic")) +
  scale_x_discrete(breaks= c(1980, 1990, 2000, 2010))







