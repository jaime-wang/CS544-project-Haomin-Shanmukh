

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
library(ggthemes)
################# 
head(data)
colnames(data)[5:38] <- 1980:2013
df_Continent <- aggregate(data$Total,data["Continent"],sum)
colnames(df_Continent)[2] <- "Total"

## Pie Chart
whole <- sum(df_Continent$Total);
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

##Bar chart

p <- ggplot(data = df_Continent,
            mapping = aes(
              x = Continent, 
              y = Total, 
              fill = Continent))
p + geom_col() +
  guides(fill=FALSE)+coord_flip()+labs(y="Immigrants")


df_top <- data[order(data$Total,decreasing = TRUE),]
df_year <- data %>% select(-Total) %>% gather(key = "Year", value = "No_of_Immigrants", -c(Country, Continent, Region, DevName));df_year


## Line Chart
data_2 <- data %>% select(-Total) %>% gather(key = "Year", value = "No_of_Immigrants", -c(Country, Continent, Region, DevName))
x <- data_2 %>% filter(Country %in% df_top[1:5,"Country"])

ggplot(x, aes(Year, No_of_Immigrants, color= Country, group= Country)) + 
  geom_smooth(se= FALSE) + 
  labs(title = "Immigration pattern of Top 5 Countries", y = "Immigrants") +
  theme_economist(base_size = 22 ) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.3)) + 
  theme(axis.text.y = element_text(hjust = 1.6)) +
  theme(axis.title.y = element_text(family = "serif", size = 14, face= "italic", hjust = 0.3 ,vjust= 0.5), legend.position = "right") +
  theme(axis.title.x = element_text(family = "serif", size = 14, face= "italic")) +
  theme(plot.title = element_text(family = "serif", size = 18, hjust = 0.5)) +
  theme(legend.title = element_text(family = "serif", size = 14, face = "bold")) +
  theme(legend.text = element_text(family = "serif", size = 12, face = "italic")) +
  scale_x_discrete(breaks= c(1980, 1990, 2000, 2010))

## boxplot
top_10 <- df_top[1:10,"Country"];top_10
data_3 <- data_2 %>% filter(Country %in% top_10)
ggplot(data_3, aes(Country, No_of_Immigrants, color= Country, group= Country)) + geom_boxplot()+coord_flip()+
ylab("Number of immigrants per year")+labs(title ="TOP 10 Countries immigrants per year")

## Developing vs Developed
df_year %>% group_by(DevName, Year) %>% summarise(Total_Immigrants = sum(No_of_Immigrants)) %>%
  ggplot(aes(Year, Total_Immigrants, fill= DevName, group= DevName)) +
  geom_area(size=0, alpha = 0.7, color= "white") +
  theme_economist(base_size = 22 ) +
  labs(title = "Developing vs Developed Region Immigration", y = "Total Immigrants") +
  theme(axis.text.x = element_text(angle = 90, hjust = 10, vjust = 0.3, size = 8)) + 
  theme(axis.text.y = element_text(hjust = 1.6)) +
  theme(axis.title.y = element_text(family = "A", size = 14, face= "bold", hjust = 0.3 ,vjust= 1.5), legend.position = "right") +
  theme(axis.title.x = element_text(family = "A", size = 14, face= "bold")) +
  theme(plot.title = element_text(family = "A", size = 18, hjust = 0.38)) +
  theme(legend.title = element_text(family = "A", size = 9, face = "bold")) +
  theme(legend.text = element_text(family = "A", size = 7, face = "bold"))+scale_fill_brewer(palette="Dark2")








