

data <- read.csv("https://raw.githubusercontent.com/jaime-wang/CS544-project-Haomin-Shanmukh/main/canadian_immegration_data.csv")

install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggpubr")

library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(sampling)
library(rgl)
library(ggrepel)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(sampling)
################# 
data[data$Country == "United Kingdom of Great Britain and Northern Ireland", "Country"] <- "UK"
data[data$Continent == "Latin America and the Caribbean", "Continent"] <- "S.America"
data[data$Continent == "Northern America", "Continent"] <- "N.America"

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



## CLT on total migration for countries.

samples <- 1000


s10<-10
xbar10 <- numeric(samples)
set.seed(123)
for (i in 1:samples) {
  xbar10[i] <- mean(data$Total[which(srswor(s10, nrow(data))==1)])
}

size10<- ggplot() + geom_histogram(aes(x= xbar10, y=..density..), fill="lightskyblue", colour="navy blue"  , bins = 30) +
  ggtitle("Sample Size = 10")

cat("Sample Size = ", s10, " Mean = ", mean(xbar10),
    " SD = ", sd(xbar10), "\n")

s25<-25
xbar25 <- numeric(samples)
set.seed(123)
for (i in 1:samples) {
  xbar25[i] <- mean(data$Total[which(srswor(s25, nrow(data))==1)])
}

size25<- ggplot() + geom_histogram(aes(x= xbar25, y=..density..), fill="lightskyblue", colour="navy blue"  , bins = 30) +
  ggtitle("Sample Size = 25")


cat("Sample Size = ", s25, " Mean = ", mean(xbar25),
    " SD = ", sd(xbar25), "\n")


s45<-45
xbar45 <- numeric(samples)
set.seed(123)
for (i in 1:samples) {
  xbar45[i] <- mean(data$Total[which(srswor(s45, nrow(data))==1)])
}

size45<- ggplot() + geom_histogram(aes(x= xbar45, y=..density..), fill="lightskyblue", colour="navy blue"  , bins = 30) +
  ggtitle("Sample Size = 45")

cat("Sample Size = ", s45, " Mean = ", mean(xbar45),
    " SD = ", sd(xbar45), "\n")


s70<-70
xbar70 <- numeric(samples)
set.seed(123)
for (i in 1:samples) {
  xbar70[i] <- mean(data$Total[which(srswor(s70, nrow(data))==1)])
}

size70<- ggplot() + geom_histogram(aes(x= xbar70, y=..density..), fill="lightskyblue", colour="navy blue"  , bins = 30) +
  ggtitle("Sample Size = 70") 

cat("Sample Size = ", s70, " Mean = ", mean(xbar70),
    " SD = ", sd(xbar70), "\n")

ggarrange(size10, size25, size45, size70 , 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#############################################################


#### Sampling Methods

###Population
barplot(table(data$Continent), las=2, col = "lightskyblue")


##Systematic Sampling
set.seed(195)
#

N <- nrow(data)
n <- 39
k <- ceiling(N / n)
r <- sample(k, 1)
s <- seq(r, by = k, length = n)

sample.1 <- data[s, ]
nrow(sample.1)
barplot(table(sample.1$Continent), las=2, col = "lightskyblue")


# Stratified, unequal sized strata
set.seed(195)
section.ids <- rep(LETTERS[1:4], c(10, 20, 30, 40))

section.scores <- round(runif(100, 60, 80))

df <- data.frame(data$Continent, data$DevName, data$Total)

head(df)

freq <- table(data$DevName)
freq

st.sizes <- round(39 * freq / sum(freq))
st.sizes

st.2 <- sampling::strata(df, stratanames = c("data.DevName"),
                         size = st.sizes, method = "srswor",
                         description = TRUE)

nrow(st.2)

st.sample2 <- getdata(df, st.2)
st.sample2
barplot(table(st.sample2$data.Continent),  las=2, col = "lightskyblue")


# srswor

s <- srswor(39, nrow(df))

sample.3 <- df[s != 0, ]
head(sample.3)
barplot(table(sample.3$data.Continent), las=2, col = "lightskyblue")
ggplot(sample.3, aes(x=data.Continent, y=data.Total))
