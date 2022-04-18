library(Hmisc)
library(corrplot)

df <- read.csv(
  "https://raw.githubusercontent.com/jaime-wang/CS544-project-Haomin-Shanmukh/main/Blueberry%20yield%20data.csv")
head(df)
df <- df[,-1]
head(df)
res <- rcorr(as.matrix(df))

corrplot(cor(df), type ="full")
