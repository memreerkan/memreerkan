data(mtcars)
head(mtcars)
data("iris")
head(iris)
library(ggplot2)
data("diamonds")
head(diamonds)
ggplot(diamonds,aes(carat,price,color=clarity))+
  geom_point(alpha=0.4)+
  geom_smooth()

