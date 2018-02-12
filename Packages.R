install.packages("ggplot2")


?ggplot
library(ggplot2)
?qplot



qplot(data=diamonds, carat, price, color=clarity, facets=.~clarity)
