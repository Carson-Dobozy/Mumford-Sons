
library(tidyverse)
#ggplot practice
ggplot2::mpg
data = mpg
head(data[1:5])
library(ggplot2)
ggplot(data, aes(x = hwy)) +
  geom_bar(aes(color='red'))
#more ggplot practice 
install.packages("gapminder")
library(gapminder)
library(ggplot2)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp,))
p + geom_point()

#Exercise 1 (displ&hwy)
library(ggplot2)
ggplot(data, mapping = aes(x = displ, y = hwy, color=class))+
  geom_point()
#exercise 1 (class&drv)
library(ggplot2)
ggplot(data, mapping = aes(x = class, y = drv, color=cyl))+
  geom_point()
#Exercise 1b
p <- ggplot(data = gapminder,
              mapping = aes(x = gdpPercap, y = lifeExp,))
p + geom_point()
#Plot with large orange line to fit  
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp,))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + 
  scale_x_log10()  
#Plot with Titles   
p <- ggplot(data = gapminder,
              mapping = aes(x = gdpPercap, y = lifeExp, color=continent, fill=continent))
p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")
#Plot with multiple lines 
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

#Plot with one line
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color=continent))+ 
  geom_smooth()+
  scale_x_log10()
#individual aes settings
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")

#--------------------------Exercise 2--------------------------------------------

#read bank.csv into R
bank = read.csv('https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv', header = TRUE)

#check data type
print(bank)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

#subset students out of the data
stu = subset(bank, job == 'student')
print (stu)

#subset entrepreneurs out of the data
ent = subset(bank, job == 'entrepreneur')
print (ent)


#First visual: students without loans
se <- ggplot(data1 = stu,
             mapping = aes(x = stu$age, y = stu$balance, color = stu$loan))
se +geom_point()+
  labs(x = "Student Age", y = "Student Bank Balance",
       title = "Student Loan Demand",
       subtitle = "",
       caption = "Source: bank.csv")

#Second visual: entrepreneurs without loans
se2 <- ggplot(data2 = ent,
              mapping = aes(x = ent$education, y = ent$age, color = ent$loan))
se2 + geom_point()+
  labs(x = 'Ent. Education', y = 'Ent. Age',
       title = 'Entrepreneur Loan Demand',
       subtitle = "",
       caption = 'Sourece: bank.csv')
 
      
 
  
  
  
  
  
  
  
  
  
  
      