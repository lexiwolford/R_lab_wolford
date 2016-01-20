# Assignment 1 
# Problem 0
print ("Alexa Wolford 1445763")

#Problem 1
#Need to download foreign to allow R to read data sets
library(foreign)
df.dta <- read.dta(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta')

df.csv <- read.csv(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv')

df.td <- read.table(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt')

load(url('https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData'))

#Problem 2
dtaSize <- format(object.size(df.dta), units = "KB") 
print(dtaSize)
csvSize <- format(object.size(df.csv), units = "KB") 
print(csvSize)
tdSize <- format(object.size(df.td), units = "KB") 
print(tdSize)
rSize <- format(object.size(NHIS_2007_RData), units = "KB")
print(rSize)

print("RData and CSV Data are the smallest size")

print("The reason size varies depending on document type is certain formats use multiple characters to display the same outcome, taking up more memory")

#Problem 3
type <- typeof(NHIS_2007_RData)
print(type)

class <- class(NHIS_2007_RData)
print(class)

length <- length(NHIS_2007_RData)
print(length)
dimmension <- dim(NHIS_2007_RData)
print(dimmension)
rows <- nrow(NHIS_2007_RData)
print(rows)
columns <- ncol(NHIS_2007_RData)
print(columns)
summary(NHIS_2007_RData)
print(summary)

#Problem 4
df <- read.dta(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta')
structure <- str(df)
print(structure)

data.frame(df)
print(data.frame(df))
print("There are 1119754 observation and 30 variables")

print(min(df$rw, na.rm = TRUE))
print(mean(df$rw, na.rm = TRUE))
print(median(df$rw, na.rm = TRUE))
print(max(df$rw, na.rm = TRUE))
print(quantile(df$rw, na.rm = TRUE))
print("The 1st Quartile is 10.704, the 3rd Quartile is 24.35")

print(sum(is.na(df$rw)))

#Problem 5
vector <- c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
print(length(vector))
#Null is a reserved word, so the reported lengths on the vector do not match the actual number of objects

print(mean(vector, na.rm = TRUE))

#Problem 6
x = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3, TRUE)
print(x)

print(t(x))

eigen(x, only.values = FALSE)
print(eigen(x, only.values = FALSE))

y = matrix(c(1, 2, 3, 3, 2, 1, 2, 3, 0), nrow=3, ncol=3, TRUE)
print(y)

inverse <- solve(y)
print(inverse)
identity <- solve(y) %*% y
print(identity)
#This is the identity matrix

#Problem 7
carat = c(5, 2, 0.5, 1.5, 5, NA, 3)
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair")
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA)
price = c(850, 450, 450, "NULL", 750, 980, 420)
diamonds = data.frame(carat, cut, clarity, price)
print(diamonds)


vecPrice <- as.numeric(as.character(diamonds$price))
mean(vecPrice, na.rm=TRUE)
print(vecPrice)

s <- subset(diamonds, cut == "fair")
s_new <- as.numeric(as.character(s$price))
mean(s_new)
print("Mean Price of fair cut is 673.333")

subset<- subset(diamonds, carat >= 2 & cut == "Ideal" | cut == "very good")
sub_new <- as.numeric(as.character(subset$price))
median(sub_new, na.rm = TRUE)
print("Mean Price of good, very good, and ideal cut is 980")



