#Econ 294A Assignment- 3
print("Lexi Wolford")
print("1445763")
print("awolford@ucsc.edu")

#1 Load data as dataframe
library(foreign)
df.ex<-read.dta(file='https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta')

#2 Filter
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)

df.ex.2<-df.ex %>%
  dplyr::filter(year == 2013 & month == 12)
#2 Part a
print(nrow(df.ex.2))
print("There are 13261 observations remaining.")

#2 Part b
df.ex.2 <- df.ex %>%
  dplyr::filter(year == 2013 & (month==7 | month == 8 | month == 9))
print(nrow(df.ex.2))
print("There are 39657 remaining observations.")

#3 Arrange
df.ex.3a <- df.ex %>%
  dplyr::arrange(year, month)

#4 Select
#4 Part a
df.ex.4a<-select(df.ex, year:age)
head(df.ex.4a)

#4 Part b
df.ex.4b<-select(df.ex, year, month, starts_with("i"))
head(df.ex.4b)

#4 Part c
distinct(select(df.ex,state))

#5 Mutate
standardize <- function(x){
  (x - min(x, na.rm = T))/(max(x,na.rm = T) - min(x,na.rm = T))
}

normalize <- function(x){
  (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
}

df.ex.5a<-df.ex %>%
 dplyr::mutate(rw.standardize = standardize(rw),rw.normalize = normalize(rw))

#5 Part b
def.ex.5b<- df.ex %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(rw.standardize = standardize(rw),rw.normalize = normalize(rw),count = n())

head(def.ex.5b)

#6 Summarize

#6 Part a
df.ex.6 <- df.ex %>%
  dplyr::group_by(year, month, state) %>%
  dplyr::summarise(
    min_rw = min(rw, na.rm = TRUE),
    rw.1 = quantile(rw, 0.25, na.rm = TRUE),
    mean_rw = mean(rw, na.rm = TRUE),
    rw.3 = quantile(rw, 0.75, na.rm = TRUE),
    max_rw = max(rw, na.rm = TRUE),
    count = n()
  )
#6 Part b/c
print(df.ex.6 %>%
        ungroup() %>%
        dplyr::arrange(desc(mean_rw)) %>%
        dplyr::select(year, month, state)%>%
        head(1)
  )


#7 Extra Credit
df.ex.7 <- df.ex %>%
  dplyr::arrange(year, month, desc(as.character(df.ex$state)))




