#### Assignment 5 ######

############
##Part 1 A##
############

library(ggplot2)
head(diamonds)

p1a<-ggplot(diamonds,
       aes(x = log(x*y*z), y = log(price)))
p1a + geom_point()
p1a + geom_point(aes(colour = clarity, size = carat, alpha = 0.5))


############
##Part 1 B##
############

p1b<-ggplot(diamonds, aes(x = carat, y = ..density..)) + facet_wrap(~cut)
p1b + geom_histogram(aes(fill = clarity)) + facet_grid(cut ~ .)

############
##Part 1 C##
############

p1c<-ggplot(diamonds, aes(x = cut, y = price))
p1c + geom_violin() + geom_jitter(alpha = .0123)

############
##Part 2 A##
############
library(foreign)
org<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

library(dplyr)

org2<- org %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(
    rw_median = median(rw, na.rm=TRUE),
    rw_1 = quantile(rw, 0.25, na.rm = TRUE),
    rw_3 = quantile(rw, 0.75, na.rm = TRUE),
    rw_dec1 = quantile(rw, prob=1/10, na.rm = TRUE, type = 5),
    rw_dec9 = quantile(rw, prob=9/10, na.rm = TRUE, type = 5)
  )

org2$Date<-as.Date(paste(org2$month, org2$year, "01", sep = "."), format = "%m.%Y.%d")


p2a<-ggplot(org2,aes(x=Date, y=rw_median))
p2a + geom_line() + geom_ribbon(aes(ymin = rw_dec1, ymax = rw_dec9), alpha = 0.2) +
geom_ribbon(aes(ymin = rw_1, ymax = rw_3), alpha = 0.2)

############
##Part 2 B##
############

org2b<- org %>%
  dplyr::group_by(year, month, educ) %>%
  dplyr::summarise(
      rw_median = median(rw, na.rm = TRUE)
  )
org2b$Date<-as.Date(paste(org2b$month, org2b$year, "01", sep = "."), format = "%m.%Y.%d")

p2b<-ggplot(org2b, aes(x=Date, y=rw_median, group=educ, colour=educ)) 
p2b + geom_line()
