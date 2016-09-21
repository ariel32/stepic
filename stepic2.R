rm(list = ls())
library(dplyr); library(ggplot2)
Sys.setlocale("LC_ALL","English")

d = read.csv("stepic.data.set.csv", sep = ";")
d$time <- as.Date(d$time)

# %d day as a number
# %a abbreviated weekday 
# %A unabbreviated weekday
# %m month
# %b abbreviated month
# %B unabbreviated month
# %y 2-digit year
# %Y 4-digit year
###############################################################################
d %>% mutate(time = ordered(format(time, format = "%b"), month.abb), text) %>%
  group_by(time,text) %>% summarise(Total = n()) -> d1
ggplot(d1, aes(x = time,y=Total)) + geom_area(aes(group = text, fill = text),colour = "black")
###############################################################################
d %>% mutate(time = ordered(format(time, format = "%b"), month.abb), text) -> d2
ggplot(d2, aes(x=time, fill=text)) + geom_density(alpha=0.25)
###############################################################################
d %>% mutate(month = ordered(format(time, format = "%b"), month.abb),year = format(time,format="%Y"), text) %>%
  group_by(month,year,text) %>% summarise(Total = n()) -> d3
ggplot(d3, aes(month, Total,colour = text)) + geom_boxplot() +
  stat_summary(fun.y=median, geom="line", aes(group=1))  + 
  stat_summary(fun.y=median, geom="point") +
  facet_grid(.~text) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
###############################################################################
library("corrplot")
d %>% mutate(time = ordered(format(time, format = "%b"), month.abb), text) %>%
  group_by(time,text) %>% summarise(Total = n()) %>% reshape2::acast(time~text) -> d4

M<-cor(d1, method = "sp")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(d4)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black",# tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 1, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

###############################################################################
d %>% mutate(time = ordered(format(time, format = "%A"),
                            c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")), text) %>%
  group_by(time,text) %>% summarise(Total = n()) -> d5
ggplot(d5, aes(x = time,y=Total)) +
  geom_area(aes(group = text, fill = text),colour = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
###############################################################################
