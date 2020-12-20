install.packages("tidyverse")
install.packages("xlsx")
install.packages("readxl") 
install.packages("openxlsx")
install.packages("janitor")
install.packages("tibble")
install.packages("rlist")
install.packages("plot3D")
install.packages("plotly")
install.packages("MASS")
install.packages("lme4")
install.packages("car")
install.packages("onewaytests")
 
library(tibble)
library(janitor)
library(ggpubr)
library(rstatix)
library(openxlsx)
library(rlist)
library(plot3D)
library(plotly)
library(MASS)
library(lme4)
library(car)
library(onewaytests)

setwd("your local directory")
getwd()

remove_drought_days <- function(f1, f2, ...) {
  # This function eliminate the redundant dry days (0 rainfall) in two datasets
  # accordingly. Specific configuration is set by feature variables a, b and c 
  # below. These variables determine when there is a prolonged period of drought,  
  # how many days within should be taken out from that period and starting how
  # many days after last rain does the function start eliminating dry days.
  
  # use f1 for water levels
  # use f2 for rainfall
  
  fLen = length(f1)
  if (fLen != length(f2)) {
    # do a simple validation on datasets. make sure lengths are the same
    stop('Error: factors lengths not equal')
  }
  
  i = 0
  j = -1 # dry days start date
  k = -1 # dry days end date
  l = -1 # days to eliminate from data
  
  for (val in f2) {
    if (val == 0) { # if no rainfall, start tracking dates
      if (k == -1){ 
        if (j == -1) { # new drought period
          j = i
          k = i
        }
      }
      else { # update k
        k = i
      }
    }
    else {
      # below the 3 numbers which determine the feature
      if (k - j > 2) {  # a
        l = k - j - 1   # b
        j = j + 1      # c
        
        while (l > 0) { # delete l times at the location (j)
          f1 <- f1[-j]
          f2 <- f2[-j]
          i = i - 1 # update i, since some elements before are removed
          #fLen = fLen - l
          l = l - 1
        }
      }
      # reset j and k
      j = -1
      k = -1
    }
    i = i + 1
  }
  
  result <- list("WL" = f1, "RF" = f2)
  return(result)
}

#convertToDateTime(f_date)
water_level_data <- read.xlsx("chennai_reservoir_levels.xlsx", detectDates = TRUE)
rain_data <- read.xlsx("chennai_reservoir_rainfall.xlsx", detectDates = TRUE)
#sapply(c(rain_data["POONDI"]), typeof)

#POONDI
###########
f_POONDI_WL <- as.numeric(as.character(factor(water_level_data$POONDI)))
f_POONDI_RF <- as.numeric(as.character(factor(rain_data$POONDI)))
#kruskal.test(f_POONDI_WL ~ f_POONDI_RF)
#hist(f_POONDI_RF, breaks = 300, main="POONDI rainfall data not processed")
tmp <- remove_drought_days(f_POONDI_WL, f_POONDI_RF)
f_POONDI_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_POONDI_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
tmp.data <- data.frame(f_POONDI_WL, f_POONDI_RF)
#hist(f_POONDI_RF, breaks = 300, main="POONDI rainfall data processed")
model1 <- aov(f_POONDI_WL ~ f_POONDI_RF)
#leveneTest(as.numeric(as.character(factor(f_POONDI_WL))), factor(f_POONDI_RF))
#fligner.test(f_POONDI_WL ~ f_POONDI_RF)
#model1.1 <- lm(f_POONDI_WL ~ 0 + f_POONDI_RF)

#kruskal.test(f_POONDI_WL ~ f_POONDI_RF)

#tmp.data <- data.frame(f_POONDI_WL, f_POONDI_RF)
#bf.test(f_POONDI_WL ~ tmp.data, data=tmp.data, alpha = 0.05)

#glmer(f_POONDI_WL ~ (1|f_POONDI_RF), family=poisson(link = "log"))
#modelz<-glm(f_POONDI_WL~f_POONDI_RF,family=poisson)
#summary(modelz) 
#plot(modelz)

#bc <- boxcox(model1.1, lambda = seq(-3,5))
#best.lam = bc$x[which(bc$y==max(bc$y))]
#modelz <- lm((f_POONDI_WL ^ 0.1) ~ f_POONDI_RF)
#plot(modelz)
#hist(residuals(modelz))

#model1.2 <- aov(f_POONDI_WL ~ 1/f_POONDI_RF)
#summary(model1)
#summary(model1.2)

#model1.1 <- lm(f_POONDI_WL ~ 0 + 1/f_POONDI_RF)
#plot(residuals(model1.1))
#summary(model1.1)

#ggqqplot(residuals(model1))
#shapiro_test(residuals(model1.1))
#plot(f_POONDI_RF, resid(model1.1))
#plot(residuals(model1.1), fitted(model1.1))


#shapiro_test(sample(residuals(model1), 5000))
#plot(model1)
ggqqplot(residuals(model1))

#CHOLAVARAM
###########
f_CHOLAVARAM_WL <- as.numeric(as.character(factor(water_level_data$CHOLAVARAM)))
f_CHOLAVARAM_RF <- as.numeric(as.character(factor(rain_data$CHOLAVARAM)))
#kruskal.test(f_CHOLAVARAM_WL ~ f_CHOLAVARAM_RF)
#hist(f_CHOLAVARAM_RF, breaks = 300, main="CHOLAVARAM rainfall data not processed")
tmp <- remove_drought_days(f_CHOLAVARAM_WL, f_CHOLAVARAM_RF)
f_CHOLAVARAM_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_CHOLAVARAM_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
#hist(f_CHOLAVARAM_RF, breaks = 300, main="CHOLAVARAM rainfall data processed")
model2 <- aov(f_CHOLAVARAM_WL ~ f_CHOLAVARAM_RF)
#model2.1 <- lm(f_CHOLAVARAM_WL ~ 0 + f_CHOLAVARAM_RF)
#leveneTest(as.numeric(as.character(factor(f_CHOLAVARAM_WL))), factor(f_CHOLAVARAM_RF))
#fligner.test(f_CHOLAVARAM_WL ~ f_CHOLAVARAM_RF)
#kruskal.test(f_CHOLAVARAM_WL ~ f_CHOLAVARAM_RF)

#REDHILLS
###########
f_REDHILLS_WL <- as.numeric(as.character(factor(water_level_data$REDHILLS)))
f_REDHILLS_RF <- as.numeric(as.character(factor(rain_data$REDHILLS)))
#kruskal.test(f_REDHILLS_WL ~ f_REDHILLS_RF)
#hist(f_REDHILLS_RF, breaks = 300, main="REDHILLS rainfall data not processed")
tmp <- remove_drought_days(f_REDHILLS_WL, f_REDHILLS_RF)
f_REDHILLS_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_REDHILLS_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
#hist(f_REDHILLS_RF, breaks = 300, main="REDHILLS rainfall data processed")
model3 <- aov(f_REDHILLS_WL ~ f_REDHILLS_RF)
#leveneTest(as.numeric(as.character(factor(f_REDHILLS_WL))), factor(f_REDHILLS_RF))
#fligner.test(f_REDHILLS_WL ~ f_REDHILLS_RF)

#model3.1 <- lm(f_REDHILLS_WL ~ 0 + f_REDHILLS_RF)

#kruskal.test(f_REDHILLS_WL ~ f_REDHILLS_RF)

#CHEMBARAMBAKKAM
###########
f_CHEMBARAMBAKKAM_WL <- as.numeric(as.character(factor(water_level_data$CHEMBARAMBAKKAM)))
f_CHEMBARAMBAKKAM_RF <- as.numeric(as.character(factor(rain_data$CHEMBARAMBAKKAM)))
#kruskal.test(f_CHEMBARAMBAKKAM_WL ~ f_CHEMBARAMBAKKAM_RF)
#hist(f_CHEMBARAMBAKKAM_RF, breaks = 300, main="CHEMBARAMBAKKAM rainfall data not processed")
tmp <- remove_drought_days(f_CHEMBARAMBAKKAM_WL, f_CHEMBARAMBAKKAM_RF)
f_CHEMBARAMBAKKAM_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_CHEMBARAMBAKKAM_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
#hist(f_CHEMBARAMBAKKAM_RF, breaks = 300, main="CHEMBARAMBAKKAM rainfall data processed")
model4 <- aov(f_CHEMBARAMBAKKAM_WL ~ f_CHEMBARAMBAKKAM_RF)

#leveneTest(as.numeric(as.character(factor(f_CHEMBARAMBAKKAM_WL))), factor(f_CHEMBARAMBAKKAM_RF))
#fligner.test(f_CHEMBARAMBAKKAM_WL ~ f_CHEMBARAMBAKKAM_RF)

#model4.1 <- lm(f_CHEMBARAMBAKKAM_WL ~ 0 + f_CHEMBARAMBAKKAM_RF)

#kruskal.test(f_CHEMBARAMBAKKAM_WL ~ f_CHEMBARAMBAKKAM_RF)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

#summary(model1.1)
#summary(model2.1)
#summary(model3.1)
#summary(model4.1)

#plot(model1.1)
#plot(model2.1)
#plot(model3.1)
#plot(model4.1)


plot(residuals(model2), fitted(model2))

#shapiro_test(residuals(model1))
#shapiro_test(residuals(model2))
#shapiro_test(residuals(model3))
#shapiro_test(residuals(model4))

plot(residuals(model1))
plot(residuals(model2))
plot(residuals(model3))
plot(residuals(model4))

hist(residuals(model1))
hist(residuals(model2))
hist(residuals(model3))
hist(residuals(model4))

ggqqplot(residuals(model1), main="POONDI res Normal Q")
ggqqplot(residuals(model2), main="CHOLAVARAM res Normal Q")
ggqqplot(residuals(model3), main="REDHILLS res Normal Q")
ggqqplot(residuals(model4), main="CHEMBARAMBAKKAM res Normal Q")


re_list <- list("POONDI" = residuals(model1), "CHOLAVARAM" = residuals(model2), "REDHILLS" = residuals(model3), "CHEMBARAMBAKKAM" = residuals(model4))
boxplot(re_list, horizontal=TRUE)

tests_data <- read.xlsx("feature_results.xlsx")

# x1, y1, z1 variables corresponds to the a, b and c above
x1 <- tests_data$a
y1 <- tests_data$b
z1 <- tests_data$c
R1 <- tests_data$POONDI
R2 <- tests_data$CHOLAVARAM
R3 <- tests_data$REDHILLS
R4 <- tests_data$CHEMBARAMBAKKAM

# in this case we use VAR4 as continuous, you can put color = ~as.factor(VAR4) to have it as factors
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R1) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R2) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R3) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R4) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
