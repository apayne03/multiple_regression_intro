library(tidyverse)

my.data <- read_csv("regLectureData.csv")
glimpse(my.data)

library(apaTables)
apa.cor.table(my.data)

# make sure no curvilinear relationships... all look more or less normal here
psych::pairs.panels(as.data.frame(my.data))

my.regression <- lm(VidScore ~ iq + age, data=my.data) # this gives you the b-weights
my.regression # more detailed info; estimate = b-weight (.32846, -.37115)
summary(my.regression)

apa.reg.table(my.regression)
# see if r is zero and beta is something that is not zero... something weird would be going on then, but that is not the case in this example

x_axis_range <- data.frame(age = c(43), iq=c(130))
x_axis_range
CI_data <- predict(my.regression, newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data


PI_data <- predict(my.regression, newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data
# telling you about future sample statistics - can expect 95% of people in future samples to score between 110 to 147 --> learn the differences between CI and PI again 



# Hierarchical Regression 
head(attitude)
reg1 <- lm(rating ~ complaints + privileges, data=attitude)
reg2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
anova(reg1, reg2) # tells you whether the delta r-squared between them is significant; if non-significant then learning didn't add incrementally; you would never do this, you'd do the command below, but this just shows how it's determinging if Delta R-squared is significant (would be starred)
apa.reg.table(reg1, reg2)
