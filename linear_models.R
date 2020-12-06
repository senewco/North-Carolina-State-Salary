# Sam Newcomer
# 12-1-2020

# Clear preexisting environment variables.
rm(list = ls())

# Include graphing library
library(ggplot2)

# Manipulation of plot axes.
require(scales)

# Read salary csv file.
sal = read.csv("nc_salary.csv", header=TRUE)

# Read administration subset csv file.
admin = read.csv("admin.csv", header=TRUE)

# Read information technology subset csv file.
infotech = read.csv("infotech.csv", header=TRUE)

# Read judicial subset csv file.
jud = read.csv("judicial.csv", header=TRUE)

# Read public safety subset csv file.
pubsafe = read.csv("pubsafe.csv", header=TRUE)

# Linear regression predicting salary.
lm_age <- lm(salary ~ age, sal)
# Shows how well the model performs.
summary(lm_age)                 
# Plot of total data.
plot <- ggplot(sal, aes(x = age, y = salary)) + geom_point(color="blue") + geom_smooth(method = "lm", col = "red")+ ggtitle("Salaries of NC State")
plot + scale_y_continuous(labels = comma)

# Linear regression predicting administration salary.
lm_admin <- lm(salary ~ age, admin)
# Plot of administration data.
plot <- ggplot(admin, aes(x = age, y = salary)) + geom_point(color="purple") + geom_smooth(method = "lm", col = "red") + ggtitle("Administration")
plot + scale_y_continuous(labels = comma)

# Linear regression predicting information techology salary.
lm_it <- lm(salary ~ age, infotech)
# Plot of IT data.
plot <- ggplot(infotech, aes(x = age, y = salary)) + geom_point(color="purple") + geom_smooth(method = "lm", col = "red") + ggtitle("Information Technology")
plot + scale_y_continuous(labels = comma)

# Linear regression predicting judicial system salary.
lm_jud <- lm(salary ~ age, jud)
# Plot of judicial data.
plot <- ggplot(jud, aes(x = age, y = salary)) + geom_point(color="purple") + geom_smooth(method = "lm", col = "red") + ggtitle("Judicial")
plot + scale_y_continuous(labels = comma)

# Linear regression predicting public safety salary.
lm_pubs <- lm(salary ~ age, pubsafe)
# Plot of public safety data.
plot <- ggplot(pubsafe, aes(x = age, y = salary)) + geom_point(color="purple") + geom_smooth(method = "lm", col = "red") + ggtitle("Public Safety")
plot + scale_y_continuous(labels = comma)

