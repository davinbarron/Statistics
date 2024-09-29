############################################################
# Statistics Assignment (Stats and Probability)
#
# Davin Barron / ID: 20102008 / Programme: Applied
#
# R commands to address the questions pertaining to the
# production of small electric motors.
############################################################

# Load the data from the clipboard
production = read.table("clipboard", header=T)

head(production)
names(production)

attach(production)

############################################################

#
# Q1: Are the defective rates for the two production lines operating in the same shift different?
#

# correalation
plot(line_y~line_x, xlab="Line X", ylab="Line Y", pch=20, bty="L", main="Defective rates for two lines")
cor.test(line_y, line_x)

# p < 0.05

tmp = !is.na(line_y) & !is.na(line_x)

HZ.test(data.frame(line_y[tmp], line_x[tmp]))
# joint normal ok

#
# Q2: How, if at all, does defective rate on line X vary between shifts?
#

# ANOVA
boxplot(line_x ~ as.factor(shift), main="Defective Rate on Line X by Shift", xlab="Shift", ylab="Defective Rate")

tapply(line_x, shift, mean, na.rm=T)
tapply(line_x, shift, sd, na.rm=T)
tapply(line_x, shift, length)

summary(aov(line_x~as.factor(shift)))

levene.test(shift, line_x)

#
# Q3: Does the ambient temperature have an impact on the line X defective rate?
#

plot(temperature~line_x, xlab="Line X", ylab="Temperature", pch=20, bty="L", main="Impact of temperature on line X")
cor.test(temperature, line_x)

# p < 0.05

#
# Q4: What effect does having a radio on have on the line X defective rate?
#

boxplot(line_x~radio, horizontal =TRUE, xlab="Line X", ylab="Radio", main="Radio effect on line X")

# Summary stats
tapply(line_x, radio, mean, na.rm=T)
tapply(line_x, radio, sd, na.rm=T)
tapply(line_x, radio, length)

# test
t.test(line_x~radio)

# checking normality
shapiro.test(line_x[radio == '1'])
shapiro.test(line_x[radio == '2'])

#
# Q5: Is the radio more likely to be on one some shifts compared to others?
#

plot(as.factor(radio), as.factor(shift), main="Radio during shift", xlab="Radio", ylab="Shift")

chisq.test(as.factor(radio), as.factor(shift))

#
# Q6: Is there evidence that the line X defective rate is related to ambient noise?
#

plot(line_x~noise, xlab="Noise", ylab="Line X", pch=20, bty="L", main="Line X related to ambient noise")
cor.test(line_x, noise)

#
# Q7: Do motors made on line X for one particular appliance tend to be more prone to defects than others?
#

boxplot(line_x ~ as.factor(appliance), main="Defective Rate on Line X by Appliance", xlab="Appliance", ylab="Defective Rate")

tapply(line_x, appliance, mean, na.rm=T)
tapply(line_x, appliance, sd, na.rm=T)
tapply(line_x, appliance, length)

summary(aov(line_x~as.factor(appliance)))

levene.test(appliance, line_x)

#
# Q8: Are motors made for different appliance made with similar frequencies on all three shifts?
#

plot(as.factor(appliance), as.factor(shift), main="Applicances made on different shift", xlab="Appliance", ylab="Shift")

chisq.test(as.factor(appliance), as.factor(shift))