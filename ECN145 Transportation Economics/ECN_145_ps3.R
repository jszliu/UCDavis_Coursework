############ ECN 145 Problem Set 3 ############

# read data
ps3=read.csv(file="ps3_data.csv")  

######## 2. summarize the data


######## 3 & 4. average speed by hour, direction for weekdays or weekend
# creat weekday dummy
ps3$weekday <- ifelse( ps3$dow >= 1 & ps3$dow <= 5,1,0)
# calculate average speed
avg_speed <- aggregate(x = ps3$speed, by = list(ps3$weekday, ps3$hour, ps3$direction  ), 
                           FUN = mean, na.rm =T)  

# subset data for "Weekday + Eastbound", and plot average speed with hour
avg_speed.g1 <- avg_speed[avg_speed$Group.1 == "1" & avg_speed$Group.3 == "E", ]
plot(avg_speed.g1$Group.2, avg_speed.g1$x , xlab="hour", 
     ylab="average speed ", pch=19)

# please do it by yourself for the other 3 groups:
# weekday Westbound
avg_speed.g2 <- avg_speed[avg_speed$Group.1 == "1" & avg_speed$Group.3 == "W", ]
plot(avg_speed.g1$Group.2, avg_speed.g2$x , xlab="hour", 
     ylab="average speed ", pch=19)


# weekend Eastbound
avg_speed.g3 <- avg_speed[avg_speed$Group.1 == "0" & avg_speed$Group.3 == "E", ]
plot(avg_speed.g1$Group.2, avg_speed.g3$x , xlab="hour", 
     ylab="average speed ", pch=19)

# weekend Westbound
avg_speed.g3 <- avg_speed[avg_speed$Group.1 == "0" & avg_speed$Group.3 == "W", ]
plot(avg_speed.g1$Group.2, avg_speed.g3$x , xlab="hour", 
     ylab="average speed ", pch=19)

########  5. regression model
# subset data to "Weekday + Westbound"
Weekday_West <- ps3[ps3$weekday == "1" & ps3$direction == "W", ]
# liner model with hour as factor 
model1 <- lm(speed ~ factor(hour), data=Weekday_West)
summary(model1) 


########  7. marginal private cost
# save model results
model1Summary <- summary(model1) 
# save model coefficients
model1Coeffs <- model1Summary$coefficients
# find out the constant and coefficient of hour for 5pm
a <-model1Coeffs["(Intercept)", "Estimate"]
b <-model1Coeffs["factor(hour)17", "Estimate"]
# calculate MPC using a and b, by yourself



########  8. calculate traffic density
ps3$density <- ps3$volume / ps3$speed



########  9. include density in regression
model2 <- lm(speed ~ density + factor(hour), data=Weekday_West)
summary(model2) 


########  10. plot relationship between traffic volumes and traffic density
#subset data to westbound
Westbound <- ps3[ps3$direction == "W",]
plot(Westbound$density, Westbound$volume , xlab="Traffic Density (cars per mile)", 
     ylab="Traffic Volume (cars per hour) ", pch=19)


######## 11. calculate MEC for 5pm weekday
# save model results from q9
model2Summary <- summary(model2) 
model2Coeffs <- model2Summary$coefficients
# find out the constant and coefficient of hour for 5pm
c <-model2Coeffs["(Intercept)", "Estimate"]
d <-model2Coeffs["factor(hour)17", "Estimate"]
e <-model2Coeffs["density", "Estimate"]

# find out average density at 5pm on weekday westbound
sub_5pm <- ps3[ps3$direction == "W" & ps3$weekday == "1" & ps3$hour == "17" ,]
f <- mean(sub_5pm$density,  na.rm =T)
# use MEC formula with c d e f, by yourself
MEC_5pm <- (f - 1)* (20/(c + d + e*f)-20/(c + d + e*(f-1)))
print(MEC_5pm)

######## 12. calcualte MEC for 6am weekday

# note that you need to change the value of "d" and "f" for 6am

c <-model2Coeffs["(Intercept)", "Estimate"]
d<-model2Coeffs["factor(hour)6", "Estimate"]
e <-model2Coeffs["density", "Estimate"]

# find out average density at 6am on weekday westbound
sub_6am <- ps3[ps3$direction == "W" & ps3$weekday == "1" & ps3$hour == "6" ,]
f=mean(sub_6am$density,  na.rm =T)


# use MEC formula with c d e f, by yourself
MEC_6am <- (f - 1)* (20/(c + d + e*f)-20/(c + d + e*(f-1)))
print(MEC_6am)


