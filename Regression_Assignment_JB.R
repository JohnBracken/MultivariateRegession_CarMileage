#Generate exploratory box plot and summarize mpg data 
car_data <- mtcars
plot_box <- boxplot(data= car_data,mpg~am, main="Car Mileage vs. Transmission Type",
xlab="Transmission Type", ylab="Miles Per Gallon",
col = c("gold"), names=c("Automatic", "Manual"))
mpg_summary <-summary(car_data$mpg)
print(mpg_summary)


#Student's t-test comparing  
 test_compare <- t.test(data=car_data, mpg~am)
 print(test_compare)

 #Linear regression fitting mpg to transmission type only.
 model_transmission <- lm(mpg~am, data = car_data)
 summary_lm <- summary(model_transmission)
 print(summary_lm)
 
 par(mfrow=c(1,2))
 plot_lm <- plot(car_data$am, car_data$mpg, main ="Regression of MPG 
 vs. transmission type", xlab = "Transmission Type",
 ylab = "Miles per gallon", pch = 21, bg = "lightgreen", col = "black",xaxt = 'n',
 cex=1.5)
 axis(side = 1, at = c(0,1),labels = c("Automatic", "Manual"))
 abline(model_transmission, lwd = 2)
 
 #Plot the residuals
 resid_plot <- plot(car_data$am, resid(model_transmission), main = "Residual plot:
 transmission", xlab = "Transmission Type", ylab = "Residuals", pch = 21, 
 bg = "lightgreen", col = "black", xaxt = 'n', cex=1.5)
 axis(side = 1, at = c(0,1),labels = c("Automatic", "Manual"))
 abline(h=0, lwd = 2)
 
 #Fit wasn't very good, so try a muliple regression with variables that I think are
 #most relevant to affecting miles per gallon.
 
 #Get correlations of other variables wrt miles per gallon.  The first row of the correlation
 #matrix is the correlation of mpg wrt. to other variables.
 correlations <- cor(mtcars)[1,]
 correlations_weight <- cor(mtcars)[6,]
 print(correlations)
 print(correlations_weight)
 
 mr_model <- update(model_transmission,mpg~am+wt-1, data = car_data)
 mr_model2 <-update(model_transmission, mpg~am+wt+qsec-1, data=car_data)
 mr_model3 <-update(model_transmission, mpg~am+wt+qsec+disp-1, data=car_data)
 
 summary_mr <- summary(mr_model)
 summary_mr2 <- summary(mr_model2)
 summary_mr3 <- summary(mr_model3)

 print(summary_mr)
 print(summary_mr2)
 print(summary_mr3)

 
 #Do the residual plot for the best multivariate regression
 par(mfrow = c(2,2))
 plot(mr_model2)
 
 anova_results <- anova(mr_model,mr_model2, mr_model3)
 print(anova_results)
 