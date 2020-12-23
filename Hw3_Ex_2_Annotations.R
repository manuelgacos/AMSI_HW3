# Set Working directory
setwd('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW3')

# Read the table with the information of the data
data <- read.delim('CH03PR18.txt', sep = '', dec = '.', header = FALSE)
names(data) <- c('Production_Time','Lot_Size')

#### Make the linear regression model with the original data ###
linearMod = lm(formula = Production_Time~Lot_Size, data = data)

# Obtain the regression coefficients
coefficients = coef(linearMod)
beta_1 = coefficients[2]
beta_0 = coefficients[1]

# Plotting the data and the estimated regression function
plot(data$Lot_Size,data$Production_Time, xlab = names(data)[2], ylab = names(data)[1], 
     main = 'Plotted data and regression line', col = 'blue')
# PLots the regression line
abline(lm(data$Production_Time ~ data$Lot_Size))

# Linear regression function with original data
y_hat <- beta_0+beta_1*data[,2]

# Obtain the residuals of the linear model
linearMod_res <- resid(linearMod) # Or just linearMod$residuals

# Plot the residuals against the fitted values
plot(y_hat, linearMod_res, xlab = "Fitted_Values", ylab = "Residuals", 
     main = 'Residual plot against Y_Hat', col = 'blue')
abline(a=0, b=0)

# Constant variance in error terms
plot(data$Production_Time, abs(linearMod$residuals), xlab = "Production_Time", 
     ylab = "Estimated residuals", main = "Constant variance in errors?")
abline(h=0,col="blue")

# Q-Q plot
qqnorm(linearMod$residuals, xlab = "Theoretical normal quantiles", ylab = "Estimated 
       residual quantiles")
qqline(linearMod$residuals, col = "blue")

### Linear model with the transformation X' = sqrt(X) ###

# Transforming the data
data_1 <- data
data_1[,'xsqrt'] <- sqrt(data_1[,2])
names(data_1)[1:2] <- c('Production_Time','Lot_Size_Trans')

# Make the linear regression model with the transformed data
linearMod_1 = lm(formula = Production_Time~Lot_Size_Trans, data = data_1)
linearMod_1 = lm(formula = Production_Time~xsqrt, data = data_1)
summary(linearMod_1)

# Obtain the regression coefficients
coefficients_1 = coef(linearMod_1)
beta_1_trans = coefficients_1[2]
beta_0_trans = coefficients_1[1]

plot(linearMod_1,1)
plot(linearMod_1,2)

# Plotting the data and the estimated regression function
plot(data_1$Lot_Size_Trans,data_1$Production_Time, xlab = names(data_1)[2], ylab = names(data_1)[1], 
     main = 'Plotted transformed data and regression line', col = 'blue')
# PLots the regression line
abline(lm(data_1$Production_Time ~ data_1$Lot_Size_Trans))

# Linear regression function with transformed data
y_hat_trans <- beta_0_trans+beta_1_trans*data_1[,2]

# Plot the residuals against the fitted values with the transformed data
plot(y_hat_trans, linearMod_1$residuals, xlab = "Fitted_Values", ylab = "Residuals", 
     main = 'Residual plot against Y_Hat with transformed data', col = 'blue')
abline(a=0, b=0)

# Constant variance in error terms (Transformed data)
plot(data_1$Production_Time, abs(linearMod_1$residuals), xlab = "Production_Time", 
     ylab = "Estimated residuals", main = "Constant variance in errors? (Transformed data)")
abline(h=0,col="blue")

# Q-Q plot
qqnorm(linearMod_1$residuals, xlab = "Theoretical normal quantiles", ylab = "Estimated 
       residual quantiles")
qqline(linearMod$residuals, col = "blue")
