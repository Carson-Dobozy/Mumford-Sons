###Lab 3----------------

###Exercise 1-----------
library(tidyverse)


#load csv data
amesdata <- read.csv("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",",
                     )

#RMSE function
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
#complexity function
get_complexity = function(model) {
  length(coef(model)) - 1
}



#remove qual and cond
amesdata$OverallCond <- amesdata$OverallQual <- NULL

#series of linear regression models
#I know this is gross but i spent hours troubleshooting with another cleaner looking method 
#but it kept giving me issues when i tried to graph
model_x1 = lm(SalePrice~1, data = amesdata)
model_x2 = lm(SalePrice~LotArea, data = amesdata )
model_x3 = lm(SalePrice~LotArea + MSSubClass, data = amesdata)
model_x4 = lm(SalePrice~LotArea + MSSubClass + Utilities, data = amesdata)
model_x5 = lm(SalePrice~LotArea + MSSubClass+ Utilities + BldgType, data = amesdata)
model_x6 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle, data = amesdata)
model_x7 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt, data = amesdata)
model_x8 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                HouseStyle, data = amesdata )
model_x9 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                HouseStyle + BedroomAbvGr, data = amesdata)
model_x10 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd, data = amesdata)
model_x11 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd + BsmtQual, data = amesdata)
model_x12 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd + BsmtQual + BsmtFinSF1 , data = amesdata)
model_x13 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd + BsmtQual + BsmtFinSF1 + GrLivArea , data = amesdata)
model_x14 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd + BsmtQual + BsmtFinSF1 + GrLivArea + GarageCars , data = amesdata)
model_x15 = lm(SalePrice~LotArea + MSSubClass + Utilities + BldgType + HouseStyle + YearBuilt + 
                 HouseStyle + BedroomAbvGr + Exterior2nd + BsmtQual + BsmtFinSF1 + GrLivArea + GarageCars + GarageType , data = amesdata)
# get complexities
complx1 = get_complexity(model_x1)
complx2 = get_complexity(model_x2)
complx3 = get_complexity(model_x3)
complx4 = get_complexity(model_x4)
complx5 = get_complexity(model_x5)
complx6 = get_complexity(model_x6)
complx7 = get_complexity(model_x7)
complx8 = get_complexity(model_x8)
complx9 = get_complexity(model_x9)
complx10 = get_complexity(model_x10)
complx11 = get_complexity(model_x11)
complx12 = get_complexity(model_x12)
complx13 = get_complexity(model_x13)
complx14 = get_complexity(model_x14)
complx15 = get_complexity(model_x15)
#store in a list
complexes = list(complx1, complx2, complx3, complx4, complx5, complx6, complx7, 
                 complx8, complx9, complx10, complx11, complx12, complx13, complx14, complx15)
#grab rmse values
rmse_1 = rmse(fitted(model_x1), predict(model_x1))
rmse_2 = rmse(fitted(model_x2), predict(model_x2))
rmse_3 = rmse(fitted(model_x3), predict(model_x3))
rmse_4 = rmse(fitted(model_x4), predict(model_x4))
rmse_5 = rmse(fitted(model_x5), predict(model_x5))
rmse_6 = rmse(fitted(model_x6), predict(model_x6))
rmse_7 = rmse(fitted(model_x7), predict(model_x7))
rmse_8 = rmse(fitted(model_x8), predict(model_x8))
rmse_9 = rmse(fitted(model_x9), predict(model_x9))
rmse_10 = rmse(fitted(model_x10), predict(model_x10))
rmse_11 = rmse(fitted(model_x11), predict(model_x11))
rmse_12 = rmse(fitted(model_x12), predict(model_x12))
rmse_13 = rmse(fitted(model_x13), predict(model_x13))
rmse_14 = rmse(fitted(model_x14), predict(model_x14))
rmse_15 = rmse(fitted(model_x15), predict(model_x15))
rmse_x = list(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7,
              rmse_8, rmse_9, rmse_10, rmse_11, rmse_12, rmse_13, rmse_14, rmse_15)

#plot the complexities vs rmse score
plot(complexes,rmse_x, main = 'Complexities v RMSE',
     xlab = 'Complexities', 
     ylab = 'RMSE', 
     col = 'blue', cex = .75 )

#with this plot we can see that the less complex models had a lower RMSE leading us to the conclusion
#that we shouldnt use the full model because it may not be as accurate as a model with fewer variables
#although this doesnt mean we should use the smallest model.
#as we can see, models in the 5-15 complexity range have very low RMSE scores, almost identical to that of 
#the models with 1-4 complexities
#we can see a slight bell shape pattern with low RMSE scores on eaither end of the complexity scale
#There is an outlier with more than 50 complexities that has a low RMSE, which may tell us that there is a threshold
#for when RMSE begins to drop with the high complexity models


#Exercise 2 ----------

set.seed(9)
num_obs = nrow(amesdata)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = amesdata[train_index, ]
test_data = amesdata[-train_index, ]

fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)
sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))
sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))
rmse(actual = train_data$SalePrice, predicted = predict(fit_0, train_data))
rmse(actual = test_data$SalePrice, predicted = predict(fit_0, test_data))

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))}

get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE


fit_1 <- lm(SalePrice ~ LotArea, 
            data = train_data)
fit_2 <- lm(SalePrice ~ LotArea + MSSubClass, 
            data = train_data )
fit_3 <- lm(SalePrice ~ LotArea + MSSubClass + Utilities, 
            data = train_data)
fit_4 <- lm(SalePrice ~ LotArea + MSSubClass + Utilities + BldgType,
            data = train_data)
fit_5 <- lm(SalePrice ~ LotArea + MSSubClass + Utilities + BldgType + HouseStyle, 
            data = train_data)

model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)

train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)

#same as sapply above
test_rmse = c(get_rmse(fit_1, test_data, "SalePrice"),
              get_rmse(fit_2, test_data, "SalePrice"),
              get_rmse(fit_3, test_data, "SalePrice"),
              get_rmse(fit_4, test_data, "SalePrice"),
              get_rmse(fit_5, test_data, "SalePrice"))
#plot the data
plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")

lines(model_complexity, test_rmse, type = "b", col = "darkorange")

#1 plot Train and Test RMSE for the 15 models from part 1

get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice")


list_models = list(model_x1, model_x2, model_x3, model_x4, model_x5, model_x6, model_x7, 
                   model_x8, model_x9, model_x10, model_x11, model_x12, model_x13, model_x14, model_x15 )

train_rmse2 = sapply(list_models, get_rmse, data = train_data, response = "SalePrice")
test_rmse2 = sapply(list_models, get_rmse, data = test_data, response = "SalePrice")
model_complexity2 = sapply(list_models, get_complexity)

plot(model_complexity2, train_rmse2, type = "b",
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")

lines(model_complexity2, test_rmse2, type = "b", col = "darkorange")

#2 saleprice regression/ train and test/ plot
model_x1x = lm(SalePrice~1, 
               data = amesdata)
model_x2x = lm(SalePrice~Neighborhood, 
               data = amesdata )
model_x3x = lm(SalePrice~Neighborhood+ OpenPorchSF, 
               data = amesdata)
model_x4x = lm(SalePrice~Neighborhood+ OpenPorchSF + GarageCars, 
               data = amesdata)
model_x5x = lm(SalePrice~Neighborhood+ OpenPorchSF + GarageCars + 
                 X1stFlrSF, data = amesdata)
model_x6x = lm(SalePrice~Neighborhood+ OpenPorchSF + GarageCars + 
                 X1stFlrSF + HouseStyle, data = amesdata)
model_x7x = lm(SalePrice~Neighborhood+ OpenPorchSF + GarageCars + 
                 X1stFlrSF + HouseStyle + TotRmsAbvGrd, data = amesdata)
model_x8x = lm(SalePrice~Neighborhood+ OpenPorchSF+ GarageCars + 
                 X1stFlrSF + HouseStyle + TotRmsAbvGrd + GarageArea, data = amesdata)
model_x9x = lm(SalePrice~Neighborhood+ OpenPorchSF+ GarageCars + 
                 X1stFlrSF + HouseStyle + TotRmsAbvGrd + GarageArea + WoodDeckSF , data = amesdata)
model_x10x = lm(SalePrice~Neighborhood+ OpenPorchSF+ GarageCars + 
                 X1stFlrSF + HouseStyle + TotRmsAbvGrd + GarageArea + WoodDeckSF + TotalBsmtSF , data = amesdata)

model_list2 = list(model_x1x, model_x2x, model_x3x, model_x4x, model_x5x, model_x6x, model_x7x, model_x8x, model_x9x, model_x10x)

train_rmse2 = sapply(model_list2, get_rmse, data = train_data, response = "SalePrice")
test_rmse2 = sapply(model_list2, get_rmse, data = test_data, response = "SalePrice")
model_complexity2 = sapply(model_list2, get_complexity)

plot(model_complexity2, train_rmse2, type = "b",
     ylim = c(min(c(train_rmse2, test_rmse2)) - 0.02,
              max(c(train_rmse2, test_rmse2)) + 0.02),
     col = "green",
     xlab = "Model Size",
     ylab = "RMSE",
     main = 'Train/Test SalePrice')
lines(model_complexity2, test_rmse2, type = "b", col = "black")

