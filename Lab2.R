library(dplyr)
library(tidyverse)
library(lattice)
install.packages("corrplot")
library(corrplot)


#Part 1-----------------

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
names(ameslist)
typeof(ameslist)
unique(ameslist$GarageType)

GarageTemp = model.matrix(~GarageType - 1, data=ameslist)

ameslist <- cbind(ameslist[1:1379,], GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

#take out integer related data from data frame
Ames = ameslist %>%
  select_if(is.integer)

#Selecting 12 variables our of Ames
Ames_12 = select(Ames, SalePrice, YearBuilt, YearRemodAdd, 
                 OverallQual, OverallCond, GrLivArea,
                 TotalBsmtSF, GarageArea, GarageCars,
                 TotRmsAbvGrd, BedroomAbvGr, FullBath)
#Matrix with all 12 variables
pairs(~SalePrice+YearBuilt+YearRemodAdd+OverallQual+OverallCond+GrLivArea+TotalBsmtSF+GarageArea+GarageCars+TotRmsAbvGrd+BedroomAbvGr+FullBath,
      col= Ames$SalePrice,
      data=Ames, 
      main="Matrix of 12 Variables")

#4 different Matrices with 3 variables each for better visualization
par(mfrow=c(2,2))
pairs(~SalePrice+YearBuilt+YearRemodAdd,
      col= Ames$SalePrice,
      data=Ames, 
      main="Matrix 1 of 4")


pairs(~OverallQual+OverallCond+GrLivArea,
          col= Ames$SalePrice,
          data=Ames, 
          main="Matrix 2 of 4")

pairs(~TotalBsmtSF+GarageArea+GarageCars,
          col= Ames$SalePrice,
          data=Ames, 
          main="Matrix 3 of 4")

pairs(~TotRmsAbvGrd+BedroomAbvGr+FullBath,
            col= Ames$SalePrice,
            data=Ames, 
            main="Matrix 4 of 4")

#correlation of 12 variables
corr_coeff = cor(Ames_12)
corr_coeff
#creating a correlation matrix with this data
corrplot(corr_coeff)

#simple regression between two variables
lmsp = lm(Ames_12$GrLivArea ~ Ames_12$SalePrice)

#plotting two variables and adding line of best fit 
plot(Ames_12$SalePrice, Ames_12$GrLivArea, 
     col = "darkgreen", 
     main = "SalePrice x GrLivArea", 
     xlab = "Sale Price",
      ylab = "Gr Living Area")
abline(lmsp, lty = 2, lwd = 3)



#Part 2-----------------------
#attach(Ames)
#lm.fit = lm(SalePrice ~ GrLivArea)
#summary(lm.fit)
#lm.fit$coefficients
#plot(lm.fit)
#lm.fit = lm(SalePrice ~ GrLivArea + LotArea)
#------
#regression with 1 var
lin.reg = lm(SalePrice~GarageOutside, data = ameslist)
summary(lin.reg)
#ggplot to help prove garage value
p = ggplot(ameslist,
            aes(x = GarageArea, y = SalePrice, color = GarageOutside))
p + geom_point()+ geom_smooth(method=lm)+
  labs(x = 'Garage Area', y= 'Sale Price', title = 'Garage Area on Sale Price')+ 
  scale_y_log10(labels = scales::dollar)
#multiple regression with all AMES variables
multi.lin.reg = lm(SalePrice~MSSubClass+LotFrontage+LotArea+OverallQual+
                   OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+
                   BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+
                   X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+
                   BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+
                   KitchenAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+
                   GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+
                   EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
                   MiscVal+MoSold+YrSold, data=Ames)
summary(multi.lin.reg)
#we can see that OverallQual, GarageCars, MasVnrArea, and TotRmsAbvGrd seem to have a significant effect on SalePrice
#These variables all have high t values (well over 1.96) suggesting they have an effect in this regression
#The Coefficient on the year variable suggest that it has a strong relationship with the response variable



#plot residual graphs in one panel
par(mfrow=c(2,2))
plot(multi.lin.reg)
#These residual plots suggest that most data falls within the predicted
#but there are a few large outliers, tho not many
#the leverage graph shows that there is one predictor that has an exceptional amount of leverage
#by looking at coefficients we can say that this predictor is most likely OverallQual



#multiple regression (some variables multiplied with : function)
multi.lin.reg2 = lm(SalePrice~OverallQual:GarageCars+MasVnrArea:TotRmsAbvGrd+
                      LotFrontage+LotArea+OverallCond+MSSubClass+
                      YearBuilt+YearRemodAdd+
                      BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+
                      X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+
                      BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+
                      KitchenAbvGr+Fireplaces+GarageYrBlt+
                      GarageArea+WoodDeckSF+OpenPorchSF+
                      EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
                      MiscVal+MoSold+YrSold, data=Ames)
summary(multi.lin.reg2)
#here we can use the : function to see  if there is some kind of joint effect on SalePrice
#OverallQual and GarageCars seem to have a pretty strong joint relationship





#multiple regression (some variables transformed with log(), sqrt(), I(), ^2)
multi.lin.reg3 = lm(SalePrice~log(OverallQual)+sqrt(GarageCars)+I(MasVnrArea^2)+TotRmsAbvGrd+
                      LotFrontage+LotArea+OverallCond+MSSubClass+
                      YearBuilt+YearRemodAdd+
                      BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+
                      X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+
                      BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+
                      KitchenAbvGr+Fireplaces+GarageYrBlt+
                      GarageArea+WoodDeckSF+OpenPorchSF+
                      EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
                      MiscVal+MoSold+YrSold, data=Ames)

summary(multi.lin.reg3)

#here we can see that log of OVerallQual might give us a more accurate response
#these tranformations can apply to regression




      











