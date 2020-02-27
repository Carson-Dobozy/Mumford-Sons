library(tidyverse)
library(caTools)
library(caret)
library(class)
library(e1071)
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ","
                       )
ameslist = ameslist %>%
  select_if(~ !any(is.na(.)))
ameslist.subset <- ameslist[c('GarageArea','GarageCars','TotRmsAbvGrd','GrLivArea',
                      'FullBath','X1stFlrSF','TotalBsmtSF','LotArea','YearBuilt','OverallCond', 'OverallQual', 'SalePrice',
                      'OpenPorchSF', 'WoodDeckSF')]
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))}
ameslist <- as.data.frame(lapply(ameslist, normalize))

smp_size <- floor(0.5 * nrow(ameslist))
set.seed(100)
train_ind <- sample(seq_len(nrow(ameslist)), size = smp_size)

trainames <- ameslist.subset[train_ind, ]
testames <- ameslist.subset[-train_ind, ]
train.ames_labels <- ameslist[train_ind,47]
test.ames_labels <-ameslist[-train_ind,47]
ames_target_category <- ameslist[train_ind,47]

trainmodel = knn(train = trainames, test = testames, cl = train.ames_labels, k=27)

Accuracy27 <- 100 * sum(test.ames_labels == trainmodel)/NROW(test.ames_labels)

confusionMatrix(table(trainmodel,test.ames_labels))


p = ggplot(data = testames,
           aes(
             x = YearBuilt,
             y = SalePrice
           ))+
  geom_point(aes(color = ames_target_category))+
  facet_wrap(ames_target_category)
p


