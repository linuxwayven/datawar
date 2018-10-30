###########################################################################
#
# Stage2_Output_JRUIZ_1
#
# Description:
# Script to apply prediction model to PROPENSION dataset
# 
# Author: Jesus A. Ruiz (linuxwayven@gmail.com)
#
# Stage2_Output_JRUIZ_1.r
#
# Version 1.0
#
# CHANGES:
# V1.0 - JRUIZ - 28/10/2018 - In the beginning everything was darkness.
#
###########################################################################

# Environment  Init and cleaning
source("init.R")

# Load Libraries
library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(rattle)
library(iplots)

# 1) Load DataSet, we using this function to read csv
OriginalDataSet <- read.csv("/Users/linuxwayven/Google Drive/data_war/track_propension/etapa2/datascience_r/files/data/input/DfEtapa2.csv",header=T,sep=",")

# 2) Split DataSet 
# Create Testing Data spliting DataSet in two portions (70% Original Data / 30% Testing Data)

# 70% of the sample size
smp_size <- floor(0.70 * nrow(OriginalDataSet))

# Set the seed
set.seed(123)
train_ind <- sample(seq_len(nrow(OriginalDataSet)), size = smp_size)

# Split
trainDataSet <- OriginalDataSet[train_ind, ]
testDataSet <- OriginalDataSet[-train_ind, ]

# 3) Save Data
# Save new DataSets
saveRDS(trainDataSet, "/Users/linuxwayven/Google Drive/data_war/track_propension/etapa2/datascience_r/trainDataSet.rds")
saveRDS(testDataSet, "/Users/linuxwayven/Google Drive/data_war/track_propension/etapa2/datascience_r/testDataSet.rds")

fit_arbol = rpart(class ~ ., trainDataSet)
fancyRpartPlot(fit_arbol)
table(predict(fit_arbol,trainDataSet) > 0.5,trainDataSet$class)/nrow(trainDataSet)*100


# 4) Tree Model Configuration
# See Column Names
colnames(trainDataSet)

# Create a data subset
trainDataSet <- subset( trainDataSet , select = c(FL_INACTIVO,QT_SCND_IN_6,QT_SCND_IN_5,QT_SCND_IN_4,QT_SCND_IN_3,QT_CALL_IN_6,QT_CALL_IN_5,QT_CALL_IN_4,QT_CALL_IN_3,QT_SCND_OUT_6,QT_SCND_OUT_5,QT_SCND_OUT_4,QT_SCND_OUT_3,QT_CALL_OUT_6,QT_CALL_OUT_5,QT_CALL_OUT_4,QT_CALL_OUT_3,QT_SMS_IN_6,QT_SMS_IN_5,QT_SMS_IN_4,QT_SMS_IN_3,
                                           QT_SMS_OUT_6,QT_SMS_OUT_5,QT_SMS_OUT_4,QT_SMS_OUT_3,QT_TRF_BYTE_6,QT_TRF_BYTE_5,QT_TRF_BYTE_4,QT_TRF_BYTE_3) )

# Check subset structure
colnames(trainDataSet)



# Train Model
fit = rpart(trainDataSet$FL_INACTIVO ~ ., trainDataSet)

# Run Prediction
prediccion = predict(fit,trainDataSet)
prediccion = apply(prediccion,1,which.max)

table(prediccion,trainDataSet$FL_INACTIVO)
plot(fit)
text(fit,all=T,cex=.7)

fancyRpartPlot(fit)

# The End.
