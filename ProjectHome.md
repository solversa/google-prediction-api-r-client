# R client library for the Google Prediction API #

Google develops this R package, googlepredictionapi, for [R](http://www.r-project.org/) users to easily access the [Google Prediction API](http://code.google.com/apis/predict).

## Install ##
  1. Request access to the [Google Prediction API](http://code.google.com/apis/predict) (Click the Request Access link on the top right corner)
  1. Install prerequisite R packages: rjson, RCurl
  1. Install [gsutil](http://code.google.com/apis/storage/docs/gsutil.html#install) for accessing data in Google Storage
  1. Click [here](https://code.google.com/p/google-prediction-api-r-client/downloads/list) to download latest googlepredictionapi package
  1. Under R prompt,
```
> install.packages("googlepredictionapi_0.1.tar.gz", repos=NULL, type="source")
```

## Usage ##

```
## Load googlepredictionapi and dependent libraries
library(rjson)
library(RCurl)
library(googlepredictionapi)

## Make a training call to the Prediction API against data in the Google Storage.
## Replace MYBUCKET and MYDATA with your data.
my.model <- PredictionApiTrain(data="gs://MYBUCKET/MYDATA")

## Alternatively, make a training call against training data stored locally as a CSV file.
## Replace MYPATH and MYFILE with your local training file, and MYBUCKET with your own Google Storage buckets.
my.model <- PredictionApiTrain(data="MYPATH/MYFILE.csv", remote.file="gs://MYBUCKET/MYOBJECT")

## Read the summary of the trained model
summary(my.model)

## Make a prediction call for text data using the trained model
predict(my.model, "This is a new piece of text")

## Similarly, predict() works for numeric features
predict(my.model, c(6, 3, 5, 2))
```