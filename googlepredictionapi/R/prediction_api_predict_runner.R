# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiPredictRunner <- function(model,
                                       newdata,
                                       verbose = FALSE) {
  # Predicts the label of newdata (one instance) using model
  # on Google Prediction API
  #
  # Args:
  #   model: the model object returned from PredictionApiTrainRunner(),
  #          and its class type must be "prediction.api.model"
  #   newdata: one instance of data for prediction, its type should be a list,
  #            remember to remove the response at first column
  #            if its class type is character, use "text" format
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if succeed, return the predicted label
  #   else, return the error report including
  #     1. result: "error"
  #     2. error.code
  #     3. error.message
  #     4. error.information

  bucket.name <- model$bucket.name
  object.name <- model$object.name

  # detect newdata type: "numeric" or "text"
  if (is.character(newdata[[1L]])) {
    if (verbose)
      cat("'newdata' detected as text\n")
    predict.type <- "text"
  } else if (is.numeric(newdata[[1L]])) {
    if (verbose)
      cat("'newdata' detected as numeric\n")
    predict.type <- "numeric"
  } else {
    stop("The class type of 'newdata' should be either character or numeric")
  }
  # number of row should be no more than 1
  row <- nrow(newdata)
  if (!is.null(row) && row > 1) {
    stop("'newdata' should only have one row: ", newdata)
  }
  # translate newdata into json string
  # make json object
  # if only one string: need to cast to a list
  if (length(newdata) == 1)
    newdata <- list(newdata)

  if (predict.type == "numeric") {
    tempx <- list(data = list(input = list(numeric = as.numeric(newdata))))
  } else if (predict.type == "text") {
    tempx <- list(data = list(input = list(text = newdata)))
  }
  if (verbose) {
    cat("data to send:\n")
    print(tempx)
  }
  # get json string data by sending newdata to jsonParser
  data.tosend <- PredictionApiDataToJson(tempx, verbose = verbose)

  # connect to Prediction API and get result
  result.conn <- PredictionApiConnectHandler(connect.type = "predict",
                                             bucket.name = bucket.name,
                                             object.name = object.name,
                                             data.tosend = data.tosend,
                                             verbose = verbose)

  # translate the json result to R object
  result <- PredictionApiJsonToData(result.conn$data, verbose)

  # handle json from prediction API
  output <- PredictionApiResultHandler(result.data  = result,
                                       mode         = "predict",
                                       verbose      = verbose)

  # handle result
  if (!is.atomic(output) && output$result == "error") {
    cat("Error: ", output$error.message,
        "\ncheck return value for more information\n")
  }
  return(output)
}
