library(plumber)
r <- plumb("model.R")
r$run(port=8000)
# http://localhost:8000/predict?student=Yes&balance=1500&income=40
# curl "http://localhost:8000/predict?student=Yes&balance=1500&income=40"