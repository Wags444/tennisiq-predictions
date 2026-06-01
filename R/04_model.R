# R/04_model.R — XGBoost training
suppressPackageStartupMessages({
  library(xgboost)
  library(dplyr)
})

train_model <- function(features, cfg) {
  matchups  <- features$matchups
  delta_cols <- grep("^d_", names(matchups), value=TRUE)
  surf_cols  <- c("surf_hard","surf_clay","surf_grass","surf_ihard")
  feat_cols  <- c(delta_cols, surf_cols)

  # Time-aware split — train on older, validate on recent
  complete   <- matchups[complete.cases(matchups[, feat_cols]) &
                         !is.na(matchups$won), ]
  complete   <- complete[order(complete$date), ]
  n          <- nrow(complete)
  cut        <- floor(n * 0.80)
  train_data <- complete[1:cut, ]
  val_data   <- complete[(cut+1):n, ]

  message(sprintf("  Train rows: %d | Val rows: %d", nrow(train_data), nrow(val_data)))
  message(sprintf("  Val date range: %s to %s",
                  min(val_data$date), max(val_data$date)))

  X_train <- as.matrix(train_data[, feat_cols])
  X_val   <- as.matrix(val_data[,   feat_cols])
  y_train <- as.numeric(train_data$won)
  y_val   <- as.numeric(val_data$won)

  dtrain  <- xgb.DMatrix(X_train, label=y_train)
  dval    <- xgb.DMatrix(X_val,   label=y_val)

  # Cross-validated round selection
  message("  Running CV to find optimal rounds...")
  cv <- xgb.cv(
    params   = cfg$xgb_params,
    data     = dtrain,
    nrounds  = cfg$xgb_nrounds,
    nfold    = cfg$xgb_cv_folds,
    early_stopping_rounds = cfg$xgb_early_stop,
    verbose  = 0
  )
  best_rounds <- cv$best_iteration
  message(sprintf("  Best rounds: %d", best_rounds))

  # Train final model
  model <- xgb.train(
    params    = cfg$xgb_params,
    data      = dtrain,
    nrounds   = best_rounds,
    watchlist = list(train=dtrain, val=dval),
    verbose   = 0
  )

  # Validation metrics
  preds    <- predict(model, dval)
  brier    <- mean((preds - y_val)^2)
  logloss  <- -mean(y_val*log(preds+1e-9) + (1-y_val)*log(1-preds+1e-9))
  accuracy <- mean((preds > 0.5) == y_val)

  message("  Validation results:")
  message(sprintf("    Accuracy:  %.3f", accuracy))
  message(sprintf("    Brier:     %.4f  (naive baseline: 0.25)", brier))
  message(sprintf("    Log-loss:  %.4f", logloss))

  # Feature importance
  imp <- xgb.importance(feature_names=feat_cols, model=model)
  message("
  Top features by gain:")
  print(head(imp[, c("Feature","Gain")], 8))

  # Save
  if (!dir.exists("output")) dir.create("output", recursive=TRUE)
  bundle <- list(model=model, feat_cols=feat_cols, best_rounds=best_rounds,
                 val_metrics=list(accuracy=accuracy, brier=brier, logloss=logloss),
                 trained_at=Sys.time())
  saveRDS(bundle, "output/model.rds")
  message("  Model saved to output/model.rds")
  bundle
}
