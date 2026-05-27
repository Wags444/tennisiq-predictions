# =============================================================================
# R/00_config.R  —  tennisIQ central configuration
# =============================================================================
# ALL tunable constants live here. No magic numbers anywhere else.
# Set RAPIDAPI_KEY in your .Renviron:  RAPIDAPI_KEY=your_key_here
# =============================================================================

CONFIG <- list(
  
  # ── API credentials ─────────────────────────────────────────────────────────
  api_key      = Sys.getenv("RAPIDAPI_KEY"),
  api_host     = "tennis-api-atp-wta-itf.p.rapidapi.com",
  api_base     = "https://tennis-api-atp-wta-itf.p.rapidapi.com/tennis/v2",
  api_delay    = 0.65,          # seconds between calls (stay under 100 req/min)
  api_retries  = 3,             # retry attempts on 429 / 5xx
  api_retry_wait = 10,          # seconds to wait before retry
  
  # ── Date window ─────────────────────────────────────────────────────────────
  # We pull 5 years of history — enough for robust surface splits without
  # over-representing a player's style from years ago.
  lookback_years       = 5,
  min_matches_player   = 20,    # discard player-surface cells with fewer obs
  min_matches_h2h      = 3,     # minimum H2H matches before H2H features fire
  
  # ── Tour ────────────────────────────────────────────────────────────────────
  tours = c("atp", "wta"),      # process both; model trains per-tour
  
  # ── Recency decay ───────────────────────────────────────────────────────────
  # Each match gets a weight based on how long ago it was played.
  # Breakpoints in days; weights are the multiplier for that band.
  recency_breaks  = c(0, 30, 90, 180, 365, Inf),
  recency_weights = c(1.00, 0.80, 0.60, 0.40, 0.20),
  
  # ── Surface crossover weights ────────────────────────────────────────────────
  # When computing a player's "hard court profile", how much do we discount
  # their stats from other surfaces?
  # Rows = prediction surface, cols = stat source surface.
  surface_crossover = list(
    hard  = c(hard = 1.00, ihard = 0.90, grass = 0.40, clay = 0.25),
    clay  = c(clay = 1.00, grass = 0.20, hard  = 0.30, ihard = 0.25),
    grass = c(grass = 1.00, ihard = 0.70, hard  = 0.45, clay = 0.15),
    ihard = c(ihard = 1.00, hard  = 0.85, grass = 0.35, clay = 0.20)
  ),
  
  # ── Opponent quality tier weights ────────────────────────────────────────────
  # A hold % against a top-10 player is more informative than vs rank 150.
  # Applied multiplicatively to each match before averaging.
  opp_tier_breaks   = c(0, 10, 25, 50, 100, Inf),
  opp_tier_weights  = c(1.50, 1.25, 1.00, 0.75, 0.50),
  
  # ── Tournament tier weights ──────────────────────────────────────────────────
  # Grand Slam results weighted higher than 250-level.
  # tourRankId values from the Matchstat API /misc/ranking endpoint.
  tier_weights = list(
    "1" = 1.40,   # Grand Slam
    "2" = 1.20,   # Masters 1000
    "3" = 1.00,   # ATP 500
    "4" = 0.85,   # ATP 250
    "5" = 0.65,   # Challenger
    "6" = 0.45    # ITF / other
  ),
  
  # ── Feature set ─────────────────────────────────────────────────────────────
  # These are the raw features computed in 02_features.R.
  # VIF pruning in 03_model.R will drop correlated ones automatically.
  serve_features = c(
    "hold_pct",             # service games held / service games played
    "first_serve_in_pct",   # 1st serve in %  (from firstServePercentage)
    "first_serve_won_pct",  # points won on 1st serve
    "second_serve_won_pct", # points won on 2nd serve
    "ace_rate",             # aces per service game
    "df_rate",              # double faults per service game (negative signal)
    "bp_saved_pct"          # break points saved %
  ),
  return_features = c(
    "break_pct",            # return games broken / return games played
    "return_pts_won_pct",   # all return points won %
    "second_return_won_pct",# points won vs opponent 2nd serve
    "bp_converted_pct"      # break points converted %
  ),
  match_ctrl_features = c(
    "total_pts_won_pct",    # total points won % (dominance measure)
    "tiebreak_win_pct",     # tiebreaks won / played
    "deciding_set_win_pct", # deciding set win %
    "straight_set_win_pct"  # wins in straight sets / total wins
  ),
  
  # ── Bayesian prior weights ───────────────────────────────────────────────────
  # Posterior = (prior_weight * career_avg) + (recent_weight * recent_form)
  prior_weight  = 0.30,
  recent_weight = 0.70,
  
  # ── Model hyperparameters (XGBoost) ─────────────────────────────────────────
  xgb_params = list(
    objective        = "binary:logistic",
    eval_metric      = "logloss",
    eta              = 0.05,
    max_depth        = 4,
    subsample        = 0.80,
    colsample_bytree = 0.80,
    min_child_weight = 5,
    gamma            = 0.1
  ),
  xgb_nrounds      = 500,
  xgb_early_stop   = 30,
  xgb_cv_folds     = 5,
  
  # ── VIF pruning threshold ────────────────────────────────────────────────────
  vif_threshold = 5.0,        # drop features with VIF above this
  
  # ── Calibration ─────────────────────────────────────────────────────────────
  calibration_method = "isotonic",   # "isotonic" | "platt"
  calibration_bins   = 10,           # reliability diagram bins
  
  # ── Backtesting ─────────────────────────────────────────────────────────────
  backtest_window_months = 3,   # rolling window size
  backtest_min_year      = 2021,# don't test on data older than this
  clv_bookmaker_margin   = 0.05 # assumed vig when computing closing line value
)