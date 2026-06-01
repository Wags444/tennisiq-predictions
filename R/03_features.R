# R/03_features.R — Feature engineering
# Builds weighted player profiles then pairwise matchup delta matrix

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(lubridate)
})

build_feature_matrix <- function(raw, cfg, elo_hard=NULL) {
  message("[features] Computing weighted player profiles...")
  profiles <- build_player_profiles(raw$matches, cfg, elo_hard=elo_hard)
  message(sprintf("[features] Built profiles for %d player-surface combos",
                  nrow(profiles)))
  message("[features] Building matchup pairs...")
  matchups <- build_matchups(raw$matches, profiles, cfg)
  message(sprintf("[features] %d matchup rows x %d features",
                  nrow(matchups), ncol(matchups)))
  if (!dir.exists("data/raw")) dir.create("data/raw", recursive=TRUE)
  saveRDS(profiles, "data/raw/profiles.rds")
  saveRDS(matchups, "data/raw/matchups.rds")
  list(profiles=profiles, matchups=matchups)
}

# ── Recency weight ───────────────────────────────────────────────────────────
recency_weight <- function(days_ago, breaks, weights) {
  idx <- findInterval(days_ago, breaks, rightmost.closed=TRUE)
  idx <- pmax(1L, pmin(idx, length(weights)))
  weights[idx]
}

# ── Opponent quality weight ───────────────────────────────────────────────────
opp_quality_weight <- function(tour_rank, cfg) {
  # tour_rank: 1=GS, 2=Masters, 3=500, 4=250, 5=Challenger, 0=ITF
  w <- dplyr::case_when(
    tour_rank == 1 ~ 1.40,
    tour_rank == 2 ~ 1.20,
    tour_rank == 3 ~ 1.00,
    tour_rank == 4 ~ 0.85,
    tour_rank == 5 ~ 0.65,
    tour_rank == 0 ~ 0.40,
    TRUE           ~ 0.50
  )
  w
}

# ── Surface crossover weight ──────────────────────────────────────────────────
surface_weight <- function(match_surface, target_surface) {
  crossover <- list(
    hard  = c(hard=1.00, ihard=0.85, grass=0.40, clay=0.25),
    clay  = c(clay=1.00, grass=0.20, hard=0.30,  ihard=0.25),
    grass = c(grass=1.00, ihard=0.70, hard=0.45, clay=0.15),
    ihard = c(ihard=1.00, hard=0.85,  grass=0.35, clay=0.20)
  )
  cw <- crossover[[target_surface]]
  if (is.null(cw)) return(0)
  w  <- cw[match_surface]
  ifelse(is.na(w), 0, w)
}

# ── Build weighted profile per player x surface ───────────────────────────────
build_player_profiles <- function(matches, cfg, elo_hard=NULL) {
  matches <- matches |>
    dplyr::filter(!is.na(date), !is.na(surface), !is.na(player_id)) |>
    dplyr::left_join(if(!is.null(elo_hard)) elo_hard %>% dplyr::select(match_id, player_id, elo_b=elo_hard_b) else data.frame(match_id=integer(),player_id=integer(),elo_b=numeric()), by=c("match_id","player_id")) |>
    dplyr::mutate(
      days_ago  = as.numeric(Sys.Date() - date),
      rec_w     = recency_weight(days_ago,
                                  cfg$recency_breaks,
                                  cfg$recency_weights),
      tour_w    = opp_quality_weight(tour_rank, cfg)
    )

  stat_cols <- c("first_serve_in_pct","first_serve_won_pct",
                 "second_serve_won_pct","ace_rate","df_rate",
                 "bp_conv_pct","total_pts_won","won")

  surfaces <- c("hard","clay","grass","ihard")

  map_dfr(surfaces, function(tgt) {
    matches |>
      dplyr::mutate(surf_w = surface_weight(surface, tgt)) |>
      dplyr::filter(surf_w > 0) |>
      dplyr::mutate(final_w = rec_w * tour_w * surf_w) |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        target_surface       = tgt,
        n_matches            = dplyr::n(),
        n_matches_surface    = sum(surface == tgt, na.rm=TRUE),
        first_serve_in_pct   = weighted.mean(first_serve_in_pct,  final_w, na.rm=TRUE),
        first_serve_won_pct  = weighted.mean(first_serve_won_pct, final_w, na.rm=TRUE),
        second_serve_won_pct = weighted.mean(second_serve_won_pct,final_w, na.rm=TRUE),
        ace_rate             = weighted.mean(ace_rate,             final_w, na.rm=TRUE),
        df_rate              = weighted.mean(df_rate,              final_w, na.rm=TRUE),
        bp_conv_pct          = weighted.mean(bp_conv_pct,          final_w, na.rm=TRUE),
        total_pts_won_avg    = weighted.mean(total_pts_won,        final_w, na.rm=TRUE),
        win_rate             = weighted.mean(won,                  final_w, na.rm=TRUE),
        win_big          = if(sum(!is.na(tour_rank) & tour_rank>=3)>=3) mean(won[!is.na(tour_rank)&tour_rank>=3], na.rm=TRUE) else 0.5,
        win_vs_strong    = if(sum(!is.na(elo_b) & elo_b>1600)>=3) mean(won[!is.na(elo_b)&elo_b>1600], na.rm=TRUE) else 0.5,
        win_recent       = local({ r90=won[!is.na(days_ago)&days_ago<=90]; r180=won[!is.na(days_ago)&days_ago>90&days_ago<=180]; n90=length(r90); n180=length(r180); if(n90+n180==0) weighted.mean(won,final_w,na.rm=TRUE) else (mean(r90)*2*n90+mean(r180)*n180)/(2*n90+n180) }),
        win_rate_decay   = local({ r90=won[!is.na(days_ago)&days_ago<=90]; r180=won[!is.na(days_ago)&days_ago>90&days_ago<=180]; rold=won[!is.na(days_ago)&days_ago>180]; n90=length(r90); n180=length(r180); nold=length(rold); tot=4*n90+2*n180+nold; if(tot==0) weighted.mean(won,final_w,na.rm=TRUE) else (mean(c(r90,r90,r90,r90))%||%0.5*4*n90 + mean(c(r180,r180))%||%0.5*2*n180 + mean(rold)%||%0.5*nold)/tot }),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # Bayesian shrinkage toward tour mean — prevents small sample extremes
        shrink = pmin(n_matches / (n_matches + 20), 1)
      )
  })
}

# ── Build pairwise matchup delta matrix ───────────────────────────────────────
build_matchups <- function(matches, profiles, cfg) {
  # Use historical matches as training rows
  # For each match: player_a = player_id, player_b = opponent
  # Features = player_a_profile - player_b_profile on match surface
  # Label = won (1/0)

  feat_cols <- c("first_serve_in_pct","first_serve_won_pct",
                 "second_serve_won_pct","ace_rate","df_rate",
                 "bp_conv_pct","total_pts_won_avg","win_rate",
                 "win_big","win_vs_strong","win_recent","win_rate_decay")

  matches_clean <- matches |>
    dplyr::filter(!is.na(surface), !is.na(won), !is.na(opponent_id))

  # Join profiles for player_a on match surface
  prof_a <- profiles |>
    dplyr::rename_with(~ paste0("a_", .x),
                       dplyr::all_of(feat_cols)) |>
    dplyr::rename_with(~ paste0("a_n_", gsub("n_","",.x)),
                       dplyr::starts_with("n_"))

  prof_b <- profiles |>
    dplyr::rename_with(~ paste0("b_", .x),
                       dplyr::all_of(feat_cols)) |>
    dplyr::rename_with(~ paste0("b_n_", gsub("n_","",.x)),
                       dplyr::starts_with("n_"))

  matchups <- matches_clean |>
    dplyr::left_join(
      prof_a |> dplyr::select(player_id, target_surface,
                               shrink, dplyr::starts_with("a_")),
      by = c("player_id", "surface" = "target_surface")
    ) |>
    dplyr::left_join(
      prof_b |> dplyr::select(player_id, target_surface,
                               shrink, dplyr::starts_with("b_")),
      by = c("opponent_id" = "player_id", "surface" = "target_surface"),
      suffix = c("", "_b")
    )

  # Compute delta features (a - b)
  # Add ranking differential (requires atp_rank_lookup in environment)
  if(exists("atp_rank_lookup")) {
    matchups$a_ranking <- as.numeric(atp_rank_lookup[as.character(matchups$player_id)])
    matchups$b_ranking <- as.numeric(atp_rank_lookup[as.character(matchups$opponent_id)])
    matchups$a_ranking[is.na(matchups$a_ranking)] <- 500  # unranked = 500
    matchups$b_ranking[is.na(matchups$b_ranking)] <- 500
    matchups$d_ranking <- matchups$a_ranking - matchups$b_ranking
  }

  for (f in feat_cols) {
    matchups[[paste0("d_", f)]] <- matchups[[paste0("a_", f)]] -
                                    matchups[[paste0("b_", f)]]
  }

  # Surface dummies
  matchups <- matchups |>
    dplyr::mutate(
      surf_hard  = as.integer(surface == "hard"),
      surf_clay  = as.integer(surface == "clay"),
      surf_grass = as.integer(surface == "grass"),
      surf_ihard = as.integer(surface == "ihard"),
      days_ago   = as.numeric(Sys.Date() - date)
    )

  # Keep only what the model needs
  keep_cols <- c("match_id","date","surface","player_id","opponent_id",
                 "tournament_id","tour_rank","won","days_ago",
                 paste0("d_", feat_cols),
                 "surf_hard","surf_clay","surf_grass","surf_ihard",
                 "a_n_matches","b_n_matches")
  keep_cols <- intersect(keep_cols, names(matchups))
  matchups[, keep_cols]
}
