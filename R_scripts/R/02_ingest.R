# R/02_ingest.R — Pull and assemble match data from Matchstat API
# Strategy: pull fixtures to get current player IDs, then pull
# past matches + stats for each player. Cache aggressively.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(lubridate)
})

if (!exists(".cn")) source("R/01_api_client.R")

ingest_all <- function(cfg, tours = cfg$tours, force = FALSE) {
  raw_path <- "data/raw/raw_data.rds"
  if (!force && file.exists(raw_path)) {
    message("[ingest] Loading cached data from ", raw_path)
    return(readRDS(raw_path))
  }
  if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)
  years <- seq(as.integer(format(Sys.Date(), "%Y")) - cfg$lookback_years,
               as.integer(format(Sys.Date(), "%Y")))
  all_results <- map(tours, function(tour) {
    message(sprintf("
[ingest] Tour: %s", toupper(tour)))
    ingest_tour(tour, years, cfg)
  })
  raw <- list(
    matches  = bind_rows(map(all_results, "matches")),
    players  = bind_rows(map(all_results, "players")),
    rankings = bind_rows(map(all_results, "rankings"))
  )
  message(sprintf("
[ingest] Done. %d match rows | %d players",
                  nrow(raw$matches), nrow(raw$players)))
  saveRDS(raw, raw_path)
  raw
}

ingest_tour <- function(tour, years, cfg) {
  # Step 1: Get player IDs from fixtures (current active players)
  message("  Fetching player IDs from fixtures...")
  fix <- api_get_fixtures(tour, cfg, pages = 10)
  if (nrow(fix) == 0) {
    message("  No fixtures returned — cannot get player IDs")
    return(list(matches = data.frame(), players = data.frame(),
                rankings = data.frame()))
  }
  player_ids <- unique(c(fix$player1Id, fix$player2Id))
  player_ids <- player_ids[!is.na(player_ids)]
  message(sprintf("  Found %d unique player IDs from fixtures", length(player_ids)))

  # Step 2: Pull profile + match history for each player
  message("  Pulling profiles and match history...")
  player_data <- map(player_ids, function(pid) {
    Sys.sleep(cfg$api_delay)
    prof_raw <- tryCatch(api_get_player_profile(pid, tour, cfg),
                         error = function(e) NULL)
    profile  <- parse_profile(prof_raw, pid, tour)
    Sys.sleep(cfg$api_delay)
    match_raw <- tryCatch(api_get_player_matches(pid, tour, cfg, years),
                          error = function(e) data.frame())
    matches <- if (nrow(match_raw) > 0) {
      m <- tryCatch(extract_match_stats(match_raw, pid),
                    error = function(e) data.frame())
      if (nrow(m) > 0) mutate(m, player_id = pid, tour = tour) else data.frame()
    } else data.frame()
    list(profile = profile, matches = matches)
  }, .progress = TRUE)

  players <- bind_rows(map(player_data, "profile"))
  matches <- bind_rows(map(player_data, "matches"))

  # Deduplicate — same match appears from both players perspective
  if (nrow(matches) > 0) {
    matches <- matches |>
      mutate(dedup_key = paste(pmin(player_id, opponent_id),
                               pmax(player_id, opponent_id),
                               match_id, sep = "_")) |>
      distinct(dedup_key, player_id, .keep_all = TRUE) |>
      select(-dedup_key)
  }

  # Rankings approximated from fixture seed data
  rankings <- fix |>
    select(player_id = player1Id, seed = seed1) |>
    bind_rows(fix |> select(player_id = player2Id, seed = seed2)) |>
    filter(!is.na(player_id)) |>
    distinct(player_id, .keep_all = TRUE) |>
    mutate(tour = tour)

  list(matches = matches, players = players, rankings = rankings)
}


parse_profile <- function(raw, player_id, tour) {
  empty <- data.frame(player_id=player_id, tour=tour, name=NA_character_,
                      country=NA_character_, current_rank=NA_integer_,
                      handedness=NA_character_, stringsAsFactors=FALSE)
  if (is.null(raw)) return(empty)
  rank_val <- tryCatch({
    r <- raw$curRank$rank
    if (is.null(r) || length(r) == 0) NA_integer_ else as.integer(r[[1]])
  }, error = function(e) NA_integer_)
  plays <- tryCatch({
    p <- raw$information$plays
    if (is.null(p) || length(p) == 0) NA_character_ else as.character(p[[1]])
  }, error = function(e) NA_character_)
  handed <- if (!is.null(plays) && length(plays)==1 &&
                !is.na(plays) && grepl("Left", plays, ignore.case=TRUE)) "left" else "right"
  data.frame(player_id=player_id, tour=tour,
             name=.cn(raw$name, NA_character_),
             country=.cn(raw$countryAcr, NA_character_),
             current_rank=rank_val, handedness=handed,
             stringsAsFactors=FALSE)
}
