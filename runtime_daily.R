suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr)
  library(purrr); library(lubridate); library(zoo); library(glmnet)
})
cat("[1/5] Packages loaded\n")
source("R/00_config.R")
source("R/01_api_client.R")

# Load runtime bundle
rb <- readRDS("output/runtime_bundle.rds")
bundle<-rb$atp_bundle; wta_bundle<-rb$wta_bundle; features2<-rb$features2
elo_hard<-rb$elo_hard; elo_clay<-rb$elo_clay; elo_grass<-rb$elo_grass; elo_ihard<-rb$elo_ihard
wta_elo<-rb$wta_elo; wta_elo_hard<-rb$wta_elo_hard; wta_elo_clay<-rb$wta_elo_clay
wta_elo_grass<-rb$wta_elo_grass; wta_elo_ihard<-rb$wta_elo_ihard
matches_full<-rb$matches_full; wta_full<-rb$wta_full
names_lookup<-rb$names_lookup; wta_names_lookup<-rb$wta_names_lookup
logit_wta<-rb$logit_wta; all_feats_wta<-rb$all_feats_wta; features_wta<-rb$features_wta
rm(rb); gc()
# Load rankings cache
if(file.exists("output/rankings_cache.rds")) {
  ranks <- readRDS("output/rankings_cache.rds")
  atp_rank_lookup <- setNames(ranks$atp$position, as.character(ranks$atp$player_id))
  wta_rank_lookup <- setNames(ranks$wta$position, as.character(ranks$wta$player_id))
} else { atp_rank_lookup <- list(); wta_rank_lookup <- list() }
# Override predict_match_v2 with v5 features
predict_match_v2 <- function(p1_id, p2_id, surface, model, profiles, melo, mfull) {
  `%||%` <- function(a,b) if(is.null(a)||is.na(a)) b else a
  get_profile <- function(pid) {
    p <- profiles[profiles$player_id==pid & profiles$target_surface==surface,]
    if(nrow(p)==0) p <- profiles[profiles$player_id==pid,]
    if(nrow(p)==0) return(NULL)
    p[which.max(p$n_matches),]
  }
  get_surf_elo <- function(pid, surf_df) {
    rows <- surf_df[surf_df$player_id==pid,]
    if(nrow(rows)==0) return(1500)
    col <- grep("_a$", names(surf_df), value=TRUE)[1]
    rows[[col]][nrow(rows)]
  }
  get_recent <- function(pid) {
    rows <- mfull[mfull$player_id==pid,]
    if(nrow(rows)==0) return(list(fatigue7=0,sets7=0,streak=0))
    rows <- rows[order(rows$date,decreasing=TRUE),]
    streak <- 0
    for(w in rows$won[1:min(10,nrow(rows))]) { if(is.na(w)) break; if(isTRUE(w==1)) streak<-streak+1 else break }
    list(fatigue7=rows$matches_last7[1]%||%0,
         sets7=sum(rows$won[1:min(7,nrow(rows))],na.rm=TRUE),
         streak=streak)
  }
  surf_elo_df <- switch(surface,hard=elo_hard,clay=elo_clay,grass=elo_grass,ihard=elo_ihard,elo_hard)
  prof1 <- get_profile(p1_id); prof2 <- get_profile(p2_id)
  if(is.null(prof1)||is.null(prof2)) return(NULL)
  elo1 <- get_surf_elo(p1_id, surf_elo_df)
  elo2 <- get_surf_elo(p2_id, surf_elo_df)
  elo_diff <- elo1 - elo2
  elo_diff_surf <- elo_diff * switch(surface, clay=1.1, grass=0.9, 1.0)
  rec1 <- get_recent(p1_id); rec2 <- get_recent(p2_id)
  p1h <- sum(mfull$player_id==p1_id&mfull$opponent_id==p2_id&mfull$won==1,na.rm=TRUE)
  p2h <- sum(mfull$player_id==p2_id&mfull$opponent_id==p1_id&mfull$won==1,na.rm=TRUE)
  h2h_pct <- if((p1h+p2h)>=2) p1h/(p1h+p2h) else 0.5
  p1s <- sum(mfull$player_id==p1_id&mfull$opponent_id==p2_id&!is.na(mfull$surface)&mfull$surface==surface&mfull$won==1,na.rm=TRUE)
  p2s <- sum(mfull$player_id==p2_id&mfull$opponent_id==p1_id&!is.na(mfull$surface)&mfull$surface==surface&mfull$won==1,na.rm=TRUE)
  h2h_surf <- if((p1s+p2s)>=2) p1s/(p1s+p2s) else 0.5
  r1 <- as.numeric(atp_rank_lookup[as.character(p1_id)])%||%500
  r2 <- as.numeric(atp_rank_lookup[as.character(p2_id)])%||%500
  nd <- data.frame(
    d_first_serve_in_pct   = (prof1$first_serve_in_pct%||%0)   - (prof2$first_serve_in_pct%||%0),
    d_first_serve_won_pct  = (prof1$first_serve_won_pct%||%0)  - (prof2$first_serve_won_pct%||%0),
    d_second_serve_won_pct = (prof1$second_serve_won_pct%||%0) - (prof2$second_serve_won_pct%||%0),
    d_ace_rate             = (prof1$ace_rate%||%0)             - (prof2$ace_rate%||%0),
    d_bp_conv_pct          = (prof1$bp_conv_pct%||%0)          - (prof2$bp_conv_pct%||%0),
    d_total_pts_won_avg    = (prof1$total_pts_won_avg%||%0)    - (prof2$total_pts_won_avg%||%0),
    elo_diff               = elo_diff,
    d_fatigue_last7        = rec1$fatigue7 - rec2$fatigue7,
    d_sets_last7           = rec1$sets7 - rec2$sets7,
    d_form_streak          = rec1$streak - rec2$streak,
    h2h_surf_win_pct       = h2h_surf,
    d_win_big              = (prof1$win_big%||%0.5)         - (prof2$win_big%||%0.5),
    d_win_vs_strong        = (prof1$win_vs_strong%||%0.5)   - (prof2$win_vs_strong%||%0.5),
    d_win_rate_decay       = (prof1$win_rate_decay%||%0.5)  - (prof2$win_rate_decay%||%0.5),
    d_ranking_norm         = (r1-r2)/500
  )
  p_win <- as.numeric(predict(model, newdata=nd, type="response"))
  p_win <- max(0.01, min(0.99, p_win))
  list(p1_id=p1_id, p2_id=p2_id, surface=surface,
       p1_win=round(p_win,3), p2_win=round(1-p_win,3),
       elo1=round(elo1), elo2=round(elo2), elo_diff=round(elo_diff),
       h2h_record=sprintf("%d-%d",p1h,p2h),
       h2h_surf=sprintf("%d-%d on %s",p1s,p2s,surface),
       p1_streak=rec1$streak, p2_streak=rec2$streak,
       p1_fatigue=rec1$fatigue7, p2_fatigue=rec2$fatigue7)
}
# Load WTA bundle from dedicated RDS file
if (file.exists("output/wta_bundle.rds")) {
  wta_bundle     <- readRDS("output/wta_bundle.rds")
  logit_wta      <- wta_bundle$model
  all_feats_wta  <- wta_bundle$feat_cols
  features_wta   <- list(profiles=wta_bundle$profiles)
  wta_elo        <- wta_bundle$elo
  wta_elo_hard   <- wta_bundle$elo_hard
  wta_elo_clay   <- wta_bundle$elo_clay
  wta_elo_grass  <- wta_bundle$elo_grass
  wta_elo_ihard  <- wta_bundle$elo_ihard
  wta_full       <- wta_bundle$matches_full
  wta_players    <- wta_bundle$players
  cat(sprintf("WTA bundle loaded: %d players\n", nrow(wta_players)))
} else {
  cat("WTA bundle not found — skipping WTA\n")
}

# WTA predict function (glmnet ridge model)
predict_match_wta <- function(p1_id, p2_id, surface) {
  `%||%` <- function(a,b) if(is.null(a)||is.na(a)) b else a
  avg_prof <- wta_bundle$avg_profile
  get_profile <- function(pid) {
    p <- features_wta$profiles[features_wta$profiles$player_id==pid &
                                features_wta$profiles$target_surface==surface,]
    if(nrow(p)==0) p <- features_wta$profiles[features_wta$profiles$player_id==pid,]
    if(nrow(p)==0) return(avg_prof)
    p[which.max(p$n_matches),]
  }
  get_surf_elo <- function(pid, surf_df) {
    rows <- surf_df[surf_df$player_id==pid,]
    if(nrow(rows)==0) return(1500)
    col <- grep("_a$", names(surf_df), value=TRUE)[1]
    rows[[col]][nrow(rows)]
  }
  get_recent <- function(pid) {
    rows <- wta_full[wta_full$player_id==pid,]
    if(nrow(rows)==0) return(list(fatigue7=0,sets7=0,win5=0.5,win5s=0.5,streak=0))
    rows <- rows[order(rows$date,decreasing=TRUE),]
    last5 <- rows[1:min(5,nrow(rows)),]
    surf_rows <- rows[!is.na(rows$surface)&rows$surface==surface,]
    last5s <- surf_rows[1:min(5,nrow(surf_rows)),]
    streak <- 0
    for(w in rows$won[1:min(10,nrow(rows))]) {
      if(is.na(w)) break
      if(isTRUE(w==1)) streak<-streak+1 else break
    }
    list(fatigue7=rows$matches_last7[1]%||%0,
         sets7=sum(rows$won[1:min(7,nrow(rows))],na.rm=TRUE),
         win5=if(nrow(last5)>0) mean(last5$won,na.rm=TRUE) else 0.5,
         win5s=if(nrow(last5s)>0) mean(last5s$won,na.rm=TRUE) else 0.5,
         streak=streak)
  }
  surf_elo_df <- switch(surface,hard=wta_elo_hard,clay=wta_elo_clay,
                        grass=wta_elo_grass,ihard=wta_elo_ihard,wta_elo_hard)
  prof1 <- get_profile(p1_id); prof2 <- get_profile(p2_id)
  elo1 <- get_surf_elo(p1_id,surf_elo_df); elo2 <- get_surf_elo(p2_id,surf_elo_df)
  elo_diff <- elo1-elo2
  elo_diff_surf <- elo_diff * switch(surface,clay=1.1,grass=0.9,1.0)
  rec1 <- get_recent(p1_id); rec2 <- get_recent(p2_id)
  p1h <- sum(wta_full$player_id==p1_id&wta_full$opponent_id==p2_id&wta_full$won==1,na.rm=TRUE)
  p2h <- sum(wta_full$player_id==p2_id&wta_full$opponent_id==p1_id&wta_full$won==1,na.rm=TRUE)
  h2h_pct <- if((p1h+p2h)>=2) p1h/(p1h+p2h) else 0.5
  p1s <- sum(wta_full$player_id==p1_id&wta_full$opponent_id==p2_id&
             !is.na(wta_full$surface)&wta_full$surface==surface&wta_full$won==1,na.rm=TRUE)
  p2s <- sum(wta_full$player_id==p2_id&wta_full$opponent_id==p1_id&
             !is.na(wta_full$surface)&wta_full$surface==surface&wta_full$won==1,na.rm=TRUE)
  h2h_surf <- if((p1s+p2s)>=2) p1s/(p1s+p2s) else 0.5
  r1 <- as.numeric(wta_rank_lookup[as.character(p1_id)])%||%500
  r2 <- as.numeric(wta_rank_lookup[as.character(p2_id)])%||%500
  # Use new glm model if available, else fall back to glmnet
  if(!is.null(wta_bundle$model_glm)) {
    nd <- matrix(c(
      (prof1$first_serve_in_pct%||%0)   - (prof2$first_serve_in_pct%||%0),
      (prof1$first_serve_won_pct%||%0)  - (prof2$first_serve_won_pct%||%0),
      (prof1$second_serve_won_pct%||%0) - (prof2$second_serve_won_pct%||%0),
      (prof1$ace_rate%||%0)             - (prof2$ace_rate%||%0),
      (prof1$bp_conv_pct%||%0)          - (prof2$bp_conv_pct%||%0),
      (prof1$total_pts_won_avg%||%0)    - (prof2$total_pts_won_avg%||%0),
      elo_diff,
      rec1$fatigue7 - rec2$fatigue7,
      rec1$streak - rec2$streak,
      h2h_surf,
      (prof1$win_big%||%0.5)        - (prof2$win_big%||%0.5),
      (prof1$win_vs_strong%||%0.5)  - (prof2$win_vs_strong%||%0.5),
      (prof1$win_rate_decay%||%0.5) - (prof2$win_rate_decay%||%0.5),
      (r1-r2)/500
    ), nrow=1, dimnames=list(NULL, wta_bundle$feat_cols_glm))
    p_win <- as.numeric(predict(wta_bundle$model_glm, newx=nd, type="response"))
  } else {
    # Legacy glmnet fallback
    nd <- matrix(c(
      (prof1$first_serve_in_pct%||%0)-(prof2$first_serve_in_pct%||%0),
      (prof1$first_serve_won_pct%||%0)-(prof2$first_serve_won_pct%||%0),
      (prof1$second_serve_won_pct%||%0)-(prof2$second_serve_won_pct%||%0),
      (prof1$ace_rate%||%0)-(prof2$ace_rate%||%0),
      (prof1$bp_conv_pct%||%0)-(prof2$bp_conv_pct%||%0),
      (prof1$win_rate%||%0.5)-(prof2$win_rate%||%0.5),
      elo1-elo2, elo_diff_surf,
      rec1$fatigue7-rec2$fatigue7, rec1$sets7-rec2$sets7,
      rec1$win5-rec2$win5, rec1$win5s-rec2$win5s, rec1$streak-rec2$streak,
      h2h_pct, h2h_surf
    ), nrow=1, dimnames=list(NULL, all_feats_wta))
    p_win <- as.numeric(predict(logit_wta, newx=nd, type="response"))
  }
  p_win <- max(0.01, min(0.99, p_win))
  list(p1_id=p1_id,p2_id=p2_id,surface=surface,
       p1_win=round(p_win,3),p2_win=round(1-p_win,3),
       elo1=round(elo1),elo2=round(elo2),elo_diff=round(elo_diff),
       h2h_record=sprintf("%d-%d",p1h,p2h),
       h2h_surf=sprintf("%d-%d on %s",p1s,p2s,surface),
       p1_streak=rec1$streak,p2_streak=rec2$streak,
       p1_fatigue=rec1$fatigue7,p2_fatigue=rec2$fatigue7)
}
# names_lookup already loaded from runtime bundle
cat(sprintf("[2/5] Lookups: %d ATP + %d WTA players\n",
           length(names_lookup), length(wta_names_lookup)))
if (nchar(CONFIG$api_key) < 10) stop("RAPIDAPI_KEY not set")
cat("[3/5] API key loaded\n")

keep_tournament <- function(nm) {
  if (is.null(nm) || is.na(nm)) return(FALSE)
  if (grepl("M15|M25|W15|W25", nm, ignore.case=FALSE)) return(FALSE)
  grepl("Open|Masters|ATP|WTA|Challenger|M75|M100|M125|W75|W100|W125|Grand Slam|Internazionali|Roland Garros|Wimbledon|Australian|Istanbul|Jiangxi|Santos|Lopota", nm, ignore.case=TRUE)
}

run_predictions <- function(fix, lookup, profiles, model, melo, mfull, tour_label) {
  preds <- list()
  for (i in seq_len(nrow(fix))) {
  n_name<-0;n_prof<-0;n_surf<-0;n_tourn<-0
    p1_id <- fix$player1Id[i]
    p2_id <- fix$player2Id[i]
    n1 <- lookup[[as.character(p1_id)]] %||% ""
    n2 <- lookup[[as.character(p2_id)]] %||% ""
    if (grepl("/",n1)||grepl("/",n2)||n1==""||n2=="") next
  n_name<-n_name+1
    if (!p1_id %in% profiles$player_id) next
    if (!p2_id %in% profiles$player_id) next
    surf <- tryCatch(normalise_surface(fix$tournament$court$name[i]),
                    error=function(e) NA_character_)
    if (is.na(surf)) next
    tourn <- tryCatch(fix$tournament$name[i], error=function(e) "")
    if (!keep_tournament(tourn)) next
    r <- tryCatch({
      predict_match_v2(p1_id, p2_id, surf, model=model,
                       profiles=profiles, melo=melo, mfull=mfull)
    }, error=function(e){if(i<=3)cat("PRED_ERR i=",i,":",conditionMessage(e),"\n"); NULL})
    if(is.null(r)) next
    # Get serve stats for Sharp breakdown
    get_prof <- function(pid) {
      p <- profiles[profiles$player_id==pid & profiles$target_surface==surf,]
      if(nrow(p)==0) p <- profiles[profiles$player_id==pid,]
      if(nrow(p)==0) return(NULL)
      p[which.max(p$n_matches),]
    }
    prof1 <- tryCatch(get_prof(p1_id), error=function(e) NULL)
    prof2 <- tryCatch(get_prof(p2_id), error=function(e) NULL)
    preds[[length(preds)+1]] <- data.frame(
      p1_id=p1_id, p2_id=p2_id, p1_name=n1, p2_name=n2, surface=surf, tournament=tourn, tournamentId=fix$tournamentId[i], match_date=as.character(as.Date(substr(fix$date[i]%||%fix$tournament$date[i],1,10))),
      p1_win=r$p1_win, p2_win=r$p2_win,
      fair_p1=round(1/r$p1_win,2), fair_p2=round(1/r$p2_win,2),
      elo_diff=r$elo_diff, elo1=r$elo1, elo2=r$elo2,
      h2h_record=r$h2h_record, h2h_surf=r$h2h_surf,
      p1_streak=r$p1_streak, p2_streak=r$p2_streak,
      p1_fatigue=r$p1_fatigue, p2_fatigue=r$p2_fatigue,
      p1_fatigue=r$p1_fatigue, p2_fatigue=r$p2_fatigue,
      p1_serve_in=if(!is.null(prof1)) round(prof1$first_serve_in_pct*100,1) else NA_real_,
      p2_serve_in=if(!is.null(prof2)) round(prof2$first_serve_in_pct*100,1) else NA_real_,
      p1_serve_won=if(!is.null(prof1)) round(prof1$first_serve_won_pct*100,1) else NA_real_,
      p2_serve_won=if(!is.null(prof2)) round(prof2$first_serve_won_pct*100,1) else NA_real_,
      p1_second_won=if(!is.null(prof1)) round(prof1$second_serve_won_pct*100,1) else NA_real_,
      p2_second_won=if(!is.null(prof2)) round(prof2$second_serve_won_pct*100,1) else NA_real_,
      p1_bp_conv=if(!is.null(prof1)) round(prof1$bp_conv_pct*100,1) else NA_real_,
      p2_bp_conv=if(!is.null(prof2)) round(prof2$bp_conv_pct*100,1) else NA_real_,
      p1_win_rate=if(!is.null(prof1)) round(prof1$win_rate*100,1) else NA_real_,
      p2_win_rate=if(!is.null(prof2)) round(prof2$win_rate*100,1) else NA_real_,
      tour=tour_label, generated=format(Sys.time(),"%Y-%m-%d %H:%M"),
      stringsAsFactors=FALSE
    )
  }
  cat(sprintf("run_predictions: name=%d prof=%d surf=%d tourn=%d preds=%d\n",n_name,n_prof,n_surf,n_tourn,length(preds)))
  dplyr::bind_rows(preds)
}

Sys.sleep(2)
fix_atp <- tryCatch({ d1<-fetch_fixtures_by_date("atp",format(Sys.Date(),"%Y-%m-%d")); d2<-fetch_fixtures_by_date("atp",format(Sys.Date()+1,"%Y-%m-%d")); dplyr::bind_rows(d1,d2) }, error=function(e) api_get_fixtures("atp",CONFIG,pages=10))
Sys.sleep(2)
fix_wta <- if(exists("logit_wta")) tryCatch({ d1<-fetch_fixtures_by_date("wta",format(Sys.Date(),"%Y-%m-%d")); d2<-fetch_fixtures_by_date("wta",format(Sys.Date()+1,"%Y-%m-%d")); dplyr::bind_rows(d1,d2) }, error=function(e) api_get_fixtures("wta",CONFIG,pages=10)) else data.frame()
cat(sprintf("[4/5] Fixtures: %d ATP + %d WTA\n", nrow(fix_atp), nrow(fix_wta)))

# Generate predictions
atp_preds <- run_predictions(fix_atp, names_lookup,
  features2$profiles, bundle$model,
  matches_full, matches_full, "atp")

wta_preds_df <- (function() {
  if (!exists("logit_wta")||!exists("features_wta")||!exists("wta_elo")||!exists("wta_full")) {
    cat("WTA model not loaded — skipping\n"); return(data.frame()) }
  preds <- list()
  for (i in seq_len(nrow(fix_wta))) {
    p1_id <- fix_wta$player1Id[i]; p2_id <- fix_wta$player2Id[i]
    n1 <- wta_names_lookup[[as.character(p1_id)]] %||% ""
    n2 <- wta_names_lookup[[as.character(p2_id)]] %||% ""
    if(grepl("/",n1)||grepl("/",n2)||n1==""||n2=="") next
    if(!p1_id %in% features_wta$profiles$player_id) next
    if(!p2_id %in% features_wta$profiles$player_id) next
    surf <- tryCatch(normalise_surface(fix_wta$tournament$court$name[i]),error=function(e) NA_character_)
    if(is.na(surf)) next
    tourn <- tryCatch(fix_wta$tournament$name[i],error=function(e) "")
    if(!keep_tournament(tourn)) next
    r <- tryCatch(predict_match_wta(p1_id,p2_id,surf),error=function(e) NULL)
    if(is.null(r)) next
    get_prof_wta <- function(pid) {
      p <- features_wta$profiles[features_wta$profiles$player_id==pid &
                                  features_wta$profiles$target_surface==surf,]
      if(nrow(p)==0) p <- features_wta$profiles[features_wta$profiles$player_id==pid,]
      if(nrow(p)==0) return(NULL)
      p[which.max(p$n_matches),]
    }
    prof1_wta <- tryCatch(get_prof_wta(p1_id), error=function(e) NULL)
    prof2_wta <- tryCatch(get_prof_wta(p2_id), error=function(e) NULL)
    preds[[length(preds)+1]] <- data.frame(
      p1_id=p1_id, p2_id=p2_id, p1_name=n1,p2_name=n2,surface=surf,tournament=tourn, tournamentId=fix_wta$tournamentId[i], match_date=as.character(as.Date(substr(fix_wta$date[i]%||%fix_wta$tournament$date[i],1,10))),
      p1_win=r$p1_win,p2_win=r$p2_win,
      fair_p1=round(1/r$p1_win,2),fair_p2=round(1/r$p2_win,2),
      elo_diff=r$elo_diff,elo1=r$elo1,elo2=r$elo2,
      h2h_record=r$h2h_record,h2h_surf=r$h2h_surf,
      p1_fatigue=r$p1_fatigue,p2_fatigue=r$p2_fatigue,
      p1_serve_in=if(!is.null(prof1_wta)) round(prof1_wta$first_serve_in_pct*100,1) else NA_real_,
      p2_serve_in=if(!is.null(prof2_wta)) round(prof2_wta$first_serve_in_pct*100,1) else NA_real_,
      p1_serve_won=if(!is.null(prof1_wta)) round(prof1_wta$first_serve_won_pct*100,1) else NA_real_,
      p2_serve_won=if(!is.null(prof2_wta)) round(prof2_wta$first_serve_won_pct*100,1) else NA_real_,
      p1_second_won=if(!is.null(prof1_wta)) round(prof1_wta$second_serve_won_pct*100,1) else NA_real_,
      p2_second_won=if(!is.null(prof2_wta)) round(prof2_wta$second_serve_won_pct*100,1) else NA_real_,
      p1_bp_conv=if(!is.null(prof1_wta)) round(prof1_wta$bp_conv_pct*100,1) else NA_real_,
      p2_bp_conv=if(!is.null(prof2_wta)) round(prof2_wta$bp_conv_pct*100,1) else NA_real_,
      p1_win_rate=if(!is.null(prof1_wta)) round(prof1_wta$win_rate*100,1) else NA_real_,
      p2_win_rate=if(!is.null(prof2_wta)) round(prof2_wta$win_rate*100,1) else NA_real_,
      tour="wta",generated=format(Sys.time(),"%Y-%m-%d %H:%M"),
      stringsAsFactors=FALSE
    )
  }
  dplyr::bind_rows(preds)
})()

# Country lookup for flags
country_df2 <- data.frame(player_id=integer(), country=character())


iso3_to_2 <- c(AUS="au",ESP="es",ITA="it",FRA="fr",GER="de",USA="us",GBR="gb",
  ARG="ar",BRA="br",SRB="rs",CRO="hr",SUI="ch",AUT="at",BEL="be",NED="nl",
  ROU="ro",CZE="cz",SVK="sk",POL="pl",RUS="ru",JPN="jp",KOR="kr",CHN="cn",
  KAZ="kz",GEO="ge",TUN="tn",IND="in",TUR="tr",CIV="ci",CGO="cg",NOR="no",
  SWE="se",DEN="dk",FIN="fi",POR="pt",GRE="gr",HUN="hu",BUL="bg",UKR="ua",
  CAN="ca",MEX="mx",COL="co",CHI="cl",PER="pe",ECU="ec",URU="uy",PAR="py",
  RSA="za",MAR="ma",EGY="eg",NZL="nz",TPE="tw",ISR="il",LTU="lt",LAT="lv",
  EST="ee",SLO="si",MDA="md",AZE="az",ARM="am",UZB="uz",BLR="by",MKD="mk",ALB="al")
country_df2$iso2 <- iso3_to_2[country_df2$country]
country_map <- setNames(country_df2$iso2, as.character(country_df2$player_id))
get_flag <- function(pid) { f <- country_map[as.character(pid)]; if(is.na(f)||is.null(f)) "" else unname(f) }


all_preds <- dplyr::bind_rows(atp_preds, wta_preds_df)
all_preds <- all_preds[order(-abs(all_preds$p1_win - 0.5)), ]
# Add country flags — lookup by name via reverse names_lookup
if (exists("get_flag") && exists("country_map")) {
  # Build name->playerID reverse lookup for ATP
  atp_name_to_id <- setNames(names(names_lookup), unlist(names_lookup))
  wta_name_to_id <- setNames(names(wta_names_lookup), unlist(wta_names_lookup))
  all_preds$p1_flag <- sapply(seq_len(nrow(all_preds)), function(i) {
    nm <- all_preds$p1_name[i]
    pid <- if(all_preds$tour[i]=="atp") atp_name_to_id[nm] else wta_name_to_id[nm]
    if(is.na(pid)) return("")
    get_flag(pid)
  })
  all_preds$p2_flag <- sapply(seq_len(nrow(all_preds)), function(i) {
    nm <- all_preds$p2_name[i]
    pid <- if(all_preds$tour[i]=="atp") atp_name_to_id[nm] else wta_name_to_id[nm]
    if(is.na(pid)) return("")
    get_flag(pid)
  })
  all_preds$p1_flag[is.na(all_preds$p1_flag)] <- ""
  all_preds$p2_flag[is.na(all_preds$p2_flag)] <- ""
  cat(sprintf("Flags: %d/%d p1, %d/%d p2\n",
    sum(all_preds$p1_flag!=""), nrow(all_preds),
    sum(all_preds$p2_flag!=""), nrow(all_preds)))
}
cat(sprintf("[5/5] Predictions: %d ATP + %d WTA = %d total\n",
           nrow(atp_preds), nrow(wta_preds_df), nrow(all_preds)))
# Save JSON
dir.create("output/daily", showWarnings=FALSE, recursive=TRUE)
# ── LIVE ODDS (The Odds API) ─────────────────────────────────────────────────
tryCatch({
  odds_api_key <- Sys.getenv("ODDS_API_KEY")
  if (nchar(odds_api_key) > 0) {
    # Get available tennis sports
    sports_r <- httr::GET("https://api.the-odds-api.com/v4/sports",
                          query=list(apiKey=odds_api_key))
    sports_data <- httr::content(sports_r, "parsed")
    tennis_keys <- sapply(
      Filter(function(s) grepl("tennis", s$key, ignore.case=TRUE), sports_data),
      function(s) s$key)

    odds_list <- list()
    for (sport_key in tennis_keys) {
      Sys.sleep(0.5)
      r <- httr::GET(
        paste0("https://api.the-odds-api.com/v4/sports/", sport_key, "/odds"),
        query=list(apiKey=odds_api_key, regions="eu", markets="h2h", oddsFormat="decimal")
      )
      if (httr::status_code(r) != 200) next
      matches <- httr::content(r, "parsed")
      for (m in matches) {
        bk <- Filter(function(b) b$key=="pinnacle", m$bookmakers)
        if (length(bk)==0) bk <- m$bookmakers[1]
        if (length(bk)==0) next
        outcomes <- bk[[1]]$markets[[1]]$outcomes
        if (length(outcomes) < 2) next
        odds_list[[length(odds_list)+1]] <- data.frame(
          p1_name=outcomes[[1]]$name, p2_name=outcomes[[2]]$name,
          p1_odds=outcomes[[1]]$price, p2_odds=outcomes[[2]]$price,
          bookmaker=bk[[1]]$key, stringsAsFactors=FALSE)
      }
    }

    if (length(odds_list) > 0) {
      odds_df <- dplyr::bind_rows(odds_list)
      cat(sprintf("Odds: %d matches from The Odds API\n", nrow(odds_df)))

      # Match by last name
      match_odds <- function(name, odds_names) {
        exact <- which(odds_names == name)
        if (length(exact) > 0) return(exact[1])
        last <- tolower(tail(strsplit(name, " ")[[1]], 1))
        fuzzy <- which(sapply(odds_names, function(n) grepl(last, tolower(n), fixed=TRUE)))
        if (length(fuzzy) > 0) return(fuzzy[1])
        return(NA_integer_)
      }

      all_names <- c(odds_df$p1_name, odds_df$p2_name)
      all_odds1 <- c(odds_df$p1_odds, odds_df$p2_odds)
      all_odds2 <- c(odds_df$p2_odds, odds_df$p1_odds)

      all_preds$p1_odds <- NA_real_
      all_preds$p2_odds <- NA_real_
      all_preds$bookmaker <- NA_character_

      for (i in seq_len(nrow(all_preds))) {
        idx <- match_odds(all_preds$p1_name[i], all_names)
        if (!is.na(idx)) {
          all_preds$p1_odds[i] <- all_odds1[idx]
          all_preds$p2_odds[i] <- all_odds2[idx]
          bk_idx <- if(idx <= nrow(odds_df)) idx else idx - nrow(odds_df)
          all_preds$bookmaker[i] <- odds_df$bookmaker[bk_idx]
        }
      }
      matched_odds <- sum(!is.na(all_preds$p1_odds))
      cat(sprintf("Odds matched: %d/%d predictions\n", matched_odds, nrow(all_preds)))
      # Flag predictions where model diverges >15pp from market implied probability
      all_preds$market_implied_p1 <- NA_real_
      all_preds$market_deviation  <- NA_real_
      all_preds$market_flag       <- FALSE
      for (i in seq_len(nrow(all_preds))) {
        if (is.na(all_preds$p1_odds[i]) || is.na(all_preds$p2_odds[i])) next
        raw1 <- 1/all_preds$p1_odds[i]; raw2 <- 1/all_preds$p2_odds[i]
        impl1 <- raw1/(raw1+raw2)  # overround-adjusted implied prob
        all_preds$market_implied_p1[i] <- round(impl1*100, 1)
        all_preds$market_deviation[i]  <- round(abs(all_preds$p1_win[i]*100 - impl1*100), 1)
        all_preds$market_flag[i]       <- all_preds$market_deviation[i] >= 15
      }
      flagged <- sum(all_preds$market_flag, na.rm=TRUE)
      if(flagged > 0) {
        cat(sprintf("Market flags: %d predictions diverge >15pp from closing line:\n", flagged))
        for(i in which(all_preds$market_flag)) {
          cat(sprintf("  ⚠ %s vs %s | Model: %.0f%% | Market: %.0f%% | Gap: %.0f pp\n",
            all_preds$p1_name[i], all_preds$p2_name[i],
            all_preds$p1_win[i]*100, all_preds$market_implied_p1[i],
            all_preds$market_deviation[i]))
        }
      }
    }
  }
}, error=function(e) cat("Odds API error:", e$message, "\n"))

all_preds <- all_preds[!is.na(all_preds$p1_win) & !is.na(all_preds$p2_win),]
# Remove predictions where model diverges >25pp from market (not sharp)
if("market_deviation" %in% names(all_preds)) {
  n_before <- nrow(all_preds)
  all_preds <- all_preds[is.na(all_preds$market_deviation) | all_preds$market_deviation <= 25,]
  n_removed <- n_before - nrow(all_preds)
  if(n_removed > 0) cat(sprintf("Removed %d predictions with >25pp market deviation\n", n_removed))
}
# Remove already-completed matches
tryCatch({
  comp_ids <- c()
  t_ids <- unique(all_preds$tournamentId[!is.na(all_preds$tournamentId)])
  for(tid in t_ids) {
    ttype <- if(any(all_preds$tournamentId==tid & all_preds$tour=="wta", na.rm=TRUE)) "wta" else "atp"
    rr <- tryCatch(httr::GET(sprintf("%s/%s/tournament/results/%s",CONFIG$api_base,ttype,tid),.hdrs(CONFIG)),error=function(e)NULL)
    if(is.null(rr)||httr::status_code(rr)!=200) next
    rd <- tryCatch(jsonlite::fromJSON(httr::content(rr,"text",encoding="UTF-8"),simplifyDataFrame=TRUE),error=function(e)NULL)
    if(is.null(rd)||is.null(rd$data$singles)||!is.data.frame(rd$data$singles)) next
    cx <- rd$data$singles[rd$data$singles$result_type=="completed",]
    if(nrow(cx)==0) next
    for(i in seq_len(nrow(all_preds))) {
      p1<-all_preds$p1_id[i]; p2<-all_preds$p2_id[i]
      if(is.na(p1)||is.na(p2)) next
      if(any((cx$player1Id==p1&cx$player2Id==p2)|(cx$player1Id==p2&cx$player2Id==p1)))
        comp_ids <- c(comp_ids, i)
    }
  }
  if(length(comp_ids)>0) {
    cat(sprintf("Removed %d completed matches\n",length(unique(comp_ids))))
    all_preds <- all_preds[-unique(comp_ids),,drop=FALSE]
    row.names(all_preds) <- NULL
  }
}, error=function(e) cat("Completed filter error:",e$message,"\n"))
# Remove already-completed matches
tryCatch({
  comp_ids <- c()
  t_ids <- unique(all_preds$tournamentId[!is.na(all_preds$tournamentId)])
  for(tid in t_ids) {
    ttype <- if(any(all_preds$tournamentId==tid & all_preds$tour=="wta", na.rm=TRUE)) "wta" else "atp"
    rr <- tryCatch(httr::GET(sprintf("%s/%s/tournament/results/%s",CONFIG$api_base,ttype,tid),.hdrs(CONFIG)),error=function(e)NULL)
    if(is.null(rr)||httr::status_code(rr)!=200) next
    rd <- tryCatch(jsonlite::fromJSON(httr::content(rr,"text",encoding="UTF-8"),simplifyDataFrame=TRUE),error=function(e)NULL)
    if(is.null(rd)||is.null(rd$data$singles)||!is.data.frame(rd$data$singles)) next
    cx <- rd$data$singles[rd$data$singles$result_type=="completed",]
    if(nrow(cx)==0) next
    for(i in seq_len(nrow(all_preds))) {
      p1<-all_preds$p1_id[i]; p2<-all_preds$p2_id[i]
      if(is.na(p1)||is.na(p2)) next
      if(any((cx$player1Id==p1&cx$player2Id==p2)|(cx$player1Id==p2&cx$player2Id==p1)))
        comp_ids <- c(comp_ids, i)
    }
  }
  if(length(comp_ids)>0) {
    cat(sprintf("Removed %d completed matches\n",length(unique(comp_ids))))
    all_preds <- all_preds[-unique(comp_ids),,drop=FALSE]
    row.names(all_preds) <- NULL
  }
}, error=function(e) cat("Completed filter error:",e$message,"\n"))
# Remove already-completed matches
tryCatch({
  comp_ids <- c()
  t_ids <- unique(all_preds$tournamentId[!is.na(all_preds$tournamentId)])
  for(tid in t_ids) {
    ttype <- if(any(all_preds$tournamentId==tid & all_preds$tour=="wta", na.rm=TRUE)) "wta" else "atp"
    rr <- tryCatch(httr::GET(sprintf("%s/%s/tournament/results/%s",CONFIG$api_base,ttype,tid),.hdrs(CONFIG)),error=function(e)NULL)
    if(is.null(rr)||httr::status_code(rr)!=200) next
    rd <- tryCatch(jsonlite::fromJSON(httr::content(rr,"text",encoding="UTF-8"),simplifyDataFrame=TRUE),error=function(e)NULL)
    if(is.null(rd)||is.null(rd$data$singles)) next
    if(!is.data.frame(rd$data$singles)) next
    cx <- rd$data$singles[rd$data$singles$result_type=="completed",]
    if(nrow(cx)==0) next
    for(i in seq_len(nrow(all_preds))) {
      p1<-all_preds$p1_id[i]; p2<-all_preds$p2_id[i]
      if(is.na(p1)||is.na(p2)) next
      if(any((cx$player1Id==p1&cx$player2Id==p2)|(cx$player1Id==p2&cx$player2Id==p1)))
        comp_ids <- c(comp_ids, i)
    }
  }
  if(length(comp_ids)>0) {
    cat(sprintf("Removed %d completed matches\n",length(unique(comp_ids))))
    all_preds <- all_preds[-unique(comp_ids),,drop=FALSE]
    row.names(all_preds) <- NULL
  }
}, error=function(e) cat("Completed filter error:",e$message,"\n"))
# Remove already-completed matches
tryCatch({
  comp_ids <- c()
  t_ids <- unique(all_preds$tournamentId[!is.na(all_preds$tournamentId)])
  for(tid in t_ids) {
    ttype <- if(any(all_preds$tournamentId==tid & all_preds$tour=="wta", na.rm=TRUE)) "wta" else "atp"
    rr <- tryCatch(httr::GET(sprintf("%s/%s/tournament/results/%s",CONFIG$api_base,ttype,tid),.hdrs(CONFIG)),error=function(e)NULL)
    if(is.null(rr)||httr::status_code(rr)!=200) next
    rd <- tryCatch(jsonlite::fromJSON(httr::content(rr,"text",encoding="UTF-8"),simplifyDataFrame=TRUE),error=function(e)NULL)
    if(is.null(rd)||is.null(rd$data$singles)) next
    if(!is.data.frame(rd$data$singles)) next
    cx <- rd$data$singles[rd$data$singles$result_type=="completed",]
    for(i in seq_len(nrow(all_preds))) {
      p1<-all_preds$p1_id[i]; p2<-all_preds$p2_id[i]
      if(is.na(p1)||is.na(p2)) next
      if(any((cx$player1Id==p1&cx$player2Id==p2)|(cx$player1Id==p2&cx$player2Id==p1)))
        comp_ids <- c(comp_ids, i)
    }
  }
  if(length(comp_ids)>0) {
    cat(sprintf("Removed %d completed matches\n",length(unique(comp_ids))))
    all_preds <- all_preds[-unique(comp_ids),,drop=FALSE]
    row.names(all_preds) <- NULL
  }
}, error=function(e) cat("Completed filter error:",e$message,"\n"))
jsonlite::write_json(all_preds, "output/daily/predictions_today.json",
                    pretty=TRUE, auto_unbox=TRUE)
meta <- list(
  generated=format(Sys.time(),"%Y-%m-%d %H:%M"),
  date=format(Sys.Date(),"%Y-%m-%d"),
  n_atp=nrow(atp_preds), n_wta=nrow(wta_preds_df),
  n_total=nrow(all_preds),
  model_accuracy_atp="68.7%", model_accuracy_wta="78.3%",
  backtest_roi="+19.2%"
)
jsonlite::write_json(meta, "output/daily/meta.json", pretty=TRUE, auto_unbox=TRUE)
cat("Saved\n")

# Push to GitHub
git_repo <- if(nchar(Sys.getenv("GITHUB_WORKSPACE"))>0) Sys.getenv("GITHUB_WORKSPACE") else "C:/Users/bwags/R/TennisIQ/tennisiq-predictions"
file.copy("output/daily/predictions_today.json",
          file.path(git_repo,"predictions_today.json"), overwrite=TRUE)
file.copy("output/daily/meta.json",
          file.path(git_repo,"meta.json"), overwrite=TRUE)
if(nchar(Sys.getenv("GITHUB_ACTIONS"))>0) {
  system("git config user.email actions@github.com")
  system("git config user.name GitHub-Actions")
  system("git add predictions_today.json results.json meta.json")
  system(paste0("git commit -m \"Predictions ", format(Sys.Date(),"%a %m/%d/%Y"), "\""))
  system("git push")
} else {
  shell("C:/Users/bwags/R/TennisIQ/Claude/push_predictions.bat", wait=TRUE)
}
cat("Pushed to GitHub\n")
cat(sprintf("Done: %d ATP + %d WTA predictions live\n", nrow(atp_preds), nrow(wta_preds_df)))

# Generate social posts
tryCatch({
  source("generate_posts.R")
  cat("Social posts generated
")
}, error=function(e) cat("Post generation failed:", e$message, "
"))

# ── RESULTS TRACKER (Tournament Results API) ────────────────────────────────
tryCatch({
  log_dir <- "output/predictions_log"
  if(!dir.exists(log_dir)) dir.create(log_dir, recursive=TRUE)

  # Save today's predictions log if not already saved
  today_log <- file.path(log_dir, paste0("predictions_", Sys.Date(), ".rds"))
  if(!file.exists(today_log) && nrow(all_preds)>0) {
    log_data <- all_preds
      if(!is.data.frame(res_data$data$singles)) next
    log_data$pred_date <- Sys.Date()
    log_data$result <- NA_character_
    log_data$correct <- NA
    saveRDS(log_data, today_log)
    cat(sprintf("Saved prediction log: %d predictions\n", nrow(log_data)))
  }

  # Load all logs
  log_files <- list.files(log_dir, pattern="*.rds", full.names=TRUE)
  if(length(log_files)==0) stop("No prediction logs found")

  total_matched <- 0; total_correct <- 0

  for(f in log_files) {
    plog <- readRDS(f)
    if(!"p1_id" %in% names(plog)) next  # skip old logs without IDs
    if(!"correct" %in% names(plog)) plog$correct <- NA
    if(!"result" %in% names(plog)) plog$result <- NA_character_
    unresolved <- which(is.na(plog$correct))
    if(length(unresolved)==0) next

    # Get unique tournament IDs from unresolved predictions
    tourn_ids <- unique(c(
      if("tournamentId" %in% names(plog)) plog$tournamentId[unresolved] else NULL
    ))

    # Also derive from fixture data for current tournaments
    pred_tourns <- unique(plog$tournament[unresolved])
    tourn_ids_from_fix <- unique(fix_atp$tournamentId[
      fix_atp$tournament$name %in% pred_tourns])
    tourn_ids <- unique(c(tourn_ids, tourn_ids_from_fix))
    tourn_ids <- tourn_ids[!is.na(tourn_ids)]

    updated <- FALSE
    for(tid in tourn_ids) {
      # Fetch results for this tournament
      res_r <- tryCatch(
        httr::GET(sprintf("%s/atp/tournament/results/%s", CONFIG$api_base, tid), .hdrs(CONFIG)),
        error=function(e) NULL
      )
      if(is.null(res_r) || httr::status_code(res_r)!=200) next
      res_data <- tryCatch(
        jsonlite::fromJSON(httr::content(res_r,"text",encoding="UTF-8"), simplifyDataFrame=TRUE),
        error=function(e) NULL
      )
      if(is.null(res_data$data$singles)||!is.data.frame(res_data$data$singles)) next
      results_df <- res_data$data$singles
      results_df <- results_df[results_df$result_type=="completed",]
      if(nrow(results_df)==0) next

      # Match predictions to results by player ID
      for(i in unresolved) {
        p1 <- plog$p1_id[i]; p2 <- plog$p2_id[i]
        if(is.na(p1)||is.na(p2)) next
        matched <- results_df[
          (results_df$player1Id==p1 & results_df$player2Id==p2) |
          (results_df$player1Id==p2 & results_df$player2Id==p1),]
        if(nrow(matched)==0) next
        winner_id <- matched$match_winner[1]
        winner_name <- if(winner_id==matched$player1Id[1]) matched$player1$name[1] else matched$player2$name[1]
        pick_id <- if(plog$p1_win[i]>plog$p2_win[i]) p1 else p2
        plog$result[i] <- winner_name
        plog$correct[i] <- (winner_id == pick_id)
        total_matched <- total_matched+1
        if(plog$correct[i]) total_correct <- total_correct+1
        cat(sprintf("  Resolved: %s vs %s → %s [%s]\n",
          plog$p1_name[i], plog$p2_name[i], winner_name,
          if(plog$correct[i]) "✓" else "✗"))
        updated <- TRUE
      }
    }
    if(updated) saveRDS(plog, f)
  }
  cat(sprintf("Results matched: %d/%d correct (%.1f%%)\n",
    total_correct, total_matched,
    if(total_matched>0) total_correct/total_matched*100 else 0))
}, error=function(e) cat("Results tracker error:", e$message, "\n"))
# Update results JSON
tryCatch({
  log_dir <- "output/predictions_log"
  logs <- list.files(log_dir, pattern="*.rds", full.names=TRUE)
  all_res <- dplyr::bind_rows(lapply(logs, function(f) {
    d <- readRDS(f); d[!is.na(d$correct),]
  }))
  all_res <- dplyr::distinct(dplyr::arrange(all_res, pred_date), p1_name, p2_name, tournament, .keep_all=TRUE)
  all_res <- dplyr::distinct(dplyr::arrange(all_res, pred_date), p1_name, p2_name, tournament, .keep_all=TRUE)
  all_res <- dplyr::distinct(dplyr::arrange(all_res, pred_date), p1_name, p2_name, tournament, .keep_all=TRUE)
  week_res     <- if(nrow(all_res)>0) all_res[all_res$pred_date >= Sys.Date()-7,] else all_res
  week_total   <- nrow(week_res)
  week_correct <- if(nrow(week_res)>0) sum(week_res$correct,na.rm=TRUE) else 0L
  min_thresh   <- 50
  show_acc     <- week_total >= min_thresh
  smry <- list(
    total_predicted  = sum(sapply(logs,function(f) nrow(readRDS(f)))),
    total_resolved   = nrow(all_res),
    week_resolved    = week_total,
    week_correct     = week_correct,
    accuracy         = if(show_acc) round(week_correct/week_total*100,1) else NULL,
    accuracy_label   = if(show_acc) paste0(round(week_correct/week_total*100,1),"%") else "Building...",
    min_threshold    = min_thresh,
    last_updated     = format(Sys.time(),"%Y-%m-%d %H:%M"),
    recent_results   = if(nrow(all_res)>0) tail(
      all_res[,c("p1_name","p2_name","surface","tournament",
                 "p1_win","p2_win","result","correct","pred_date")],50
    ) else list()
  )
  jsonlite::write_json(smry,"output/daily/results.json",pretty=TRUE,auto_unbox=TRUE)
  file.copy("output/daily/results.json",file.path(git_repo,"results.json"),overwrite=TRUE)
  cat(sprintf("Results JSON: %d resolved, %s\n", smry$total_resolved, smry$accuracy_label))
}, error=function(e) cat("Results JSON error:", e$message, "\n"))
