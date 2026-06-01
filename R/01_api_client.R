.cn <- function(a,b){if(is.null(a)||length(a)==0)return(b);if(length(a)==1&&!is.list(a)&&is.na(a))return(b);a}
.hdrs <- function(cfg){httr::add_headers("X-RapidAPI-Key"=cfg$api_key,"X-RapidAPI-Host"=cfg$api_host)}
.cache_path <- function(key){dir<-"data/cache";if(!dir.exists(dir))dir.create(dir,recursive=TRUE);file.path(dir,paste0(gsub("[^a-zA-Z0-9_-]","_",key),".rds"))}
.cache_get <- function(key){p<-.cache_path(key);if(file.exists(p))readRDS(p) else NULL}
.cache_set <- function(key,val){saveRDS(val,.cache_path(key));invisible(val)}
.api_get <- function(url,query=list(),cfg,cache_key=NULL){
  if(!is.null(cache_key)){cached<-.cache_get(cache_key);if(!is.null(cached))return(cached)}
  Sys.sleep(cfg$api_delay)
  for(attempt in seq_len(cfg$api_retries)){
    resp<-tryCatch(httr::GET(url,.hdrs(cfg),query=query,httr::timeout(30)),error=function(e)NULL)
    if(is.null(resp)){Sys.sleep(cfg$api_retry_wait*attempt);next}
    status<-httr::status_code(resp)
    if(status==200){
      parsed<-tryCatch(jsonlite::fromJSON(httr::content(resp,"text",encoding="UTF-8"),simplifyDataFrame=TRUE,flatten=FALSE),error=function(e)NULL)
      if(!is.null(cache_key)&&!is.null(parsed)).cache_set(cache_key,parsed)
      return(parsed)}
    if(status==429){Sys.sleep(cfg$api_retry_wait*attempt);next}
    if(status>=500){Sys.sleep(cfg$api_retry_wait);next}
    return(NULL)}
  NULL}
api_get_fixtures <- function(tour="atp",cfg,pages=5){
  url<-sprintf("%s/%s/fixtures",cfg$api_base,tour)
  rows<-list()
  for(pg in seq_len(pages)){
    res<-.api_get(url,list(pageNo=pg,pageSize=50,include="tournament,tournament.court,tournament.rank,round"),cfg)
    if(is.null(res)||!is.data.frame(res$data))break
    rows[[pg]]<-res$data
    if(!isTRUE(res$hasNextPage))break
    Sys.sleep(cfg$api_delay)}
  if(length(rows)==0)return(data.frame())
  dplyr::bind_rows(rows)}
normalise_surface <- function(x){
  x<-tolower(trimws(as.character(x)))
  dplyr::case_when(
    x%in%c("hard","outdoor hard","h.hard","1")~"hard",
    x%in%c("clay","2")~"clay",
    x%in%c("grass","3")~"grass",
    x%in%c("i.hard","indoor hard","carpet","indoor","4","5")~"ihard",
    TRUE~NA_character_)}
extract_match_stats <- function(matches_df,player_id){
  if(nrow(matches_df)==0||!"stats"%in%names(matches_df))return(matches_df)
  stats_df<-matches_df$stats[[1]]
  if(is.null(stats_df)||nrow(stats_df)==0)return(matches_df)
  stats_clean<-data.frame(
    won=as.integer(matches_df$match_winner==player_id),
    first_serve_in=stats_df$firstServe,first_serve_in_of=stats_df$firstServeOf,
    aces=stats_df$aces,double_faults=stats_df$doubleFaults,
    first_serve_won=stats_df$winningOnFirstServe,first_serve_won_of=stats_df$winningOnFirstServeOf,
    second_serve_won=stats_df$winningOnSecondServe,second_serve_won_of=stats_df$winningOnSecondServeOf,
    bp_converted=stats_df$breakPointsConverted,bp_converted_of=stats_df$breakPointsConvertedOf,
    total_pts_won=stats_df$totalPointsWon)
  stats_clean$first_serve_in_pct<-stats_clean$first_serve_in/pmax(stats_clean$first_serve_in_of,1)
  stats_clean$first_serve_won_pct<-stats_clean$first_serve_won/pmax(stats_clean$first_serve_in,1)
  stats_clean$second_serve_won_pct<-stats_clean$second_serve_won/pmax(stats_clean$second_serve_won_of,1)
  stats_clean$bp_conv_pct<-stats_clean$bp_converted/pmax(stats_clean$bp_converted_of,1)
  stats_clean$ace_rate<-stats_clean$aces/pmax(stats_clean$first_serve_in_of,1)
  stats_clean$df_rate<-stats_clean$double_faults/pmax(stats_clean$first_serve_in_of,1)
  meta<-data.frame(
    match_id=matches_df$id,date=as.Date(matches_df$date),
    player1_id=matches_df$player1Id,player2_id=matches_df$player2Id,
    opponent_id=ifelse(matches_df$player1Id==player_id,matches_df$player2Id,matches_df$player1Id),
    tournament_id=matches_df$tournamentId,result=matches_df$result,
    surface=normalise_surface(matches_df$tournament$court$name),
    tour_rank=matches_df$tournament$rankId,stringsAsFactors=FALSE)
  cbind(meta,stats_clean)}
