library(jsonlite);library(dplyr)
SITE_URL <- "thetennishq.com/tennisiq.html"
PROMO <- "WIMBLEDON"
PROMO_ACTIVE <- TRUE
`%||%` <- function(a,b) if(is.null(a)||is.na(a)) b else a

preds <- tryCatch(jsonlite::fromJSON(file.path(git_repo,"predictions_today.json")),error=function(e){cat("Failed to load predictions\n");NULL})
if(is.null(preds)||nrow(preds)==0) stop("No predictions")
preds <- preds[!is.na(preds$p1_win) & !is.na(preds$p2_win) & !grepl("/",preds$p1_name)&!grepl("/",preds$p2_name),]
preds <- preds[order(-abs(preds$p1_win-0.5)),]

fav_name <- function(m) { if(is.na(m$p1_win)||is.na(m$p2_win)) return(NA_character_); if(m$p1_win>m$p2_win) m$p1_name else m$p2_name }
dog_name <- function(m) { if(is.na(m$p1_win)||is.na(m$p2_win)) return(NA_character_); if(m$p1_win>m$p2_win) m$p2_name else m$p1_name }
fav_prob <- function(m) round(max(m$p1_win,m$p2_win)*100)
dog_prob <- function(m) round(min(m$p1_win,m$p2_win)*100)
fav_fat  <- function(m) if(m$p1_win>m$p2_win) (m$p1_fatigue%||%0) else (m$p2_fatigue%||%0)
dog_streak<-function(m) if(m$p1_win>m$p2_win) (m$p2_streak%||%0) else (m$p1_streak%||%0)
fav_streak<-function(m) if(m$p1_win>m$p2_win) (m$p1_streak%||%0) else (m$p2_streak%||%0)
surf_emoji <- function(s) switch(tolower(s),clay="🟤",hard="🔵",grass="🟢","🎾")
tour_short <- function(t) trimws(gsub("Open$","",gsub(" - .*","",t)))

upset_signals <- function(m) {
  sigs <- c()
  if(is.null(m$market_implied_p1)||is.na(m$market_implied_p1)) return(c())
  edge <- round(m$p1_win*100 - m$market_implied_p1, 1)
  if(edge > 0 && edge <= 25) sigs <- c(sigs, paste0("Model gives ", round(m$p1_win*100), "% vs market implied ", m$market_implied_p1, "% — ", edge, "pp edge"))
  sigs
}
is_upset_watch <- function(m) { length(upset_signals(m)) >= 1 }
tournaments <- unique(preds$tournament)
is_wimbledon <- any(grepl("Wimbledon",tournaments,ignore.case=TRUE))
day_label <- format(Sys.Date(),"%B %d")
tourney_header <- if(is_wimbledon) "Wimbledon" else paste(sapply(tournaments[1:min(2,length(tournaments))],tour_short),collapse=" & ")
tourney_tag <- if(is_wimbledon) "#Wimbledon #Wimbledon2026" else "#Tennis #ATP"
top3 <- head(preds[preds$p1_win>0.65|preds$p2_win>0.65,],3)
upset_watches <- local({ uw <- preds[sapply(1:nrow(preds),function(i) is_upset_watch(preds[i,])),]; if(nrow(uw)>0) { uw$edge <- uw$p1_win*100 - uw$market_implied_p1; uw[order(-uw$edge),] } else uw })

# POST 1: Morning Preview
morning_post <- local({
  lines <- c(paste0("🎾 ",tourney_header," · ",day_label),paste0("tennisIQ model — ",nrow(preds)," predictions loaded"),"")
  if(nrow(top3)>0) {
    lines <- c(lines,"Top model picks today:")
    for(i in 1:nrow(top3)) { m<-top3[i,]; lines<-c(lines,paste0(surf_emoji(m$surface)," ",fav_name(m)," ",fav_prob(m),"% · ",tour_short(m$tournament))) }
    lines <- c(lines,"")
  }
  if(nrow(upset_watches)>0) {
    m<-upset_watches[1,]; sigs<-upset_signals(m)
    lines<-c(lines,paste0("⚡ Value Watch: ",dog_name(m)," vs ",fav_name(m)),paste0("→ ",sigs[1]),"")
  }
  lines <- c(lines,"Full Matchup DNA + all picks 👇",SITE_URL)
  if(PROMO_ACTIVE) lines <- c(lines,"",paste0("50% off first month → code ",PROMO))
  lines <- c(lines,"",paste0(tourney_tag," #TennisAnalytics #TennisIQ"))
  paste(lines,collapse="
")
})

# POST 2: Upset Alert
upset_post <- if(nrow(upset_watches)>0) local({
  m<-upset_watches[1,]; sigs<-upset_signals(m)
  lines<-c(paste0("⚡ VALUE WATCH — ",tour_short(m$tournament)),"",paste0("Models see this as closer than the market suggests:"))
  for(s in sigs[1:min(2,length(sigs))]) lines<-c(lines,paste0("→ ",s))
  lines<-c(lines,"",paste0("Sharp subscribers notified this morning."),"",paste0("Full DNA: ",SITE_URL),"",paste0(tourney_tag," #TennisIQ #MatchPrediction"))
  paste(lines,collapse="
")
}) else NULL

# POST 3: Stat of Day
stats_pool <- list(
  list(stat="34%",  text="When Elo gap is under 100 points, upsets happen 34% of the time. Nearly 1 in 3."),
  list(stat="32%",  text="Grass produces more upsets than any surface — favourites lose 32% vs 27% on hard."),
  list(stat="84%",  text="Fatigued favourites (4+ matches) still win 84% of the time. Momentum beats fatigue."),
  list(stat="+19.2%",text="Our model beats the Pinnacle closing line by +19.2%. The gold standard for edge."),
  list(stat="68.7%",text="Backtested across 110,000+ ATP and WTA matches. 68.7% accuracy with surface Elo.")
)
idx <- (as.integer(format(Sys.Date(),"%j")) %% length(stats_pool))+1
s <- stats_pool[[idx]]
stat_post <- paste(c("📊 tennisIQ Data Point","",paste0(s$stat),"",s$text,"",paste0("From 110,000+ matches: ",SITE_URL),"",paste0(tourney_tag," #TennisData #TennisIQ")),collapse="
")

# POST 4: Promo
promo_post <- paste(c("🎾 Wimbledon is here.","","tennisIQ gives you:","→ Daily model predictions for every match","→ Matchup DNA — visual breakdown of every contest","→ Surface Elo, serve profiles, fatigue signals","→ Upset alerts before the upsets happen","","Free through July 12 — no code needed.",SITE_URL,"",paste0(tourney_tag," #TennisIQ")),collapse="
")

# POST 5: Instagram
instagram_post <- if(nrow(top3)>0) local({
  m<-top3[1,]
  lines<-c(paste0("🎾 ",tourney_header," — ",day_label),"",paste0("Top matchup: ",fav_name(m)," vs ",dog_name(m)),paste0(surf_emoji(m$surface)," ",tools::toTitleCase(m$surface)," · ",tour_short(m$tournament)),"",paste0("tennisIQ model: ",fav_name(m)," ",fav_prob(m),"%"),paste0("Elo gap: ",abs(round(m$elo_diff))," points"),"")
  if(nrow(upset_watches)>0) { uw<-upset_watches[1,]; lines<-c(lines,paste0("⚡ Value Watch: ",dog_name(uw)," (",dog_prob(uw),"%)"),"") }
  lines<-c(lines,"Full Matchup DNA at link in bio 👆","",if(PROMO_ACTIVE) paste0("Free through July 12 🎾") else NULL,"","#Wimbledon #Wimbledon2026 #Tennis #TennisAnalytics #TennisData #TennisIQ #ATP #WTA")
  paste(lines,collapse="
")
}) else NULL

# OUTPUT
posts <- list(morning_preview=morning_post,upset_alert=upset_post,stat_of_day=stat_post,promo=promo_post,instagram=instagram_post)
cat("
",rep("=",60),"
",sep="")
cat("tennisIQ SOCIAL POSTS —",format(Sys.Date(),"%B %d %Y"),"
")
cat(rep("=",60),"

",sep="")
for(nm in names(posts)) {
  if(!is.null(posts[[nm]])) {
    cat(strrep("-",40),"
",sep="")
    cat(toupper(gsub("_"," ",nm)),"
")
    cat(strrep("-",40),"
",sep="")
    cat(posts[[nm]],"

")
  }
}
out_path <- paste0("output/social_posts_",format(Sys.Date(),"%Y-%m-%d"),".txt")
dir.create("output",showWarnings=FALSE)
out_lines <- unlist(lapply(names(posts),function(nm) { if(is.null(posts[[nm]])) return(NULL); c(strrep("-",40),toupper(gsub("_"," ",nm)),strrep("-",40),posts[[nm]],"","") }))
writeLines(out_lines, out_path)
cat("Saved to:",out_path,"
")

# ── EMAIL DAILY POSTS ───────────────────────────────────────────────────────
tryCatch({
  send_email_sg <- function(to, subject, body_text) {
    api_key <- Sys.getenv("SENDGRID_API_KEY")
    if(nchar(api_key)==0) { cat("No SendGrid key\n"); return(FALSE) }
    payload <- list(
      personalizations = list(list(to=list(list(email=to)))),
      from = list(email="predictions@thetennishq.com", name="tennisIQ"),
      subject = subject,
      content = list(list(type="text/plain", value=body_text))
    )
    r <- httr::POST("https://api.sendgrid.com/v3/mail/send",
      httr::add_headers(Authorization=paste("Bearer",api_key),`Content-Type`="application/json"),
      body=jsonlite::toJSON(payload,auto_unbox=TRUE), encode="raw")
    httr::status_code(r) == 202
  }

  # Read todays posts
  posts_text <- paste(out_lines, collapse="
")
  n_preds <- tryCatch({
    p <- jsonlite::fromJSON(file.path(git_repo,"predictions_today.json"))
    nrow(p)
  }, error=function(e) 0)

  subject <- paste0("tennisIQ Posts — ", format(Sys.Date(),"%B %d"), 
                    " · ", n_preds, " predictions")

  ok <- send_email_sg("yungbluds444@gmail.com", subject, posts_text)
  if(ok) cat("Daily posts emailed\n") else cat("Email failed\n")
}, error=function(e) cat("Email error:", e$message, "\n"))
