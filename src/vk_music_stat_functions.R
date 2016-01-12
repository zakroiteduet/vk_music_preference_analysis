library("RSQLite")
library("stringr")
library("data.table")


preprocess_names <- function(v.artists) {
  v.artists <- tolower(iconv(v.artists,"UTF-8","WINDOWS-1251"))
  v.artists <- str_replace(v.artists, pattern = "\\s{2,}", " ")
  v.artists <- str_replace(v.artists, pattern = "-+\\s*$", "")
  v.artists <- str_replace(v.artists, pattern = "¸", "å")
  v.artists <- str_replace(v.artists, pattern = "\\sfeat[\\.\\s].+", "")
  v.artists <- str_replace(v.artists, pattern = "\\sft[\\.\\s].+", "")
  v.artists <- str_replace(v.artists, pattern = "\\(.+\\)", "")
  v.artists <- str_replace(v.artists, pattern = "\\[.+\\]", "")
  v.artists <- str_replace(v.artists, pattern = "\\s+$", "")
  v.artists <- str_replace(v.artists, pattern = "^\\s+", "")
  return(v.artists)
}


load_playlists <- function(uni_id) {
  dbname <- paste("C:/Databases/VK_data/",uni_id, ".db", sep = "")
  pllst_con <- dbConnect(SQLite(), dbname=dbname)
  
  table_names <- dbListTables(pllst_con)
  if ("pllst_db" %in% table_names) {
    df.pllst <- dbGetQuery(pllst_con, "SELECT * FROM pllst_db")
    df.pllst$count <- 1
  }else{
    df.pllst <- dbGetQuery(pllst_con, "SELECT * FROM pllst_counts_db")
  }
  
  df.pllst$artist <- preprocess_names(df.pllst$artist)
  dt.pllst <- data.table(df.pllst)
  dt.pllst <- dt.pllst[,list(count=sum(count)),
                       by=list(uid, artist)]
  dbDisconnect(pllst_con)
  return(dt.pllst)
}


load_uid_data <- function(uni_id) {
  uid_con <- dbConnect(SQLite(), dbname="data/uids.db")
  query <- sprintf("SELECT * FROM uid_db WHERE uni_id = %d", uni_id)
  dt.uids <- data.table(
    dbGetQuery(uid_con, query))
  dbDisconnect(uid_con)
  return(dt.uids)
}

get_playlist_detail_stat <- function(uni_id, uids=c(), artists=c()) {
  dt.uids <- load_uid_data(uni_id)
  if (length(uids) != 0) {
    dt.uids <- dt.uids[dt.uids$uid %in% uids]
  }
  dt.pllst <- load_playlists(uni_id)
  if (length(artists) != 0) {
    dt.pllst <- dt.pllst[dt.pllst$artist %in% artists]
  }
  
  dt.pllst <- merge(dt.pllst, dt.uids, by="uid", all.x=F, all.y=F)
  dt.pllst.stat <- dt.pllst[,list(artist_count=length(uid)),
                            by=list(uni_id, age, sex, artist)]
  dt.pllst.stat <- dt.pllst.stat[order(dt.pllst.stat$artist_count, decreasing = T),]
  return(dt.pllst.stat)
}