special_artists <- c("coil",
                     "popol vuh",
                     "oneohtrix point never",
                     "colin stetson",
                     "robert wyatt",
                     "soft machine",
                     "psychic tv",
                     "embryo",
                     "glenn branca",
                     "fuck buttons",
                     "dean blunt",
                     "александр башлачев",
                     "tim hecker",
                     "laurel halo",
                     "nicolas jaar",
                     "hype williams",
                     "inga copeland",
                     "mulatu astatke",
                     "john coltrane",
                     "boredoms",
                     "alice coltrane",
                     "king crimson",
                     "eno",
                     "brian eno",
                     "23 skidoo",
                     "deepchord presents echospace",
                     "ariel pink",
                     "gas",
                     "аукцыон",
                     "звуки му",
                     "owen pallett",
                     "pharoah sanders",
                     "darkside",
                     "this heat",
                     "the field",
                     "autechre",
                     "tuxedomoon",
                     "boards of canada",
                     "mahavishnu orchestra",
                     "shpongle",
                     "tindersticks")


uid_con <- dbConnect(SQLite(), dbname="data/uids.db")

df.interesting <- data.frame()

for (uni_id in c(297, 269, 250, 128, 304, 241, 240)) {
  try({
    print(uni_id)
    
    query <- sprintf("SELECT * FROM uid_db WHERE uni_id = %d", uni_id)
    dt.uids <- data.table(
      dbGetQuery(uid_con, query))
    
    
    dbname <- paste("C:/Databases/VK_data/",uni_id, ".db", sep = "")
    pllst_con <- dbConnect(SQLite(), dbname=dbname)
    
    table_names <- dbListTables(pllst_con)
    if ("pllst_db" %in% table_names) {
      df.pllst <- dbGetQuery(pllst_con, "SELECT * FROM pllst_db")
    }else{
      df.pllst <- dbGetQuery(pllst_con, "SELECT * FROM pllst_counts_db")
    }
    
    df.pllst$artist <- tolower(iconv(df.pllst$artist,"UTF-8","WINDOWS-1251"))
    df.pllst$artist <- str_replace(df.pllst$artist, pattern = "\\s{2,}", " ")
    df.pllst$artist <- str_replace(df.pllst$artist, pattern = "^\\s+", "")
    df.pllst$artist <- str_replace(df.pllst$artist, pattern = "-+\\s*$", "")
    df.pllst$artist <- str_replace(df.pllst$artist, pattern = "\\s+$", "")
    df.pllst$artist <- str_replace(df.pllst$artist, pattern = "ё", "е")
    
    
    df.pllst <- df.pllst[,c("uid", "artist")]
    dt.pllst <- data.table(df.pllst)
    dt.pllst <- dt.pllst[,list(artist=unique(artist)),
                         by=list(uid)]
    
    dt.pllst <- merge(dt.pllst, dt.uids, by="uid", all.x=T, all.y=F)
    
    uids <- dt.pllst$uid[dt.pllst$artist %in% special_artists]
    dt.interesting_part <- dt.pllst[dt.pllst$uid%in%uids,]
    df.interesting_part <- as.data.frame(dt.interesting_part)
    
    df.interesting <- rbind(df.interesting, df.interesting_part)
    dbDisconnect(pllst_con)
  })
}

df.interesting <- df.interesting[df.interesting$sex==1,]
df.interesting <- df.interesting[df.interesting$age>=20,]
df.interesting <- df.interesting[!is.na(df.interesting$artist),]
df.interesting <- merge(df.interesting, uni.data, by="uni_id",all.x=T,all.y=F)
df.interesting <- df.interesting[!is.na(df.interesting$city_id),]
df.interesting <- df.interesting[df.interesting$city_id == 1,]

#=============================
dt.intersting <- data.table(df.interesting)
dt.intersting.stat <- dt.intersting[,list(score=length(artist[artist%in%special_artists])),
                                    by=list(uid, city, city_id, uni, mean_score, uni_profile,age)]
dt.intersting.stat <- dt.intersting.stat[order(dt.intersting.stat$score,dt.intersting.stat$mean_score,decreasing = T)]
dt.intersting.stat <- dt.intersting.stat[dt.intersting.stat$age<=24 & dt.intersting.stat$city_id==1 & dt.intersting.stat$mean_score>=69,]
#=============================