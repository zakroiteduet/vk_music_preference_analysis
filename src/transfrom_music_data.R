library("RSQLite")
library("stringr")
library("data.table")

setwd("C:/Source code/vk_music_preference_analysis/")
load("outdata/uni.data.Rdata")
uni.data <- as.data.table(uni.data)
uni.data <- uni.data[order(uni.data$mean_score,decreasing = T),]
uni.data$order <- c(1:439)


dir.create("outdata/counts/", showWarnings = FALSE)



uid_con <- dbConnect(SQLite(), dbname="data/uids.db")

df.interesting <- data.frame()

db_filenames <- list.files("C:/Databases/VK_data/",pattern = "*.db")
for (db_filename in db_filenames) {
  try({
    uni_id <- as.numeric(str_extract(db_filename, "\\d+"))
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
    
    
    dt.pllst[,uid_count:=length(unique(uid)),
             by=list(age, sex, uni_id)]
      
    dt.pllst.stat <- dt.pllst[,list(artist_count=length(uid)),
                              by=list(uni_id, age, sex, artist, uid_count)]
    
    dt.pllst.stat <- dt.pllst.stat[order(dt.pllst.stat$artist_count, decreasing = T),]
    dbDisconnect(pllst_con)
    
    outname <- paste("outdata/counts/",uni_id, ".Rdata", sep = "")
    
    save(dt.pllst.stat,
         file = outname)
    
  })
}

# df.interesting <- df.interesting[df.interesting$sex==1,]
# df.interesting <- df.interesting[df.interesting$age>=20,]
# df.interesting <- df.interesting[!is.na(df.interesting$artist),]
# df.interesting <- merge(df.interesting, uni.data, by="uni_id",all.x=T,all.y=F)
# df.interesting <- df.interesting[!is.na(df.interesting$city_id),]
# df.interesting <- df.interesting[df.interesting$city_id == 1,]
# 
# #=============================
# dt.intersting <- data.table(df.interesting)
# dt.intersting.stat <- dt.intersting[,list(score=length(artist[artist%in%special_artists])),
#                                     by=list(uid, city, city_id, uni, mean_score, uni_profile,age)]
# dt.intersting.stat <- dt.intersting.stat[order(dt.intersting.stat$score,dt.intersting.stat$mean_score,decreasing = T)]
# dt.intersting.stat <- dt.intersting.stat[dt.intersting.stat$age<=24 & dt.intersting.stat$city_id==1 & dt.intersting.stat$mean_score>=69,]
#=============================
load("outdata/dt.pllst.whole_stat.RData")
sel_artists <- dt.pllst.whole_stat$artist[dt.pllst.whole_stat$artist_count>100]
sel_artists <- sel_artists[!is.na(sel_artists)]

dt.pllst.whole_stat <- data.table()
passed_uni_ids <- c()
rdata_filenames <- list.files("outdata/counts/", pattern = "*.Rdata",full.names = T)
for (rdata_filename in rdata_filenames) {
  try({
    load(rdata_filename)
    print(rdata_filename)
    dt.pllst.stat <- dt.pllst.stat[dt.pllst.stat$artist %in% sel_artists]
    dt.pllst.stat <- merge(dt.pllst.stat, uni.data, by="uni_id", all.x=T, all.y=F)
    dt.pllst.stat <- dt.pllst.stat[!is.na(dt.pllst.stat$mean_score)]
    dt.pllst.stat <- dt.pllst.stat[,list(artist_count=sum(artist_count),
                                         sum_score=sum(mean_score * artist_count), sum_sq_score=sum(artist_count * mean_score^2),
                                         sum_order=sum(artist_count * order), sum_sq_order=sum(artist_count * order^2)),
                                   by=list(artist, uni_profile, sex, age, uni_id)]
    
    #dt.pllst.stat <- merge(dt.pllst.stat, dt.user_count.stat, by=c("sex", "age", "uni_id"), all.x=F)
    passed_uni_ids <- c(passed_uni_ids, unique(dt.pllst.stat$uni_id))
    dt.pllst.stat[,uni_id:=NULL]
    
    dt.pllst.whole_stat <- rbind(dt.pllst.whole_stat, dt.pllst.stat)
    dt.pllst.whole_stat <- dt.pllst.whole_stat[,list(artist_count=sum(artist_count),
                                         sum_score=sum(sum_score), sum_sq_score=sum(sum_sq_score),
                                         sum_order=sum(sum_order), sum_sq_order=sum(sum_sq_order)),
                                   by=list(artist, uni_profile, sex, age)]
    
  })
}
save(dt.pllst.whole_stat, file="outdata/dt.pllst.whole_stat.new.RData")

#

str(dt.pllst.whole_stat)

dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\s{2,}", " ")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "^\\s+", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "-+\\s*$", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\s+$", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "ё", "е")

#dt.pllst.whole_stat <- dt.pllst.whole_stat[,list(artist_count=sum(artist_count), uid_count=sum(uid_count),
#                                                 sum_score=sum(sum_score), sum_sq_score=sum(sum_sq_score)),
#                                           by=list(artist)]

dt.pllst.whole_stat <- dt.pllst.whole_stat[order(dt.pllst.whole_stat$artist_count,decreasing = T),]

dt.pllst.whole_stat$mean_score <- dt.pllst.whole_stat$sum_score / dt.pllst.whole_stat$artist_count
dt.pllst.whole_stat$sd <- sqrt( (dt.pllst.whole_stat$sum_sq_score / dt.pllst.whole_stat$artist_count) - dt.pllst.whole_stat$mean_score^2 )
dt.pllst.whole_stat$mean_order <- dt.pllst.whole_stat$sum_order / dt.pllst.whole_stat$artist_count
dt.pllst.whole_stat$sd_order <- sqrt( (dt.pllst.whole_stat$sum_sq_order / dt.pllst.whole_stat$artist_count) - dt.pllst.whole_stat$mean_order^2 )

#
#dt.pllst.whole_stat
#dt.pllst.whole_stat.sub <- dt.pllst.whole_stat[dt.pllst.whole_stat$artist_count>=250,]
dt.pllst.whole_stat.techniqs <- dt.pllst.whole_stat[dt.pllst.whole_stat$uni_profile=="технический"]



dt.pllst.whole_stat.sub <- dt.pllst.whole_stat.techniqs
dt.pllst.whole_stat.sub <- dt.pllst.whole_stat.sub[dt.pllst.whole_stat.sub$artist_count>=50,]
dt.pllst.whole_stat.sub <- dt.pllst.whole_stat.sub[order(mean_order,decreasing = F)]




#
dt.pllst.whole_stat.sub_rus <- dt.pllst.whole_stat[which(str_detect(dt.pllst.whole_stat$artist, pattern = "^[а-я]"))]
dt.pllst.whole_stat.sub_rus <- dt.pllst.whole_stat.sub_rus[1:10000,]
dt.pllst.whole_stat.sub_rus <- dt.pllst.whole_stat.sub_rus[order(mean_score,decreasing = T)]

#============================ Get counts of user in each group =========================================

#TODO: plot distr of playlist length:

uid_con <- dbConnect(SQLite(), dbname="data/uids.db")
dt.user_count.all <- data.table()
db_filenames <- list.files("C:/Databases/VK_data/",pattern = "*.db")
for (db_filename in db_filenames) {
  try({
    uni_id <- as.numeric(str_extract(db_filename, "\\d+"))
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
    
    df.pllst <- df.pllst[,c("uid", "artist")]
    dt.pllst <- data.table(df.pllst)
    dt.user_count <- dt.pllst[,list(artist_count=length(artist)),
                              by=list(uid)]
    dt.user_count <- merge(dt.user_count, dt.uids, by="uid", all.x=T, all.y=F)
    dbDisconnect(pllst_con)
    dt.user_count.all <- rbind(dt.user_count.all, dt.user_count)
  })
}


#save(dt.user_count.all, file="outdata/dt.user_count.all")
load("outdata/dt.user_count.all")

dt.user_count.stat <- dt.user_count.all[,list(uid_count=length(uid)), by=list(uni_id, age, sex)]
