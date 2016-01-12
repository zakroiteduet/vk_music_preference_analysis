setwd("C:/Source code/vk_music_preference_analysis/")

load("outdata/dt.user_count.all")
dt.user_count.all[,uid_count:=length(uni_id), by=list(uid)]
dt.user_count.all <- dt.user_count.all[uid_count == 1]
dt.user_count.all <- dt.user_count.all[artist_count >= 20]
uids <- dt.user_count.all$uid

dt.user_count.per_uni <- dt.user_count.all[,list(uid_count=length(uid)),
                                           by=list(uni_id)]
uni_ids <- dt.user_count.per_uni[uid_count >= 400]$uni_id


load("outdata/dt.pllst.whole_stat.sub_250.RData")
artists <- dt.pllst.whole_stat.sub$artist


dt.pllst_stat.all_females <- data.table()
dt.pllst_stat.all_males <- data.table()

for (uni_id in uni_ids) {
  try({
    print(uni_id)
    dt.pllst_stat <- get_playlist_detail_stat(uni_id, uids, artists)
    dt.pllst_stat <- dt.pllst_stat[,list(artist_count=sum(artist_count)),
                                   by=list(uni_id, sex, artist)]
    dt.pllst_stat <- dt.pllst_stat[order(artist_count, decreasing = T)]
    dt.pllst_stat.all_females <- rbind(dt.pllst_stat.all_females, dt.pllst_stat[sex==1][1:2000])
    dt.pllst_stat.all_males <- rbind(dt.pllst_stat.all_males, dt.pllst_stat[sex==2][1:2000])
    
    outfname <- paste("outdata/pllst_counts/", uni_id, ".RData", sep="")
    save(dt.pllst_stat, file=outfname)
  })
}


# save(dt.pllst_stat.all_males, file="outdata/dt.pllst_stat.all_males.RData")
# save(dt.pllst_stat.all_females, file="outdata/dt.pllst_stat.all_females.RData")

#-------------------------------------------------------------------
load("outdata/dt.pllst_stat.all_males.RData")
load("outdata/dt.pllst_stat.all_females.RData")
dt.user_count.per_uni_sex <- dt.user_count.all[,list(uid_count=length(uid)),
                                           by=list(uni_id, sex)]
dt.user_count.per_uni_sex$uni_id_sex <- paste(dt.user_count.per_uni_sex$uni_id,
                                              dt.user_count.per_uni_sex$sex, sep = "_")
dt.pllst_stat.all <- rbind(dt.pllst_stat.all_females, dt.pllst_stat.all_males)
dt.pllst_stat.all <- merge(dt.pllst_stat.all, dt.user_count.per_uni_sex,
                           by=c("sex", "uni_id"), all=F)

dt.pllst_stat.all$artist_id <- as.numeric(as.factor(dt.pllst_stat.all$artist))
dt.pllst_stat.all$artist_raw_frac <- dt.pllst_stat.all$artist_count / dt.pllst_stat.all$uid_count
dt.pllst_stat.all[, artist_occurence:=length(uni_id), by=list(artist)]
dt.pllst_stat.all <- dt.pllst_stat.all[artist_occurence>=10,]
save(dt.pllst_stat.all, file="outdata/dt.pllst_stat.all.RData")

artist_matrix <- as.matrix(cast(dt.pllst_stat.all, uni_id_sex ~ artist_id,
                           value="artist_raw_frac", fill = 0))


dm.manhtattan <- dist(artist_matrix, method = "Manhattan")

library("MASS")
mds.manhattan <- isoMDS(dm.manhtattan, k = 2)
df.mds.manhattan <- as.data.frame(mds.manhattan$points)
df.mds.manhattan$uni_id_sex <- rownames(mds.manhattan$points)
dt.mds.manhattan <- data.table(df.mds.manhattan)
dt.mds.manhattan$uni_id <- as.numeric(str_extract(dt.mds.manhattan$uni_id_sex, pattern = "^\\d+"))
dt.mds.manhattan$sex <- as.numeric(str_extract(dt.mds.manhattan$uni_id_sex, pattern = "\\d+$"))



load("outdata/uni.data.Rdata")

uni.data <- data.table(uni.data)
dt.mds.manhattan <- merge(dt.mds.manhattan, uni.data, by="uni_id", all=F)

ggplot(dt.mds.manhattan, aes(x=V1, y=V2,
                             color=mean_ege_score,
                             shape=factor(sex)))+
  geom_text(aes(label=uni), size=2)+
  geom_point(size=5)+scale_colour_gradientn(colours = rainbow(7))+
  theme_bw()

#------------------- Get Coordinates in genre axes ----------------------
dt.artis_ids <- dt.pllst_stat.all[,list(artist_id=unique(artist_id)),
                                  by=list(artist)]

first_artist <- "оргия праведников"
second_artist <- "аукцыон"

first_artist_id <- as.character(dt.artis_ids[dt.artis_ids$artist == first_artist]$artist_id)
second_artist_id <- as.character(dt.artis_ids[dt.artis_ids$artist == second_artist]$artist_id)

dt.two_artists <- data.table(uni_id_sex=rownames(artist_matrix),
                             first_artist=artist_matrix[,first_artist_id],
                             second_artist=artist_matrix[,second_artist_id])
dt.two_artists$uni_id <- as.numeric(str_extract(dt.two_artists$uni_id_sex, pattern = "^\\d+"))
dt.two_artists$sex <- as.numeric(str_extract(dt.two_artists$uni_id_sex, pattern = "\\d+$"))
dt.two_artists <- merge(dt.two_artists, uni.data, by="uni_id", all=F)

ggplot(dt.two_artists, aes(x=first_artist, y=second_artist,
                             color=mean_ege_score,
                             shape=factor(sex)))+
  geom_text(aes(label=uni), size=2)+
  geom_point(size=5)+scale_colour_gradientn(colours = rainbow(7))+
  theme_bw()



# get_artist_2d <- function(data, first_artist, second_artist) 
# 
# 
# dm.canberra <- dist(artist_matrix, method = "Canberra")
# 
# library("proxy")
# dt.pllst_stat.all_males$rank <- rep(x = c(1:2000), 415)
# dt.pllst_stat.all_females$rank <- rep(x = c(1:2000), 415)
# artist_matrix.male <- cast(dt.pllst_stat.all_males, uni_id ~ rank,
#                            value="artist")
# artist_matrix.male <- as.matrix(artist_matrix.male)
# jacc_f <- function(x,y) {
#   return(length(intersect(x,y))/ length(x))
# }
# 
# a <- dist(artist_matrix.male, jacc_f)
