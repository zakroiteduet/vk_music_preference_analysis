#---------------------------------
#             Genres:
#---------------------------------
library("ggplot2")

dt.genres <- read.table("data/genres.final.short.txt", sep=";", header=F)
colnames(dt.genres) <- c("artist_num", "genre")
dt.genres <- data.table(dt.genres)
dt.genres$genre <- tolower(dt.genres$genre)
dt.genres[,genre_count:=.N, by=list(genre)]

dt.genres.counts <- dt.genres[,list(genre_count=.N), by=list(genre)]
ggplot(dt.genres.counts, aes(x=genre_count))+geom_histogram(binwidth=1)+xlim(c(0,100))
dt.genres <- dt.genres[genre_count>=20]

dt.genres.counts <- dt.genres.counts[order(genre_count, decreasing = T)]


load("outdata/dt.pllst.whole_stat.sub_250.RData")
dt.pllst.whole_stat.sub$artist_num <- seq(0, dim(dt.pllst.whole_stat.sub)[1] - 1)
dt.pllst.whole_stat.sub <- merge(dt.pllst.whole_stat.sub, dt.genres,
                                 by="artist_num", all=F)

#save(dt.pllst.whole_stat.sub, file = "outdata/dt.pllst.whole_stat.sub_250.with_genre_info.RData")

#-------------------------------------------------------------------
load("outdata/dt.pllst_stat.all.RData")

dt.pllst_stat.all <- merge(dt.pllst_stat.all, )

dt.pllst.whole_stat.sub
