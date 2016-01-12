library("RSQLite")
library("stringr")
library("data.table")

setwd("C:/Source code/vk_music_preference_analysis/")
load("outdata/uni.data.Rdata")

#=============================
load("outdata/dt.pllst.whole_stat.RData", verbose=T)
#
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\s{2,}", " ")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "-+\\s*$", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "ё", "е")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\sfeat[\\.\\s].+", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\sft[\\.\\s].+", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\(.+\\)", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\[.+\\]", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "\\s+$", "")
dt.pllst.whole_stat$artist <- str_replace(dt.pllst.whole_stat$artist, pattern = "^\\s+", "")

dt.pllst.whole_stat <- dt.pllst.whole_stat[, list(artist_count = sum(artist_count)),
                                           by=list(artist)]
dt.pllst.whole_stat$nchar <- nchar(dt.pllst.whole_stat$artist)
dt.pllst.whole_stat <- dt.pllst.whole_stat[nchar >= 3 & nchar <= 50]
dt.pllst.whole_stat.sub <- dt.pllst.whole_stat[dt.pllst.whole_stat$artist_count>250,]
save(dt.pllst.whole_stat.sub, file = "outdata/dt.pllst.whole_stat.sub_250.RData")

write.table(dt.pllst.whole_stat.sub$artist, file = "outdata/artist_list.txt",
            sep = "\t", row.names = F, col.names = F, quote = F)



