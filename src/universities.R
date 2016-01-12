library("stringr")

data.uni <- read.csv("C:/Users/Lynx/Documents/universities.csv", sep = ";", header = F, stringsAsFactors=F)
names(data.uni) <- c("uni", "region")
data.uni$city <- str_extract(data.uni$uni, pattern = perl("(?<=г\\.).+"))
data.uni$city <- str_replace(data.uni$city, perl("\\s"), "")
data.uni$city[is.na(data.uni$city)] <- str_extract(data.uni$uni[is.na(data.uni$city)],
                                                   perl("\\w+(?=ск[ийаяой])"))
#write.table(data.uni, file = "C:/Users/Lynx/Documents/universities_modif.csv",
#            quote = F, sep = ";",col.names = F, row.names = F)


#write.table(unique(data.uni$city), file = "C:/Users/Lynx/Documents/cities.list",
#            quote = F, sep = ";",col.names = F, row.names = F)

data.cities <- read.csv("C:/Users/Lynx/Documents/city_ids.list", sep = ";", header = F, stringsAsFactors=F)
names(data.cities) <- c("city_id", "city_vk", "city")

data.uni$city_id <- data.cities$city_id[match(data.uni$city, data.cities$city)]

write.table(data.uni, file = "C:/Users/Lynx/Documents/universities_modif.csv",
            quote = F, sep = ";",col.names = F, row.names = F)



#=====  new  =====

data.uni <- read.csv("C:/Users/Lynx/Documents/universities_modif.csv", sep = ";", header = F, stringsAsFactors=F)
names(data.uni) <- c("uni", "region", "city", "city_id")

data.uni.ids <- read.csv("C:/Users/Lynx/Documents/uni_ids.csv", sep = ";", header = T, stringsAsFactors=F)
names(data.uni.ids) <- c("uni", "city_id", "uni_id")
data.uni.ids <- data.uni.ids[,c(1,2,3)]

unis <- data.uni$uni
data.uni.res <- data.frame()
uni_num <- length(unis)
for (i in c(1:uni_num)) {
  uni_ids <- as.numeric(str_split(data.uni.ids$uni_id[i],";")[[1]])
  temp <- data.frame(uni=data.uni.ids$uni[i], uni_id=uni_ids)
  data.uni.res <- rbind(data.uni.res, temp)
}

data.uni.res <- merge(data.uni.res, data.uni, by="uni", all=T)

write.table(data.uni.res, file = "C:/Users/Lynx/Documents/universities_final.csv",
            quote = F, sep = ";",col.names = F, row.names = F)
