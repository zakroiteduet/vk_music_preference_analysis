rm(list=ls())
setwd("C:/Source code/vk_music_preference_analysis/")

admission.data <- read.csv("data/admission_quality_hse_2014.csv", header = T, sep = ";", stringsAsFactors = F)
admission.data <- admission.data[,c(1,2,4,12)]
names(admission.data) <- c("uni", "uni_profile", "mean_ege_score", "mean_score")

uni.data <- read.csv("data/universities_final.csv", header = F, sep = ";", stringsAsFactors = F)
names(uni.data) <- c("uni", "uni_id", "region", "city", "city_id")

uni.data <- merge(uni.data, admission.data, by=c("uni"),
                  all.x=T, all.y=F)
uni.data[is.na(uni.data$mean_score),]

save(uni.data, file="outdata/uni.data.Rdata")
