rm(list=ls())
setwd("C:/Source code/vk_music_preference_analysis/")
load("outdata/dt.user_count.all")
load("outdata/uni.data.Rdata")
library("ggplot2")
library("data.table")

setkey(dt.user_count.all, uid)
dt.user_count.all <- unique(dt.user_count.all)
dt.user_count.all <- dt.user_count.all[artist_count>=10]
dt.user_count.all <- dt.user_count.all[artist_count<1500]
dt.user_count.all <- dt.user_count.all[uni_id != 850]

### age + sex total num of users
dt.user_count.all[,list(uid_num=length(uid)),
                  by=list(age,sex)]

dt.user_count.all[,uid_num:=length(uid),
                  by=list(age,sex)]

### all users altogether
dt.audio_count_hist <- dt.user_count.all[,list(uid_count=length(uid)),
                                    by=list(artist_count)]

ggplot(dt.audio_count_hist,aes(x=artist_count, y=uid_count))+
  geom_point(color="red")+theme_bw()
ggplot(dt.audio_count_hist,aes(x=artist_count, y=uid_count))+
  geom_point(color="red")+scale_x_log10()+scale_y_log10()+theme_bw()
ggplot(dt.audio_count_hist,aes(x=artist_count, y=uid_count))+
  geom_point(color="red")+scale_y_log10()+theme_bw()

### all users separetely by age and sex
dt.audio_count_hist.sep <- dt.user_count.all[,list(uid_count=length(uid)),
                                         by=list(artist_count,sex,age,uid_num)]
ggplot(dt.audio_count_hist.sep,aes(x=artist_count, y=uid_count/uid_num)) +
  geom_point(color="red")+scale_y_log10()+theme_bw()+facet_grid(sex~age)

ggplot(dt.audio_count_hist.sep,aes(x=artist_count, y=uid_count/uid_num, color=factor(age)))+
  geom_point()+scale_y_log10()+theme_bw()

##############################################################################
############### Unified State Exam
ggplot(uni.data, aes(x=mean_ege_score))+geom_histogram(binwidth=2)+theme_bw()
dt.user_count.all$mean_ege_score <- uni.data$mean_ege_score[match(dt.user_count.all$uni_id, uni.data$uni_id)]
dt.user_count.all$uni_profile <- uni.data$uni_profile[match(dt.user_count.all$uni_id, uni.data$uni_id)]
dt.user_count.all <- dt.user_count.all[!is.na(uni_profile)]
ggplot(dt.user_count.all, aes(x=uni_profile,fill=factor(sex)))+geom_histogram(position="fill")+theme_bw()+
  scale_fill_brewer(palette="Set1")




