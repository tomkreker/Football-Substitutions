# This scripts processes the original dataset of 900k match events as downloaded from
# https://www.kaggle.com/secareanualin/football-events
# So only substitutions and goals, with their relevant columns, are left.
# This allows to store the data file on github, which is otherwisie 200mb.
# The original data file is refered to as 'events.csv'.

library(stringr)
foo = read.csv('events.csv') #import data

subs = foo[which(foo$event_type==7),]
summary(is.na(subs)) #check for NAs
remove = which(subs$id_odsp %in% subs[which(is.na(subs$player_in)),'id_odsp']) #matches with faulty data
subs = subs[-remove,] #remove observations with NAs
allevents = foo[which(foo$id_odsp %in% subs$id_odsp),]

inj = str_detect(allevents$text, 'injury') #this only happens in a sub due to injury
matches_with_injuries = allevents$id_odsp[inj] #find all matches in which injury happened
allevents = allevents[-which(allevents$id_odsp %in% matches_with_injuries),] #remove these matches

reds = which(allevents$event_type==5 | allevents$event_type==6) #find all matches with a red card
matches_with_reds = allevents$id_odsp[reds]
allevents = allevents[-which(allevents$id_odsp %in% matches_with_reds),] #remove these matches
allevents = allevents[which(allevents$event_type==7 | allevents$is_goal==1),] #only consider goals and subs

sum(allevents$event_type==7) #final num of subs in sample
length(unique(allevents$id_odsp)) #final num of matches in sample - 4346
allevents = allevents[,c(1,4,6,8,17)]

write.csv(allevents,'data.csv')
