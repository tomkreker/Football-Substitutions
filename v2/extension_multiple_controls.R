###
####
###

# This scripts takes a different approach at obtaining the treatment effect by generating
# 'control units' for each performed substitution, using matches that had one less sub than
# the considered match (so for the 3rd sub, find a match that had 2 subs, and the minute
# at which the team COULD HAVE made the 3rd but did not). THEN, after each sub has a control
# sub, obtain the estimated treatment effect of each sub, and based on THAT try to derive the
# minutes of ideal subs and in turn a decision rule. The splits from these trees should make the
# idea decision rule.

# 15/3 - I need to re-run this code and see where it stands, this was left semi-working
# In any case, this is also secondary to the traversal of the rule space derived through
# the main method, which has higher priority because it could lead to a well-formed paper,
# this could be an extension or validation.

###
####
###


# Generating counterfactuals for all individual subs
matchinfo = read.csv('ginf.csv')


#Group matches by number of subs

# first, two random games for testing
#random_matches = c(as.character(allevents[1000,'id_odsp']), as.character(allevents[15,'id_odsp']))
# later - use training set
random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp))/2) #select half the eliginle match on random

train_set = allevents[which(allevents$id_odsp %in% random_matches),]
sum(train_set$event_type==7) #final num of subs in sample
train_set$id_odsp = as.character(train_set$id_odsp)

get_numsubs = function(train_set){
  #input - training set events
  #data frame with results: game_id, side, nsubs
  id = c()
  side = c()
  num_subs = c()
  
  for (g in unique(train_set$id_odsp)){
    game = train_set[which(train_set$id_odsp==g),]
    num_hsubs = sum(game$event_type==7 & game$side==1)
    num_asubs = sum(game$event_type==7 & game$side==2)
    id = c(id, as.character(g), as.character(g))
    side = c(side,1,2)
    num_subs = c(num_subs, num_hsubs, num_asubs)
  }
  
  numsubs = data.frame(id_odsp=id,side=side,num_subs=num_subs)
  return(numsubs)
}

num_subs = get_numsubs(train_set) #run this next then gen controls

sum(num_subs$num_subs==2)
sum(get_numsubs(allevents)$num_subs==0)


n=2
generate_controls = function(n,num_subs,matchinfo){
  pool = num_subs[which(num_subs$num_subs==n-1) ,]
  rows = list()
  rows_i = 1
  for (i in 1:nrow(pool)){ #iterate over each game with the relevant sub number
    game = allevents[which(as.character(allevents$id_odsp)==pool[i,'id_odsp']),]
    id = as.character(game[1,'id_odsp'])
    sub_times = game[which(game$event_type==7 & game$side==pool[i,'side']),'time']
    if (pool[i,'side']==1){ #if our unit is a home team
      scoreline = htimes_behind(game)#get hscore at every minute
      behind = which(scoreline<0)
      after_sub = behind[which(behind > sub_times[n-1] & behind<86)] #these are the relevant times
      for (t in after_sub){
        #generate a control unit - a time in which the team was behind and could have
        #used its next substitution, but it didn't
        #id, side, #sub, time, score_at_time, tb, min, treat(0),outcome,bin_out
        tb = sum(scoreline[1:t]<0)
        outcome = scoreline[100] - scoreline[t]
        bin_out = as.numeric(outcome>0)
        rows[[rows_i]] = c(id,pool[i,'side'],n,t, scoreline[t],tb,0,outcome,bin_out)
        rows_i = rows_i + 1
      }
    }
    if (pool[i,'side']==2){ #if our unit is an away team
      scoreline = htimes_behind(game)#get hscore at every minute
      behind = which(scoreline>0)
      after_sub = behind[which(behind > sub_times[n-1] & behind<86)] #these are the relevant times
      for (t in after_sub){
        tb = sum(scoreline[1:t]>0)
        outcome = scoreline[100]*-1 - scoreline[t]*-1
        bin_out = as.numeric(outcome>0)
        rows[[rows_i]] = c(id,pool[i,'side'],n,t, scoreline[t]*-1,tb,0,outcome,bin_out)
        rows_i = rows_i + 1
      }
    }
  }
  df = do.call("rbind", rows)
  cols = c('id_odsp','side','sub','time', 'score_at_time', 'tb', 'treat','outcome','bin_out')
  df = as.data.frame(df, stringsAsFactors=F)
  names(df) = cols
  
  ###
  #### Part 2: Fetch game info
  ###
  games = matchinfo[,c(1,5,6,7,12:14)]
  games = games[which(games$id_odsp %in% df$id_odsp),]
  games$id_odsp = as.character(games$id_odsp)
  odds = c()
  odds2 = c()
  for (i in 1:nrow(df)){
    odds_diff_h =  games[which(games$id_odsp==df[i,'id_odsp']),'odd_h'] - 
      games[which(games$id_odsp==df[i,'id_odsp']),'odd_a']
    if (df[i,'side']==1){
      odds = c(odds, odds_diff_h)
      odds2 = c(odds2, sign(odds_diff_h)*odds_diff_h^2)}
    else{
      odds = c(odds, -1*odds_diff_h)
      odds2 = c(odds2, sign(-1*odds_diff_h)*odds_diff_h^2)}
  }
  df$odds = odds
  df$odds2 = odds2
  
  return(df)
}

controls_for_2 = generate_controls(2,num_subs,matchinfo) 
head(controls_for_2,40)
controls_for_2

controls_for_3 = generate_controls(3,num_subs,matchinfo) 
head(controls_for_3,10)


#next - generate matching treatment units.

generate_treatment = function(n, num_subs, matchinfo){
  pool = num_subs[which(num_subs$num_subs==n) ,]
  rows = list()
  rows_i = 1
  for (i in 1:nrow(pool)){ #iterate over each game with the relevant sub number
    game = allevents[which(as.character(allevents$id_odsp)==pool[i,'id_odsp']),]
    id = as.character(game[1,'id_odsp'])
    t = game[which(game$event_type==7 & game$side==pool[i,'side']),'time'][n]
    scoreline = htimes_behind(game)#get hscore at every minute
    if (pool[i,'side']==1){#home candidate
      if(scoreline[t]<0){ #team was trailing - relevant 'treatment' sub
        tb = sum(scoreline[1:t]<0) #for home
        outcome = scoreline[100] - scoreline[t]
        bin_out = as.numeric(outcome>0)
        rows[[rows_i]] = c(id,pool[i,'side'],n,t, scoreline[t],tb,1,outcome,bin_out)
        rows_i = rows_i + 1
      } 
    }
    if (pool[i,'side']==2){#away candidate
      if(scoreline[t]>0){ #team was trailing - relevant 'treatment' sub
        tb = sum(scoreline[1:t]>0) #for away
        outcome = scoreline[100]*-1 - scoreline[t]*-1
        bin_out = as.numeric(outcome>0)
        rows[[rows_i]] = c(id,pool[i,'side'],n,t, scoreline[t]*-1,tb,1,outcome,bin_out)
        rows_i = rows_i + 1
      } 
    }
    
  }
  df = do.call("rbind", rows)
  cols = c('id_odsp','side','sub','time', 'score_at_time', 'tb', 'treat','outcome','bin_out')
  df = as.data.frame(df, stringsAsFactors = F)
  names(df) = cols
  
  ###
  #### Part 2: Fetch game info
  ###
  games = matchinfo[,c(1,5,6,7,12:14)]
  games = games[which(games$id_odsp %in% df$id_odsp),]
  games$id_odsp = as.character(games$id_odsp)
  odds = c()
  odds2 = c()
  for (i in 1:nrow(df)){
    odds_diff_h =  games[which(games$id_odsp==df[i,'id_odsp']),'odd_h'] - 
      games[which(games$id_odsp==df[i,'id_odsp']),'odd_a']
    if (df[i,'side']==1){
      odds = c(odds, odds_diff_h)
      odds2 = c(odds2, sign(odds_diff_h)*odds_diff_h^2)}
    else{
      odds = c(odds, -1*odds_diff_h)
      odds2 = c(odds2, sign(-1*odds_diff_h)*odds_diff_h^2)}
  }
  df$odds = odds
  df$odds2 = odds2
  return(df)
}

treatment_for_2 = generate_treatment(2,num_subs, matchinfo)
treatment_for_3 = generate_treatment(3,num_subs, matchinfo)

second_sub = rbind(treatment_for_2,controls_for_2)
third_sub = rbind(treatment_for_3,controls_for_3)

for (col in names(second_sub)){
  print(col)
  if (col!='id_odsp'){
    second_sub[,col] = as.numeric(second_sub[,col])
  }
}

for (col in names(third_sub)){
  print(col)
  if (col!='id_odsp'){
    third_sub[,col] = as.numeric(third_sub[,col])
  }
}


##
##
## Genetic Matching
##
##
library(Matching)
X = cbind(second_sub$time, second_sub$score_at_time, second_sub$tb, second_sub$odds,
          second_sub$odds2)
          
X3 = cbind(third_sub$time, third_sub$score_at_time, third_sub$tb, third_sub$odds,
          third_sub$odds2)

caliper = c(0.8,0.8,0.8,0.8,100) 

genout = GenMatch(Tr=second_sub$treat,X=X,caliper = caliper,
                  pop.size = 600, wait.generations = 10) #run genetic matching

genout3 = GenMatch(Tr=third_sub$treat,X=X3,caliper = caliper,
                  pop.size = 600, wait.generations = 10) #run genetic matching


mout = Match(Y = second_sub$bin_out, Tr=second_sub$treat,X=X,caliper = caliper
             ,Weight.matrix = genout)

mout3 = Match(Y = third_sub$bin_out, Tr=third_sub$treat,X=X3,caliper=caliper
             ,Weight.matrix = genout3)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treat~time+score_at_time+tb+odds+odds2,data=second_sub,
             match.out = mout) 
MatchBalance(treat~time+score_at_time+tb+odds+odds2,data=third_sub,
             match.out = mout3) 

#
##
#
summary(mout) #examine number of followers and defiers remained
summary(mout3) #examine number of followers and defiers remained


mean(second_sub[mout$index.treated,'bin_out']) # % success in matched follower
mean(second_sub[mout$index.control,'bin_out']) # % success in matched defiers 
summary(second_sub$time)


sub_before_t = c()
times = min(mout$mdata$X[,1]):max(mout$mdata$X[,1])
for (t in times){ #earliest to latest sub considered
  treat = mout$mdata$Y[which(mout$mdata$Tr==1 & mout$mdata$X[,1]<t)]
  ctrl =  mout$mdata$Y[which(mout$mdata$Tr==1 & mout$mdata$X[,1]<t)+mout$wnobs] #matched units
  if (length(treat) > mout$wnobs/4){
    sub_before_t = c(sub_before_t,  mean(treat) - mean(ctrl))
  }
  else{
    sub_before_t = c(sub_before_t,  0)
  }
}

plot(times[which(sub_before_t>0)], sub_before_t[which(sub_before_t>0)], pch=16)

#another approach - fit trees on per-unit sub outcome, i.e. match outcome - counterfactual outcome
mout_nonbin = Match(Y = second_sub$outcome, Tr=second_sub$treat,X=X,caliper = caliper
                    ,Weight.matrix = genout)
unitwise = mout_nonbin$mdata$Y[which(mout_nonbin$mdata$Tr==1)] - mout_nonbin$mdata$Y[which(mout_nonbin$mdata$Tr==0)]
unitwise_df = data.frame(time=mout_nonbin$mdata$X[1:138,1], outcome=as.numeric(unitwise>0))
tree2_match = rpart(outcome~time,data=unitwise_df, method='anova', control=rpart.control(minbucket = round(138/4))) 
#best first split that ensures at least 1/4 of the data is in each group  (prevent 10 before, 120 after the time)
print(tree2_match)


#code to view the subs and goals of a given game
#allevents[which(allevents$id_odsp=='M3FzCtd9/'),]
