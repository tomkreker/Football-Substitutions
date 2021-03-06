library(stringr)
library(tree)
library(Matching)

##
###
#### Clean data: remove NAs and games with injuries and red cards (see preprocessing.R)
###
##

allevents = read.csv('data.csv') #import data pre-processed in preprocessing.R

##
###
#### Create training set based on a random sample of half of the eligible matches
###
##

random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp))/2) #select half the eliginle match on random
train_set = allevents[which(allevents$id_odsp %in% random_matches),]
sum(train_set$event_type==7) #final num of subs in sample

BuildTraining = function(df){ 
#Input: dataframe with events of substitutions to be used for fitting trees to find decision points  
#Output: added variables with the number of sub, score at sub,if improved goal differential at the end
  
  numsub=c() #length of subs
  goaldifsub=c() #length of subs
  home_final_scores=rep(NA,length(unique(df$id_odsp))) #length of matches
  names(home_final_scores) = (unique(df$id_odsp))
  hscore=0
  ascore=0
  asub=1
  hsub=1 
  #the first row in train_set is a home substitution
  numsub = c(numsub,hsub)
  goaldifsub = c(goaldifsub,hscore-ascore)
  hsub=2
  for (i in (2:nrow(df))){ #now for every other row
    if (df[i,'id_odsp']!=df[i-1,'id_odsp']){ #a new game has started
      name=(df[i-1,'id_odsp'])
      home_final_scores[name] =  hscore-ascore
      hscore=0
      ascore=0
      asub=1
      hsub=1
    }
    
    if(df[i,'event_type']==7){ #if sub
      if (df[i,'side']==1){
        numsub = c(numsub,hsub)
        goaldifsub = c(goaldifsub,hscore-ascore)
        hsub=hsub+1
        }
      else{
        numsub = c(numsub,asub)
        goaldifsub = c(goaldifsub,ascore-hscore)
        asub=asub+1
        }
    }
    if(df[i,'is_goal']==1){ #if goal
      if (df[i,'side']==1){hscore = hscore+1}
      else{ascore = ascore+1}
    }
    
  }
  name=(df[i,'id_odsp'])
  home_final_scores[name] =  hscore-ascore #update final score for the final match
  
  subs = df[which(df$event_type==7),]
  subs$numsub = numsub
  subs$goaldifsub= goaldifsub
  goaldiffin= rep(0,nrow(subs))
  for (i in 1:nrow(subs)){
    name = (subs[i,'id_odsp'])
    goaldiffin[i]=home_final_scores[name]
    if (subs[i,'side']==2){#if it is an away sub, switch signs of the score
      goaldiffin[i] = goaldiffin[i]*(-1)
    }
  }
  subs$goaldiffin=goaldiffin
  subs$goal_diff_improve = subs$goaldiffin - subs$goaldifsub
  goal_diff_improve01 = rep(0,nrow(subs))
  goal_diff_improve01[which(subs$goal_diff_improve>0)]=1 #binary outcome as in Myers
  subs$goal_diff_improve01 = goal_diff_improve01
  
  return (subs)
}

#run function on the training sample matches to get the subs with the required variables
train_with_variables =  BuildTraining(train_set)
remove = which(train_with_variables$id_odsp %in% train_with_variables[which(train_with_variables$numsub>3),'id_odsp']) #few matches with faulty data
train_with_variables = train_with_variables[-remove,] 
train_with_variables$goal_diff_improve01 = as.factor(train_with_variables$goal_diff_improve01)

#create subsets to fit trees per substitute category
train_behind = train_with_variables[which(train_with_variables$goaldifsub<0),] #only games behind - ~4500 subs
first_train_behind = train_behind[which(train_behind$numsub==1),] #1st subs
mean(as.numeric((first_train_behind$goal_diff_improve01))-1) #average success

second_train_behind =train_behind[which(train_behind$numsub==2),] #2nd subs
mean(as.numeric((second_train_behind$goal_diff_improve01))-1) #average success

third_train_behind = train_behind[which(train_behind$numsub==3),] #3rd subs
mean(as.numeric((third_train_behind$goal_diff_improve01))-1) #average success

before_after_minute = function(df){ 
#Auxiliary functions to 'mimic' tree activity for the first split (as explained in Appendix A)
#And obtain p-values provided in the Table 1
#Input: training dataset.
#Output: List of all differences in mean outcomes before and after each given minute
  best=0
  minute=0
  times = c()
  diffs = c()
  bef = c()
  aft = c()
  for(m in min(df$time):max(df$time)){
    bef = c(bef, nrow(df[which(df$time<m+0.5),]))
    aft = c(aft, nrow(df[which(df$time>m+0.5),]))
    prior = mean(as.numeric(df[which(df$time<(m+0.5)),'goal_diff_improve01']))
    post = mean(as.numeric(df[which(df$time>(m+0.5)),'goal_diff_improve01']))
    dif = prior-post
    if (is.na(post)){dif=prior}
    if (is.na(prior)){dif=post}
    
    times = c(times,m+0.5)
    diffs=c(diffs,dif)
    if (dif > best){
      best=prior
      minute=m
    }
  }
  return(data.frame(time=times,difference=diffs,n_before=bef,n_after=aft))
}

pvalue = function(df,m){  
# perform t-test between the before-after groups for Table 1
  cat('subs before',m,'minute:',length(df[which(df$time<m),'goal_diff_improve01']))
  cat('\nsubs after',m,'minute:',length(df[which(df$time>m),'goal_diff_improve01']))
  t.test(as.numeric(df[which(df$time<m),'goal_diff_improve01'])-1,  
         y = as.numeric(df[which(df$time>m),'goal_diff_improve01'])-1, 
         alternative = "greater", mu = 0)
}

##
###
#### Fit trees to training set to find potential decision rule
###
##

library(tree)
tree1 = tree(goal_diff_improve01~time,data=first_train_behind)
tree1
mimic = before_after_minute(first_train_behind)
round(mimic,3)
#
## Figure in Appendix A
#
plot(mimic$time[6:66],mimic$difference[6:66], xlab='Minute',ylab ='Before-After Mean Success Difference',
     main='First Sub Mean Before-After Success by Minute',pch=16)
abline(v=64.5,lty=2)
#text('64.5: largest before-after difference',x=50,y=-0.06)
pvalue(first_train_behind,64.5) #p-value is 0.0001424
#
##
#

tree2 = tree(goal_diff_improve01~time,data=second_train_behind)
tree2 #obtain before and after proportions
pvalue(second_train_behind,71.5) #p=3.117e-07

tree3 = tree(goal_diff_improve01~time,data=third_train_behind)
tree3 #obtain before and after proportions
pvalue(third_train_behind,77.5) #p=6.54e-08



##
###
#### Testing the rule on the other half of the data: unused matches
###
##

testevents = allevents[-which(allevents$id_odsp %in% random_matches),] #the other 2173 matches unused for training

Build_test_set = function(df,t1,t2,t3){
#Input: dataframe with match events unused in building the rule, the three decision points of the rule
#Output: A dataframe where each row is a match situation in which a team could (defier) or did (followers)
#follow the rule, including various in-game (part 1) and pre-game (part 2) covariates and the outcome for that team
  
  ###
  #### Part 1: build in-game covariates and outcome
  ###
  allgames = unique(df$id_odsp)
  could = c() #matches that had a chance to follow
  side = c() #what side could follow
  when_follow = c() #what was the point out ot t1/2/3 from which the team could(neg)/did(pos) follow
  score_when_could = c() #what was the score during the chance point
  time_behind = c() #how long has the team at chance been behind
  outcome = c() #goal difference from when chance point to end of match
  
  for (g in allgames){
    game = df[which(df$id_odsp==g),]
    
    hgoals_t1 = sum(game$is_goal==1 & game$time<t1 & game$side==1) -
      sum(game$is_goal==1 & game$time<t1 & game$side==2)
    hgoals_t2 = sum(game$is_goal==1 & game$time<t2 & game$side==1) -
      sum(game$is_goal==1 & game$time<t2 & game$side==2)
    hgoals_t3 = sum(game$is_goal==1 & game$time<t3 & game$side==1) -
      sum(game$is_goal==1 & game$time<t3 & game$side==2)
    hgoals_fin = sum(game$is_goal==1 & game$side==1) -
      sum(game$is_goal==1 & game$side==2)
    
    hsubs_at_t1 = sum(game$event_type==7 & game$time<t1 & game$side==1)
    hsubs_at_t2 = sum(game$event_type==7 & game$time<t2 & game$side==1)
    hsubs_at_t3 = sum(game$event_type==7 & game$time<t3 & game$side==1)
    
    agoals_t1 = hgoals_t1*(-1)
    agoals_t2 = hgoals_t2*(-1)
    agoals_t3 = hgoals_t3*(-1)
    agoals_fin = hgoals_fin*(-1)
    asubs_at_t1 = sum(game$event_type==7 & game$time<t1& game$side==2)
    asubs_at_t2 = sum(game$event_type==7 & game$time<t2& game$side==2)
    asubs_at_t3 = sum(game$event_type==7 & game$time<t3& game$side==2)
    
    hfailed=FALSE
    hstart_follow=0
    afailed=FALSE
    astart_follow=0
    ############## Chance point #1 ##########################
    if (hgoals_t1 < 0){ #potential for home team
      if (hsubs_at_t1>0){hstart_follow=1}
      else{
        hfailed=TRUE
        could = c(could,g)
        side = c(side,1)
        when_follow = c(when_follow,-1)
        score_when_could = c(score_when_could,hgoals_t1)
        tb = sort(game[which(game$is_goal=='1' & game$time < t1 & game$side=='2'),'time'],
                         decreasing=TRUE)[abs(hgoals_t1)]
        time_behind = c(time_behind,(t1-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t1))
        }
    }
    if (agoals_t1 < 0){ #potential for away team
      if (asubs_at_t1>0){astart_follow=1}
      else{
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        when_follow = c(when_follow,-1)
        score_when_could = c(score_when_could,agoals_t1)
        tb = sort(game[which(game$is_goal=='1' & game$time < t1 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t1)]
        time_behind = c(time_behind,(t1-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t1))
      }
    }
    
    ############## Chance point #2 ##########################
    if (hgoals_t2 < 0 & hfailed==FALSE){#potential for home team
      if (hsubs_at_t2<2){
        hfailed=TRUE
        could = c(could,g)
        side = c(side,1)
        score_when_could = c(score_when_could,hgoals_t2)
        tb = sort(game[which(game$is_goal=='1' & game$time < t2 & game$side=='2'),'time'],
                  decreasing=TRUE)[abs(hgoals_t2)]
        time_behind = c(time_behind,(t2-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t2))
        if (hstart_follow==1){when_follow = c(when_follow,-1)}
        else {when_follow = c(when_follow,-2)}
        }
      else{
        if (hstart_follow==0){hstart_follow=2} #started now
      }
    }
    if (agoals_t2 < 0 & afailed==FALSE){#potential for away team
      if (asubs_at_t2<2){
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        score_when_could = c(score_when_could,agoals_t2)
        tb = sort(game[which(game$is_goal=='1' & game$time < t2 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t2)]
        time_behind = c(time_behind,(t2-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t2))
        if (astart_follow==1){when_follow = c(when_follow,-1)}
        else {when_follow = c(when_follow,-2)}
      }
      else{
        if (astart_follow==0){astart_follow=2} #started now
      }
    }
    ############## Chance point #3 ##########################
    if (hgoals_t3 < 0 & hfailed==FALSE){#potential for home team
      if (hsubs_at_t3<3){
        hfailed=TRUE
        could = c(could,g)
        side = c(side,1)
        score_when_could = c(score_when_could,hgoals_t3)
        tb = sort(game[which(game$is_goal=='1' & game$time < t3 & game$side=='2'),'time'],
                  decreasing=TRUE)[abs(hgoals_t3)]
        time_behind = c(time_behind,(t3-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t3))
        if (hstart_follow>0){when_follow = c(when_follow,hstart_follow*-1)}
        else {when_follow = c(when_follow,-3)}
        }
      else{
        if (hstart_follow==0){hstart_follow=3} #started now
      }
    }
    if (agoals_t3 < 0 & afailed==FALSE){#potential for away team
      if (asubs_at_t3<3){
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        score_when_could = c(score_when_could,agoals_t3)
        tb = sort(game[which(game$is_goal=='1' & game$time < t3 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t3)]
        time_behind = c(time_behind,(t3-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t3))
        if (astart_follow>0){when_follow = c(when_follow,astart_follow*-1)}
        else {when_follow = c(when_follow,-3)}
      }
      else{
        if (astart_follow==0){astart_follow=3} #started now
      }
    }
    ################# Final check for success ##################
    if (hfailed == FALSE & hstart_follow>0){ #home team success
      could = c(could,g)
      side = c(side,1)
      when_follow = c(when_follow,hstart_follow) #followers unit
      if (hstart_follow==1){
        score_when_could = c(score_when_could,hgoals_t1)
        tb = sort(game[which(game$is_goal=='1' & game$time < t1 & game$side=='2'),'time'],
                  decreasing=TRUE)[abs(hgoals_t1)]
        time_behind = c(time_behind,(t1-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t1))}
      if (hstart_follow==2){
        score_when_could = c(score_when_could,hgoals_t2)
        tb = sort(game[which(game$is_goal=='1' & game$time < t2 & game$side=='2'),'time'],
                  decreasing=TRUE)[abs(hgoals_t2)]
        time_behind = c(time_behind,(t2-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t2))}
      if (hstart_follow==3){
        score_when_could = c(score_when_could,hgoals_t3)
        tb = sort(game[which(game$is_goal=='1' & game$time < t3 & game$side=='2'),'time'],
                  decreasing=TRUE)[abs(hgoals_t3)]
        time_behind = c(time_behind,(t3-0.5-tb))
        outcome = c(outcome,(hgoals_fin - hgoals_t3))}
    }
    if (afailed == FALSE & astart_follow>0){ #away team success
      could = c(could,g)
      side = c(side,2)
      when_follow = c(when_follow,astart_follow) #followers unit
      if (astart_follow==1){
        score_when_could = c(score_when_could,agoals_t1)
        tb = sort(game[which(game$is_goal=='1' & game$time < t1 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t1)]
        time_behind = c(time_behind,(t1-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t1))}
      if (astart_follow==2){
        score_when_could = c(score_when_could,agoals_t2)
        tb = sort(game[which(game$is_goal=='1' & game$time < t2 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t2)]
        time_behind = c(time_behind,(t2-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t2))}
      if (astart_follow==3){
        score_when_could = c(score_when_could,agoals_t3)
        tb = sort(game[which(game$is_goal=='1' & game$time < t3 & game$side=='1'),'time'],
                  decreasing=TRUE)[abs(agoals_t3)]
        time_behind = c(time_behind,(t3-0.5-tb))
        outcome = c(outcome,(agoals_fin - agoals_t3))}
    }
  }
  #organize relevant columns
  treatment = rep(0,length(when_follow))
  treatment[which(when_follow>0)]=1
  when_follow = abs(when_follow)
  binary_outcome = rep(0,length(outcome))
  binary_outcome[which(outcome>0)]=1
  
  
  test_set = data.frame(id_odsp=could,side=side,chance=when_follow,score_at_chance=score_when_could,
                     time_behind=time_behind,treatment=treatment,outcome=outcome,binary_outcome=binary_outcome)
  
  ###
  #### Part 2: Fetch game info
  ###
  test_set$id_odsp = as.character(test_set$id_odsp)
  matchinfo = read.csv('ginf.csv')
  games = matchinfo[,c(1,5,6,7,12:14)]
  games = games[which(games$id_odsp %in% test_set$id_odsp),]
  games$id_odsp = as.character(games$id_odsp)
  odds = c()
  odds2 = c()
  for (i in 1:nrow(test_set)){
    odds_diff_h =  games[which(games$id_odsp==test_set[i,'id_odsp']),'odd_h'] - 
      games[which(games$id_odsp==test_set[i,'id_odsp']),'odd_a']
    if (test_set[i,'side']==1){
      odds = c(odds, odds_diff_h)
      odds2 = c(odds2, sign(odds_diff_h)*odds_diff_h^2)}
    else{
      odds = c(odds, -1*odds_diff_h)
      odds2 = c(odds2, sign(-1*odds_diff_h)*odds_diff_h^2)}
  }
  test_set$odds = odds
  test_set$odds2 = odds2
  
  return(test_set)
}

test_set = Build_test_set(testevents,64.5,71.5,77.5) #build test set based on the decision rule
test_set_time = test_set[which(test_set$time_behind > 7),] #only those behind more than 7 mins
test_time_1 = test_set_time[which(test_set_time$chance==1),] #only those who could start from point 1

##
###
#### Genetic Matching
###
##

#Define covariates to match on - presented in Table 2
X1t = cbind(test_time_1$score_at_chance,test_time_1$time_behind,
           test_time_1$odds,test_time_1$odds2,test_time_1$side,I(test_time_1$score_at_chance^2),
           I(test_time_1$time_behind^2),I(test_time_1$score_at_chance*test_time_1$time_behind),
           I(test_time_1$odds*test_time_1$time_behind),I(test_time_1$odds*test_time_1$score_at_chance))

caliper1t2 = c(0.45,0.45,0.45,0.45,100,100,100,100,100,100) 
#caliper to ensure matching result above 0.2 consistently
#without it or with a higher cap it didn't work well


genout1t2 = GenMatch(Tr=test_time_1$treatment,X=X1t,caliper=caliper1t2,
                   pop.size = 500, wait.generations = 15) #run genetic matching

mout1t2 = Match(Tr=test_time_1$treatment,X=X1t,caliper=caliper1t2,Weight.matrix = genout1t2)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~score_at_chance+time_behind+odds+odds2+side+I(score_at_chance^2)+
               I(time_behind^2)+I(score_at_chance*time_behind)+I(odds*time_behind)+
               I(odds*score_at_chance),data=test_time_1,match.out = mout1t2) 
#
##
#
summary(mout1t2) #examine number of followers and defiers remained

Ymout1t = Match(Y=test_time_1$binary_outcome,Tr=test_time_1$treatment,X=X1t,
              caliper=caliper1t2,Weight.matrix = genout1t2) #obtain effect

summary(Ymout1t2)

mean(test_time_1[mout1t2$index.treated,'binary_outcome']) # % success in matched follower
mean(test_time_1[mout1t2$index.control,'binary_outcome']) # % success in matched defiers 



##
###
#### Testing the Myers' original decision rule on the all relevant matches in the sample
###
##

test_myers = Build_test_set(allevents,57.5,72.5,78.5) #build test set based on the decision rule
test_myers = test_myers[which(test_myers$chance==1 & test_myers$time_behind>7),]

Xmyers = cbind(test_myers$score_at_chance,test_myers$time_behind,
               test_myers$odds,test_myers$odds2,test_myers$side,I(test_myers$score_at_chance^2),
          I(test_myers$time_behind^2),I(test_myers$score_at_chance*test_myers$time_behind),
          I(test_myers$odds*test_myers$time_behind),I(test_myers$odds*test_myers$score_at_chance))

caliper_myers = c(0.45,0.45,0.45,0.45,100,100,100,100,100,100)


genout_myers = GenMatch(Tr=test_myers$treatment,X=Xmyers,caliper=caliper_myers, 
                  pop.size = 500, wait.generations = 15)

mout_myers = Match(Tr=test_myers$treatment,X=Xmyers,caliper=caliper_myers,Weight.matrix = genout_myers)

MatchBalance(treatment~score_at_chance+time_behind+odds+odds2+side+I(score_at_chance^2)+
               I(time_behind^2)+I(score_at_chance*time_behind)+I(odds*time_behind)+
               I(odds*score_at_chance),data=test_myers,match.out = mout_myers)

summary(mout_myers)

Ymout_myers = Match(Y=test_myers$binary_outcome,Tr=test_myers$treatment,X=Xmyers,
              caliper=caliper_myers,Weight.matrix = genout_myers)

summary(Ymout_myers)

mean(test_myers[mout_myers$index.treated,'binary_outcome']) # % success in matched follower
mean(test_myers[mout_myers$index.control,'binary_outcome']) # % success in matched defiers 
