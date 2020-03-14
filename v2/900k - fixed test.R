library(stringr)
library(tree)
library(Matching)
source('fell_behind.R') #define the function to calcualte when did a team fell behind
                        #used in building the test set
##
###
#### Clean data: remove NAs and games with injuries and red cards
###
##

allevents = read.csv('data.csv') #import data pre-processed in preprocessing.R

##
###
#### Create a training set based on a random sample of half of the eligible matches
###
##

random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp))/2) #select half the eliginle match on random
train_set = allevents[which(allevents$id_odsp %in% random_matches),]
sum(train_set$event_type==7) #final num of subs in sample

#6/1/2020 - USING ALL THE DATA AS THE 'TRAINING'
train_set = allevents


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
  
  #handle first row
  if(df[1,'event_type']==7){ #if sub
    if (df[1,'side']==1){
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
  if(df[1,'is_goal']==1){ #if goal
    if (df[1,'side']==1){hscore = hscore+1}
    else{ascore = ascore+1}
  }
  
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

#run the function on the training sample matches to get the subs with the required variables
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

#Auxiliary functions to 'mimic' tree activity for the first split (as explained in Appendix A)
#And obtain p-values provided in the Table 1

before_after_minute = function(df){ 
#Input: training dataset with columns 'time', 'goal_diff_improve01'
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

pvalue = function(df,m){ #perform t-test between the before-after groups for Table 1 
  
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

library(rpart)
tree1 = rpart(goal_diff_improve01~time,data=first_train_behind, method='anova')
tree1
mimic = before_after_minute(first_train_behind)
round(mimic,3)
#
## Figure in Appendix A
#
plot(mimic$time[6:66],mimic$difference[6:66], xlab='Minute',ylab ='Before-After Mean Success Difference',
     main='First Sub Mean Before-After Success by Minute',pch=16)
abline(v=64.5,lty=2)
text('64.5: largest before-after difference',x=48,y=-0.06)
pvalue(first_train_behind,64.5) #p-value is 0.0001424 - but this is not the correct metric for tree
#
##
#

tree2 = rpart(goal_diff_improve01~time,data=second_train_behind, method='anova')
tree2 #obtain before and after proportions
pvalue(second_train_behind,69.5) #p=3.117e-07

tree3 = rpart(goal_diff_improve01~time,data=third_train_behind, method='anova')
#tree3_best = rtree
tree3 #obtain before and after proportions
pvalue(third_train_behind,77.5) #p=6.54e-08



##
###
#### Testing the rule on the other half of the data: unused matches
###
##


testevents = allevents[-which(allevents$id_odsp %in% random_matches),] #the other 2173 matches unused for training

#6.1.10 NO TRAIN/TEST - always use ALL
testevents = allevents

new_Build_test_set = function(df,t1,t2,t3){
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
  #dec_points = c() #in which decision points the team was trailing --- removed because affecting outcome
  
  for (g in allgames){
    game = df[which(df$id_odsp==g),]
    
    #Find out the score at and subs prior to each decsion point for home team
    hgoals_t1 = sum(game$is_goal==1 & game$time<t1 & game$side==1) -
      sum(game$is_goal==1 & game$time<t1 & game$side==2)
    hgoals_t2 = sum(game$is_goal==1 & game$time<t2 & game$side==1) -
      sum(game$is_goal==1 & game$time<t2 & game$side==2)
    hgoals_t3 = sum(game$is_goal==1 & game$time<t3 & game$side==1) -
      sum(game$is_goal==1 & game$time<t3 & game$side==2)
    hgoals_fin = sum(game$is_goal==1 & game$side==1) -
      sum(game$is_goal==1 & game$side==2)
    
    hsubs = game[which(game$event_type==7 & game$side==1),'time']
    
    while(length(hsubs)<3){ #add 1 as sub time if team did not make all three subs
      hsubs = c(hsubs,99)
    }
    hsubs_at_t1 = sum(game$event_type==7 & game$time<t1 & game$side==1)
    hsubs_at_t2 = sum(game$event_type==7 & game$time<t2 & game$side==1)
    hsubs_at_t3 = sum(game$event_type==7 & game$time<t3 & game$side==1)
    
    #Find out the score at and subs prior to each decsion point for away team
    agoals_t1 = hgoals_t1*(-1)
    agoals_t2 = hgoals_t2*(-1)
    agoals_t3 = hgoals_t3*(-1)
    agoals_fin = hgoals_fin*(-1)
    
    asubs = game[which(game$event_type==7 & game$side==2),'time']
    while(length(asubs)<3){ #add 1 as sub time if team did not make all three subs
      asubs = c(asubs,99)
    }
    asubs_at_t1 = sum(game$event_type==7 & game$time<t1& game$side==2)
    asubs_at_t2 = sum(game$event_type==7 & game$time<t2& game$side==2)
    asubs_at_t3 = sum(game$event_type==7 & game$time<t3& game$side==2)
    
    #Set game variables to zero - whether a team failed to follow, when started
    hfailed=FALSE
    hstart_follow=0
    afailed=FALSE
    astart_follow=0
    scoreline = htimes_behind(game)#get hscore at every minute
    
    ############## Chance point #1 ##########################
    
    if (sum(scoreline[1:(t1-0.5)]<0) > 0){ #home team was behind any point before t1 
      fb = (which(scoreline[1:(t1-0.5)] %in% c(-7:-1)))
      tb = t1-0.5-fb[1]
      if (hsubs[1] < t1 && scoreline[hsubs[1]]<0 && hsubs[1] > fb[1]){ #home team was behind, subbed after conceding, before t1
        hstart_follow=1  
      }
    }
    if (hgoals_t1 < 0 && tb > 4 && hsubs[1] > t1){ #the home team did not sub on time
          hfailed=TRUE
          could = c(could,g)
          side = c(side,1)
          #latest_trail = rev(which(scoreline[1:(t1-0.5)] %in% c(-7:-1)))[1]
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,hgoals_t1)
          time_behind = c(time_behind,t1-0.5-fb[1])
          outcome = c(outcome,(hgoals_fin - hgoals_t1))
    }
    
  #away
  
    if (sum(scoreline[1:(t1-0.5)]>0) > 0){ #away team was behind at any point before t1 
      fb = (which(scoreline[1:(t1-0.5)] %in% c(1:7)))
      tb = t1-0.5-fb[1]
      if (asubs[1] < t1 && scoreline[asubs[1]]>0 && asubs[1] > fb[1]){ #home team was behind at time of first sub
        astart_follow=1  
      }
    }
    if (agoals_t1 < 0 && tb > 4 && asubs[1] > t1){ #the home team did not sub - create control with the latest time of trail before t1
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        #latest_trail = rev(which(scoreline[1:(t1-0.5)] %in% c(1:7)))[1]
        when_follow = c(when_follow,-1)
        score_when_could = c(score_when_could,agoals_t1)
        time_behind = c(time_behind,t1-0.5-fb[1])
        outcome = c(outcome,(agoals_fin - agoals_t1))
    }
    
    ############## Chance point #2 ##########################
    if (sum(scoreline[(t1-0.5):(t2-0.5)]<0) > 0 && hfailed==FALSE){ #home team was behind for at least 5m before t2
      fb = (which(scoreline[1:(t2-0.5)] %in% c(-7:-1)))
      tb = t2-0.5-fb[1]
      if (hsubs[2]<t2 && scoreline[hsubs[2]]<0 && hsubs[2] > fb[1] && hstart_follow==0){
          hstart_follow=2  
      }
    }
    if (hgoals_t2 < 0 && tb > 4 && hsubs[2] > t2 && hfailed==FALSE){ #the home team did not sub - create control with the latest time of trail before t2?
         #assuming only one entry with earliest failure
        hfailed=TRUE
        could = c(could,g)
        side = c(side,1)
        if (hstart_follow==1){
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,hgoals_t1)
          tb = sum(scoreline[1:hsubs[1]]<0)
          time_behind = c(time_behind,tb)
          
          }
        else {
          when_follow = c(when_follow,-2)
          score_when_could = c(score_when_could,hgoals_t2)
          time_behind = c(time_behind,t2-0.5-fb[1])
          }
        
        #latest_trail = rev(which(scoreline[(t1-0.5):(t2-0.5)] %in% c(-7:-1)))[1]
        outcome = c(outcome,(hgoals_fin - hgoals_t2)) 
      }
    
    #away 
    
    if (sum(scoreline[(t1-0.5):(t2-0.5)]>0) > 0 && hfailed==FALSE){ #away team was behind for at least 5m at any point before t1 
      fb = (which(scoreline[1:(t2-0.5)] %in% c(1:7)))
      tb = t2-0.5-fb[1]
      if (asubs[2] < t2 && scoreline[asubs[2]]>0 && asubs[2] > fb[1] && astart_follow==0){
          astart_follow=2  
      }
    }
    if (agoals_t2 < 0 && tb > 4 && asubs[2] > t2 && afailed==FALSE){ #the away team did not sub - create control with the latest time of trail before t2?
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        if (astart_follow==1){
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,agoals_t1)
          tb = sum(scoreline[1:asubs[1]]>0)
          time_behind = c(time_behind,tb)
          
        }
        else {
          when_follow = c(when_follow,-2)
          score_when_could = c(score_when_could,agoals_t2)
          time_behind = c(time_behind,t2-0.5-fb[1])
          
        }
        
        #latest_trail = rev(which(scoreline[(t1-0.5):(t2-0.5)] %in% c(-7:-1)))[1]
        outcome = c(outcome,(agoals_fin - agoals_t2))
    }
      
    ############## Chance point #3 ##########################
    if (sum(scoreline[(t2-0.5):(t3-0.5)]<0) > 0 && hfailed==FALSE){ #home team was behind for at least 5m at any point before t1 
      fb = (which(scoreline[1:(t3-0.5)] %in% c(-7:-1)))
      tb = t3-0.5-fb[1]

      if (hsubs[3]<t3 && scoreline[hsubs[3]]<0 && hsubs[3] > fb[1] && hstart_follow==0){
          hstart_follow=3  
      }
    }
    if (hgoals_t3 < 0 && tb > 4 && hsubs[3] > t3 && hfailed==FALSE){ #the home team did not sub - create control with the latest time of trail before t2?
        hfailed=TRUE
        could = c(could,g)
        side = c(side,1)
        if (hstart_follow==0) {
          when_follow = c(when_follow,-3)
          score_when_could = c(score_when_could,hgoals_t3)
          time_behind = c(time_behind,t3-0.5-fb[1])
          
        }
        else{
          if (hstart_follow==1){
            when_follow = c(when_follow,-1)
            score_when_could = c(score_when_could,hgoals_t1)
            tb = sum(scoreline[1:hsubs[1]]<0)
            time_behind = c(time_behind,tb)
            
          }
          else{
            when_follow = c(when_follow,-2)
            score_when_could = c(score_when_could,hgoals_t2)
            tb = sum(scoreline[(t1-0.5):hsubs[2]]<0)
            time_behind = c(time_behind,tb)
            
          }  
        }
        #latest_trail = rev(which(scoreline[(t1-0.5):(t2-0.5)] %in% c(-7:-1)))[1]
        outcome = c(outcome,(hgoals_fin - hgoals_t3))
      }
    
    #away 
    
    if (sum(scoreline[(t2-0.5):(t3-0.5)]>0) > 0 && hfailed==FALSE){ #away team was behind for at least 5m at any point before t1 
      fb = (which(scoreline[1:(t3-0.5)] %in% c(1:7)))
      tb = t3-0.5-fb[1]

      if (asubs[3] < t3 && scoreline[asubs[3]]>0 && asubs[3] > fb[1] && astart_follow==0){
          astart_follow=3
      }
    }
    if (agoals_t3 < 0 && tb > 4 && asubs[3] > t3 && afailed==FALSE){ #the away team did not sub - create control with the latest time of trail before t2?
        afailed=TRUE
        could = c(could,g)
        side = c(side,2)
        if (astart_follow==0) {
          when_follow = c(when_follow,-3)
          score_when_could = c(score_when_could,agoals_t3)
          time_behind = c(time_behind,t3-0.5-fb[1])
          
        }
        else{
            if (astart_follow==1){
            when_follow = c(when_follow,-1)
            score_when_could = c(score_when_could,agoals_t1)
            tb = sum(scoreline[1:asubs[1]]>0)
            
            time_behind = c(time_behind,tb)
            
            }
            else{
              when_follow = c(when_follow,-2)
              score_when_could = c(score_when_could,agoals_t1)
              tb = sum(scoreline[(t1-0.5):asubs[2]]>0)
              time_behind = c(time_behind,tb)
              
            }
        }
        
        #latest_trail = rev(which(scoreline[(t1-0.5):(t2-0.5)] %in% c(-7:-1)))[1]
        outcome = c(outcome,(agoals_fin - agoals_t3)) 
    }
    
    ################# Final check for success ##################
    if (hfailed == FALSE & hstart_follow>0){ #home team success
      could = c(could,g)
      side = c(side,1)
      when_follow = c(when_follow,hstart_follow) #followers unit
      if (hstart_follow==1){
        tb = sum(scoreline[1:hsubs[1]]<0)
        score_when_could = c(score_when_could,scoreline[hsubs[1]])
        time_behind = c(time_behind,tb)
        outcome = c(outcome,(hgoals_fin - scoreline[hsubs[1]]))}
      if (hstart_follow==2){
        tb = sum(scoreline[1:hsubs[2]]<0)
        score_when_could = c(score_when_could,scoreline[hsubs[2]])
        time_behind = c(time_behind,(tb))
        outcome = c(outcome,(hgoals_fin - scoreline[hsubs[2]]))}
      if (hstart_follow==3){
        tb = sum(scoreline[1:hsubs[3]]<0)
        score_when_could = c(score_when_could,scoreline[hsubs[3]])
        time_behind = c(time_behind,(tb))
        outcome = c(outcome,(hgoals_fin - scoreline[hsubs[3]]))}
    }
    if (afailed == FALSE & astart_follow>0){ #away team success
      could = c(could,g)
      side = c(side,2)
      when_follow = c(when_follow,astart_follow) #followers unit
      if (astart_follow==1){
        tb = sum(scoreline[1:asubs[1]]>0)
        score_when_could = c(score_when_could,-1*scoreline[asubs[1]])
        time_behind = c(time_behind,tb)
        outcome = c(outcome,(agoals_fin - -1*scoreline[asubs[1]]))}
      if (astart_follow==2){
        tb = sum(scoreline[1:asubs[2]]>0)
        score_when_could = c(score_when_could,-1*scoreline[asubs[2]])
        time_behind = c(time_behind,(tb))
        outcome = c(outcome,(agoals_fin - -1*scoreline[asubs[2]]))}
      if (astart_follow==3){
        tb = sum(scoreline[1:asubs[3]]>0)
        score_when_could = c(score_when_could,-1*scoreline[asubs[3]])
        time_behind = c(time_behind,(tb))
        outcome = c(outcome,(agoals_fin - -1*scoreline[asubs[3]]))}
    }
    ############# Adding multiple control units for failed ###########
    #if (hfailed){
    #  
    #}
    #if(afailed){
    #  #same
    #}
  }
      
  #organize relevant columns
  treatment = rep(0,length(when_follow))
  treatment[which(when_follow>0)]=1
  when_follow = abs(when_follow)
  binary_outcome = rep(0,length(outcome))
  binary_outcome[which(outcome>0)]=1
  
  test_set = data.frame(id_odsp=could,side=side,chance=when_follow,score_at_chance=score_when_could,
                     time_behind=time_behind,treatment=treatment,outcome=outcome,
                     binary_outcome=binary_outcome)
  
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
#consider changing to ratio

#trial = new_Build_test_set(testevents[1:100,],64.5,69.5,77.5) #build test set based on the decision rule

test_set = new_Build_test_set(testevents,64.5,69.5,77.5) #build test set based on the decision rule
#this is lower than the 9 minutes because it's cases that DID sub - that's reasonable to do in 5m.
#test_time_1 = test_set[which(test_set$chance==1),] #only those who could start from point 1
test_time_1 = test_set

##
###
#### Genetic Matching
###
##

#Define covariates to match on - presented in Table 2
#X = cbind(test_time_1$score_at_chance,test_time_1$time_behind,
#            test_time_1$odds,test_time_1$odds2,test_time_1$side,I(test_time_1$score_at_chance^2),
#            I(test_time_1$time_behind^2),I(test_time_1$score_at_chance*test_time_1$time_behind),
#            I(test_time_1$odds*test_time_1$time_behind),I(test_time_1$odds*test_time_1$score_at_chance))

X = cbind(test_time_1$chance,test_time_1$score_at_chance,test_time_1$time_behind,
          test_time_1$odds,test_time_1$odds2,test_time_1$side,I(test_time_1$score_at_chance^2),
          I(test_time_1$time_behind^2),I(test_time_1$score_at_chance*test_time_1$time_behind),
          I(test_time_1$odds*test_time_1$time_behind),I(test_time_1$odds*test_time_1$score_at_chance))

caliper = c(0.25,0.25,0.25,0.25,0.25,100,100,100,100,100,100) 
exact = rep(FALSE,length(caliper))
exact[1]=TRUE

genout = GenMatch(Tr=test_time_1$treatment,X=X, caliper=caliper,exact=exact,
                   pop.size = 500, wait.generations = 25) #run genetic matching

mout = Match(Tr=test_time_1$treatment,X=X,caliper=caliper,exact=exact
             ,Weight.matrix = genout)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~chance+score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance^2)+I(time_behind^2)+I(score_at_chance*time_behind)+
               I(odds*time_behind)+I(odds*score_at_chance),data=test_time_1,match.out = mout) 
#
##
#
summary(mout) #examine number of followers and defiers remained

Ymout = Match(Y=test_time_1$binary_outcome,Tr=test_time_1$treatment,X=X,caliper=caliper,exact=exact,
              Weight.matrix = genout) #obtain effect

summary(Ymout)

mean(test_time_1[mout$index.treated,'binary_outcome']) # % success in matched follower
mean(test_time_1[mout$index.control,'binary_outcome']) # % success in matched defiers 

#Sensitivity test
library(rbounds)
#sensitivity test
psens(Ymout,Gamma=2,GammaInc=0.1) 
hlsens(Ymout,Gamma=2,GammaInc=0.1) 


#25.12 change: when I create control units, create not only for the failture at t, but when I know
#they failed I can create a counterfactual for another treatment unit using their situation 2,3,4,5
#etc. minutes before Tx, as long as they were behind, using the 'time behind' and score for these 
#minutes to generate more control units (remember the interpretation is follow potential).
#this is similar to the derivation of the rule idea - just in this case I would consider each 
#numsub and minute as covariates, and generate controls ONLY with those who had less subs as I wrote
#in the notebook. This way also adds a nice twist - to suit the inquiry for top-tier questions (what
#minutes are best for them, maybe wait longer?) I can choose treatment group only with high odds.
#that could be a nice last section for the analysis.

##
###
#### Testing the Myers' original decision rule on the all relevant matches in the sample
###
##

#6.1.10 NO TRAIN/TEST - always use ALL
testevents = allevents
test_myers = Build_test_set(allevents,57.5,72.5,78.5) #build test-set based on the decision rule
#test_myers = test_myers[which(test_myers$chance==1 & test_myers$time_behind>7),]

Xmyers = cbind(test_myers$chance, test_myers$score_at_chance,test_myers$time_behind,
               test_myers$odds,test_myers$odds2,test_myers$side,I(test_myers$score_at_chance*test_myers$time_behind),
          I(test_myers$odds*test_myers$time_behind),I(test_myers$odds*test_myers$score_at_chance))

caliper_myers = c(100,0.45,0.45,0.45,0.45,100,100,100,100)
exact_myers = rep(FALSE,length(caliper_myers))
exact_myers[1]=TRUE

genout_myers = GenMatch(Tr=test_myers$treatment,X=Xmyers,caliper=caliper_myers,exact=exact_myers, 
                  pop.size = 500, wait.generations = 15)

mout_myers = Match(Tr=test_myers$treatment,X=Xmyers,caliper=caliper_myers,exact=exact_myers,
                   Weight.matrix = genout_myers)

MatchBalance(treatment~chance+score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance*time_behind)+I(odds*time_behind)+
               I(odds*score_at_chance),data=test_myers,match.out = mout_myers)

summary(mout_myers)

Ymout_myers = Match(Y=test_myers$binary_outcome,Tr=test_myers$treatment,X=Xmyers,
              caliper=caliper_myers,exact=exact_myers,Weight.matrix = genout_myers)

summary(Ymout_myers)

mean(test_myers[mout_myers$index.treated,'binary_outcome']) # % success in matched follower
mean(test_myers[mout_myers$index.control,'binary_outcome']) # % success in matched defiers 
