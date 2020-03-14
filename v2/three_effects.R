
Build_test_alt = function(df,t1,t2,t3){
  #Input: dataframe with match events to find the effect of subbing in various 1st/2nd/3rd sub timings
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
    
    hsubs_at_t1 = sum(game$event_type==7 & game$time<t1 & game$side==1)
    hsubs_at_t2 = sum(game$event_type==7 & game$time<t2 & game$side==1)
    hsubs_at_t3 = sum(game$event_type==7 & game$time<t3 & game$side==1)
    
    #Find out the score at and subs prior to each decsion point for away team
    agoals_t1 = hgoals_t1*(-1)
    agoals_t2 = hgoals_t2*(-1)
    agoals_t3 = hgoals_t3*(-1)
    agoals_fin = hgoals_fin*(-1)
    asubs_at_t1 = sum(game$event_type==7 & game$time<t1& game$side==2)
    asubs_at_t2 = sum(game$event_type==7 & game$time<t2& game$side==2)
    asubs_at_t3 = sum(game$event_type==7 & game$time<t3& game$side==2)
    
    scoreline = htimes_behind(game)#get hscore at every minute
    hsubs = game[which(game$event_type==7 & game$side==1),'time']
    asubs = game[which(game$event_type==7 & game$side==2),'time']
    
    ############## Chance point #1 ##########################
    ##### Home team #####
    if (hgoals_t1 < 0){ #home team is behind
      
      fb = fell_behind(game,t1,1) #(fell_behind, min_sub,score
      tb = t1-0.5-fb[1]
      
      if (hsubs_at_t1>0){ #if subbed before the point

        if (!is.na(fb[2])){ #if the first sub was after falling behind - success following
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,1)
          score_when_could = c(score_when_could,fb[3])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - fb[3] ))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,hgoals_t1)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - hgoals_t1))  
        }
      }
    }
    ##### away team #####
    if (agoals_t1 < 0){ #home team is behind
      fb = fell_behind(game,t1,2) #(fell_behind, min_sub,score
      tb = t1-0.5-fb[1]
      if (asubs_at_t1>0){ #if subbed before the point
       
        if (!is.na(fb[2])){ #if the sub was after falling behind - success following
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,1)
          score_when_could = c(score_when_could,fb[3])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - fb[3]))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,agoals_t1)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - agoals_t1))  
        }
      }
    }
    
    ############## Chance point #2 ##########################
    ##### Home team #####
    if (hgoals_t2 < 0){ #home team is behind
      fb = fell_behind(game,t2,1) #(fell_behind, min_sub,score
      tb = t2-0.5-fb[1]
      if (hsubs_at_t2>1){ #if subbed before the point
        
        if (!is.na(fb[2])){ #if the sub was after falling behind - success following
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,2)
          score_when_could = c(score_when_could,fb[4])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - fb[4]))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,-2)
          score_when_could = c(score_when_could,hgoals_t2)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - hgoals_t2))  
        }
      }
    }
    ##### away team #####
    if (agoals_t2 < 0){ #home team is behind
      fb = fell_behind(game,t2,2) #(fell_behind, min_sub,score
      tb = t2-0.5-fb[1]
      if (asubs_at_t2>1){ #if subbed before the point
        
        if (!is.na(fb[2])){ #if the sub was after falling behind - success following
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,2)
          score_when_could = c(score_when_could,fb[4])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - fb[4]))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,-2)
          score_when_could = c(score_when_could,agoals_t2)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - agoals_t2))  
        }
      }
    }
    ############## Chance point #3 ##########################
    ##### Home team #####
    if (hgoals_t3 < 0){ #home team is behind
      fb = fell_behind(game,t3,1) #(fell_behind, min_sub,score
      tb = t3-0.5-fb[1]
      if (hsubs_at_t3==3){ #if subbed before the point
        
        if (!is.na(fb[2])){ #if the sub was after falling behind - success following
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,3)
          score_when_could = c(score_when_could,fb[5])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - fb[5] ))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,1)
          when_follow = c(when_follow,-3)
          score_when_could = c(score_when_could,hgoals_t3)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(hgoals_fin - hgoals_t3))  
        }
      }
    }
    ##### away team #####
    if (agoals_t3 < 0){ #home team is behind
      fb = fell_behind(game,t3,2) #(fell_behind, min_sub,score
      tb = t3-0.5-fb[1]
      if (asubs_at_t3==3){ #if subbed before the point
        
        if (!is.na(fb[2])){ #if the sub was after falling behind - success following
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,3)
          score_when_could = c(score_when_could,fb[5])
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - fb[5]))
        } 
      } 
      else{ #the home team did not sub
        if (tb>9){ #while it had enough time --> failed to follow
          could = c(could,g)
          side = c(side,2)
          when_follow = c(when_follow,-3)
          score_when_could = c(score_when_could,agoals_t3)
          time_behind = c(time_behind,tb)
          outcome = c(outcome,(agoals_fin - agoals_t3))  
        }
      }
    }
  }
    
  ###### Organize relevant columns
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


alt_test = Build_test_alt(testevents,64.5, 71.5, 77.5)

alt_test_1= alt_test[which(alt_test$chance==1),]
sum(alt_test_1$treatment) #num treated units
nrow(alt_test_1) - sum(alt_test_1$treatment) #num control units

alt_test_2= alt_test[which(alt_test$chance==2),]
sum(alt_test_2$treatment) #num treated units
nrow(alt_test_2) - sum(alt_test_2$treatment) #num control units

alt_test_3= alt_test[which(alt_test$chance==3),]
sum(alt_test_3$treatment) #num treated units
nrow(alt_test_3) - sum(alt_test_3$treatment) #num control units


##
###
#### Genetic Matching - one per sub timing
###
##

################### Matching for Chance 1 #######################

#Define covariates to match on - presented in Table 2
X1 = cbind(alt_test_1$score_at_chance,alt_test_1$time_behind,
          alt_test_1$odds,alt_test_1$odds2,alt_test_1$side,I(alt_test_1$score_at_chance^2),
          I(alt_test_1$time_behind^2),I(alt_test_1$score_at_chance*alt_test_1$time_behind),
          I(alt_test_1$odds*alt_test_1$time_behind),I(alt_test_1$odds*alt_test_1$score_at_chance))


caliper1 = c(0.4,0.4,0.4,0.4,100,100,100,100,100,100) 

genout1 = GenMatch(Tr=alt_test_1$treatment,X=X1,caliper=caliper1,
                  pop.size = 500, wait.generations = 15) #run genetic matching

mout1 = Match(Tr=alt_test_1$treatment,X=X1,caliper=caliper1,Weight.matrix = genout1)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance^2)+I(time_behind^2)+I(score_at_chance*time_behind)+
               I(odds*time_behind)+I(odds*score_at_chance),data=alt_test_1,match.out = mout1) 
#
##
#
summary(mout1) #examine number of followers and defiers remained

Ymout1 = Match(Y=alt_test_1$binary_outcome,Tr=alt_test_1$treatment,X=X1,
              caliper=caliper1,Weight.matrix = genout1) #obtain effect

summary(Ymout1)

mean(alt_test_1[mout1$index.treated,'binary_outcome']) # % success in matched follower
mean(alt_test_1[mout1$index.control,'binary_outcome']) # % success in matched defiers 




################### Matching for Chance 2 #######################



#Define covariates to match on - presented in Table 2
X2 = cbind(alt_test_2$score_at_chance,alt_test_2$time_behind,
           alt_test_2$odds,alt_test_2$odds2,alt_test_2$side,I(alt_test_2$score_at_chance^2),
           I(alt_test_2$time_behind^2),I(alt_test_2$score_at_chance*alt_test_2$time_behind),
           I(alt_test_2$odds*alt_test_2$time_behind),I(alt_test_2$odds*alt_test_2$score_at_chance))


caliper2 = c(0.25,0.25,0.25,0.25,100,100,100,100,100,100) 

genout2 = GenMatch(Tr=alt_test_2$treatment,X=X2,caliper=caliper2,
                   pop.size = 500, wait.generations = 15) #run genetic matching

mout2 = Match(Tr=alt_test_2$treatment,X=X2,caliper=caliper2,Weight.matrix = genout2)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance^2)+I(time_behind^2)+I(score_at_chance*time_behind)+
               I(odds*time_behind)+I(odds*score_at_chance),data=alt_test_2,match.out = mout2) 
#
##
#
summary(mout2) #examine number of followers and defiers remained

Ymout2 = Match(Y=alt_test_2$binary_outcome,Tr=alt_test_2$treatment,X=X2,
              caliper=caliper2,Weight.matrix = genout2) #obtain effect

summary(Ymout2)

mean(alt_test_2[mout2$index.treated,'binary_outcome']) # % success in matched follower
mean(alt_test_2[mout2$index.control,'binary_outcome']) # % success in matched defiers




################### Matching for Chance 3 #######################



#Define covariates to match on - presented in Table 2
X3 = cbind(alt_test_3$score_at_chance,alt_test_3$time_behind,
           alt_test_3$odds,alt_test_3$odds2,alt_test_3$side,I(alt_test_3$score_at_chance^2),
           I(alt_test_3$time_behind^2),I(alt_test_3$score_at_chance*alt_test_3$time_behind),
           I(alt_test_3$odds*alt_test_3$time_behind),I(alt_test_3$odds*alt_test_3$score_at_chance))


caliper3 = c(0.25,0.25,0.25,0.25,100,100,100,100,100,100) 

genout3 = GenMatch(Tr=alt_test_3$treatment,X=X3,caliper=caliper3,
                   pop.size = 500, wait.generations = 15) #run genetic matching

mout3 = Match(Tr=alt_test_3$treatment,X=X3,caliper=caliper3,Weight.matrix = genout3)

#
##Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance^2)+I(time_behind^2)+I(score_at_chance*time_behind)+
               I(odds*time_behind)+I(odds*score_at_chance),data=alt_test_3,match.out = mout3) 
#
##
#
summary(mout3) #examine number of followers and defiers remained

Ymout3 = Match(Y=alt_test_3$binary_outcome,Tr=alt_test_3$treatment,X=X3,
               caliper=caliper3,Weight.matrix = genout3) #obtain effect

summary(Ymout3)

mean(alt_test_3[mout3$index.treated,'binary_outcome']) # % success in matched follower
mean(alt_test_3[mout3$index.control,'binary_outcome']) # % success in matched defiers 
  