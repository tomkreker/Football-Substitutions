# ESTIMATING THE EFFECT OF FOLLOWING SUBSTITUTION DECISION RULES USING GENETIC MATCHING

# This script performs the second part of the analysis - estimating the effect of following
# a given decision rule for substitutions, of the form "if behind, perform sub 1/2/3 by 
# minutes T1/T2/T3, respectivey. Conceiving of following the rule as 'treatment', the main 
# function identified instances of treatment and control units, the latter being teams that 
# could have followed the rule (were behind on the relevant minutes) but did not do so. After
# all matches scanned, genetic matching is used to form treatment and control groups that are 
# similar on relevant characteristics - team stength, time behind, score at (potential) subbing
# point, and the decision point in which they started following the rule. After matching, the 
# effect of following the rule can be estimated by the difference of mean outcomes for both groups,
# represented by binary variable that equals '1' if the team outscored their opponent from the moment
# they began following the rule to the end of the game (i.e. they improved their goal differential)
# or '0' otherwise. The treatment effect is the difference in means between the two groups.

########################################################################################

library(Matching)
source('fell_behind.R') #define the function to calcualte when did a team fell behind
                        #used in building the test set


##
###
#### Defining functions to support the main one that find treatment and control units
###
##


#testevents = allevents[-which(allevents$id_odsp %in% random_matches),] #if testing on 1/2 of the data
testevents = allevents #test for all the data

htimes_behind = function(game){
  # Auxiliary function that takes in a game ID and returns an array indexed by minute that
  # has the current score from the home team's perspective for each minute
  # Input: a dataframe of all events from a given game
  # Output: a 100-length array with the hscore (home score) at each minute
  
  score = 0
  goals = game[which(game$is_goal==1),'time']
  sides = game[which(game$is_goal==1),'side']
  sides[which(sides==2)]=-1
  result = rep(0,100)
  score=0
  #return (c(goals,sides))
  if (length(goals)>1){
    for (i in 1:(length(goals)-1)){
      score = score + sides[i]
      result[goals[i]:goals[i+1]]=score
    }  
  }
  if (length(goals)>0){
    result[goals[length(goals)]:100] = score + sides[length(sides)]}
  return(result)
}

hscore_at_t = function(game,t){
  # Returns the score at a given time for the home team in a given match
  return(sum(game$is_goal==1 & game$time<t & game$side==1) -
           sum(game$is_goal==1 & game$time<t & game$side==2))
}

build_test_rule = function(df,t1,t2,t3){
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
    hgoals_t1 = hscore_at_t(game,t1)
    hgoals_t2 = hscore_at_t(game,t2)
    hgoals_t3 = hscore_at_t(game,t3)
    hgoals_fin = hscore_at_t(game,120)
    
    hsubs = game[which(game$event_type==7 & game$side==1),'time'] #an array of sub times
    
    while(length(hsubs)<3){ #add 1 as sub time if team did not make all three subs
      hsubs = c(hsubs,120) #if there was no 3 subs, make sure the array is length 3 with late times
    }
    # number of subs performed before each timepoint
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
    scoreline = htimes_behind(game) #get score for home team at every minute
    
    ######################### Chance point #1 ##########################
    
    # Home team
    
    if (sum(scoreline[1:(t1-0.5)]<0) > 0){ #home team was behind any point before t1 
      fb = (which(scoreline[1:(t1-0.5)] %in% c(-7:-1))) # all minutes in which home was behind
      tb = t1-0.5-fb[1]
      if (hsubs[1] < t1 && scoreline[hsubs[1]]<0 && hsubs[1] > fb[1]){ #home team was behind, subbed after conceding, before t1
        hstart_follow=1  
      }
    }
    if (hgoals_t1 < 0 && tb > 4 && hsubs[1] > t1){ #the home team did not sub on time - add control unit
          hfailed=TRUE #failed to follow - no further checks later
          could = c(could,g) #adding the game ID
          side = c(side,1) #adding the side
          when_follow = c(when_follow,-1) #failed at decision point 1
          score_when_could = c(score_when_could,hgoals_t1) #score at latest point - t1
          time_behind = c(time_behind,t1-0.5-fb[1]) #time the team was behind
          outcome = c(outcome,(hgoals_fin - hgoals_t1)) #outcome from t1 to end
    }
    
    # Away team - exactly the same process using the away variables
  
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
    
    ######################## Chance point #2 ##########################
    # Home team
    
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
        if (hstart_follow==1){ #if the team started following and now failed
          when_follow = c(when_follow,-1)
          score_when_could = c(score_when_could,hgoals_t1)
          tb = sum(scoreline[1:hsubs[1]]<0)
          time_behind = c(time_behind,tb)
          
          }
        else { #if the team did not have a chance at t1 and only could now
          when_follow = c(when_follow,-2)
          score_when_could = c(score_when_could,hgoals_t2)
          time_behind = c(time_behind,t2-0.5-fb[1])
          }
        outcome = c(outcome,(hgoals_fin - hgoals_t2)) 
      }
    
    # Away team
    
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
        outcome = c(outcome,(agoals_fin - agoals_t2))
    }
      
    ############################## Chance point #3 ##########################
    # Home team
    
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
        outcome = c(outcome,(hgoals_fin - hgoals_t3))
      }
    
    # Away team 
    
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
    #Home teamm
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
    # Away team
    
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
    ############# Adding multiple control units for failed - potential extension ###########
    
    #when I create control units, create not only for the failture at t, but when I know
    #they failed I can create a counterfactual for another treatment unit using their situation 2,3,4,5
    #etc. minutes before Tx, as long as they were behind, using the 'time behind' and score for these 
    #minutes to generate more control units (remember the interpretation is follow potential).
    #this is similar to the derivation of the rule idea - just in this case I would consider each 
    #numsub and minute as covariates, and generate controls ONLY with those who had less subs as I wrote
    #in the notebook. This way also adds a nice twist - to suit the inquiry for top-tier questions (what
    #minutes are best for them, maybe wait longer?) I can choose treatment group only with high odds.
    #that could be a nice last section for the analysis.
    #if (hfailed){
    #  
    #}
    #if(afailed){
    #  #same
    #}
    ###################################################################################
  }
      
  #organize relevant columns - treatment and outcome binary indicators
  treatment = rep(0,length(when_follow))
  treatment[which(when_follow>0)]=1 #treatment indicator
  when_follow = abs(when_follow) #decision point for matching - when could the team start follow
  binary_outcome = rep(0,length(outcome))
  binary_outcome[which(outcome>0)]=1
  
  test_rule = data.frame(id_odsp=could,side=side,chance=when_follow,score_at_chance=score_when_could,
                     time_behind=time_behind,treatment=treatment,outcome=outcome,
                     binary_outcome=binary_outcome)
  
  ###
  #### Part 2: Fetch game info
  ###
  test_rule$id_odsp = as.character(test_rule$id_odsp)
  matchinfo = read.csv('ginf.csv')
  games = matchinfo[,c(1,5,6,7,12:14)]
  games = games[which(games$id_odsp %in% test_rule$id_odsp),]
  games$id_odsp = as.character(games$id_odsp)
  odds = c()
  odds2 = c()
  for (i in 1:nrow(test_rule)){
    odds_diff_h =  games[which(games$id_odsp==test_rule[i,'id_odsp']),'odd_h'] - 
      games[which(games$id_odsp==test_rule[i,'id_odsp']),'odd_a']
    if (test_rule[i,'side']==1){
      odds = c(odds, odds_diff_h)
      odds2 = c(odds2, sign(odds_diff_h)*odds_diff_h^2)}
    else{
      odds = c(odds, -1*odds_diff_h)
      odds2 = c(odds2, sign(-1*odds_diff_h)*odds_diff_h^2)}
  }
  test_rule$odds = odds
  test_rule$odds2 = odds2
  
  return(test_rule)
}
#consider changing to ratio

#trial = build_test_rule(testevents[1:100,],64.5,69.5,77.5) #testing the function with a few records
test_rule = build_test_rule(testevents,64.5,69.5,77.5) #build test set based on the decision rule

##
###
#### Genetic Matching
###
##

#Define covariates to match on - presented in Table 2

X = cbind(test_rule$chance,test_rule$score_at_chance,test_rule$time_behind,
          test_rule$odds,test_rule$odds2,test_rule$side,I(test_rule$score_at_chance^2),
          I(test_rule$time_behind^2),I(test_rule$score_at_chance*test_rule$time_behind),
          I(test_rule$odds*test_rule$time_behind),I(test_rule$odds*test_rule$score_at_chance))

caliper = c(0.25,0.25,0.25,0.25,0.25,100,100,100,100,100,100) 
exact = rep(FALSE,length(caliper))
exact[1]=TRUE

genout = GenMatch(Tr=test_rule$treatment,X=X, caliper=caliper,exact=exact,
                   pop.size = 500, wait.generations = 25) #run genetic matching

mout = Match(Tr=test_rule$treatment,X=X,caliper=caliper,exact=exact
             ,Weight.matrix = genout)

#
## Before and after matching balance - data presented in Table 2
#
MatchBalance(treatment~chance+score_at_chance+time_behind+odds+odds2+side+
               I(score_at_chance^2)+I(time_behind^2)+I(score_at_chance*time_behind)+
               I(odds*time_behind)+I(odds*score_at_chance),data=test_rule,match.out = mout) 

summary(mout) #examine number of followers and defiers remained

Ymout = Match(Y=test_rule$binary_outcome,Tr=test_rule$treatment,X=X,caliper=caliper,exact=exact,
              Weight.matrix = genout) #obtain effect when satisfied with the balance

summary(Ymout)

mean(test_rule[mout$index.treated,'binary_outcome']) # % success in matched follower
mean(test_rule[mout$index.control,'binary_outcome']) # % success in matched defiers 

#Sensitivity test
library(rbounds)
psens(Ymout,Gamma=2,GammaInc=0.1) 
hlsens(Ymout,Gamma=2,GammaInc=0.1) 


##
###
#### Testing the Myers' original decision rule on the all relevant matches in the sample
###
##

testevents = allevents
test_myers = Build_test_rule(allevents,57.5,72.5,78.5) #build test-set based on the decision rule
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
