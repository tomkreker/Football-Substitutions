# DERIVING THE DECISION RULE FOR OPTIMAL SUBSTITUTION TIMINGS

# This script performs the first part of the analysis - deriving the optimal minutes
# for substitutions by fitting decision trees on a processed dataset in which each sub
# has an associated binary outcome variable of whether the team had a positive goal 
# differential from the moment of the sub until the end of the match. After the points
# are derived, I use bootstrapping to obtain an estimation of uncertainty around them.

########################################################################################


###
#### Clean data: remove NAs and games with injuries and red cards
###

allevents = read.csv('data.csv') #import data pre-processed in preprocessing.R

# Alternatively, run
# source('preprocessing.R') 
# which also creates the csv file with the data based on the original 'events.csv'

###
#### Add relevant varirables to derive optimal substitution rules
###

#random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp))/2) #select half the eliginle match on random
#train_set = allevents[which(allevents$id_odsp %in% random_matches),]
sum(allevents$event_type==7) #final num of subs in sample

BuildTraining = function(df){ 
  # Input: dataframe with events of substitutions to be used for fitting trees to find decision points  
  # Output: A dataframe with extra variables that allow to fit trees, including the number of sub (1/2/3),
  # score at time of sub, and a binary variable for whether the team improved goal differential at the end.
  
  numsub=c() #length of subs
  goaldifsub=c() #length of subs
  home_final_scores=rep(NA,length(unique(df$id_odsp))) #length of matches
  names(home_final_scores) = (unique(df$id_odsp))
  hscore=0 #goals scored by home team
  ascore=0
  hsub=1 #subs done by home team
  asub=1 
  
  
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
    if (df[i,'id_odsp']!=df[i-1,'id_odsp']){ #a new game has started -reset counters
      name=(df[i-1,'id_odsp'])
      home_final_scores[name] =  hscore-ascore
      hscore=0
      ascore=0
      asub=1
      hsub=1
    }
    
    if(df[i,'event_type']==7){ #if a sub occurred
      if (df[i,'side']==1){ #home sub
        ## update sub numbers and goal difference
        numsub = c(numsub,hsub)
        goaldifsub = c(goaldifsub,hscore-ascore)
        hsub=hsub+1
      }
      else{ #similarly for away subs
        numsub = c(numsub,asub)
        goaldifsub = c(goaldifsub,ascore-hscore)
        asub=asub+1
      }
    }
    if(df[i,'is_goal']==1){ #if goal, update current score
      if (df[i,'side']==1){hscore = hscore+1}
      else{ascore = ascore+1}
    }
    
  }
  name=(df[i,'id_odsp'])
  home_final_scores[name] =  hscore-ascore #update final score for the final match
  
  subs = df[which(df$event_type==7),] #create the to-be-returned data-frame
  subs$numsub = numsub #add the numsubs column
  subs$goaldifsub= goaldifsub #add the goal difference column
  goaldiffin= rep(0,nrow(subs)) #an array from the outcome: end score - score at sub
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
  goal_diff_improve01[which(subs$goal_diff_improve>0)]=1 #transform outcome to binary
  subs$goal_diff_improve01 = goal_diff_improve01
  
  return (subs)
}

#run the function on the training sample matches to get the subs with the required variables
derive_rule =  BuildTraining(allevents)
remove = which(derive_rule$id_odsp %in% derive_rule[which(derive_rule$numsub>3),'id_odsp']) #few matches with faulty data
derive_rule = derive_rule[-remove,] 
derive_rule$goal_diff_improve01 = as.factor(derive_rule$goal_diff_improve01)

#create subsets to fit trees per substitute category
derive_rule_behind = derive_rule[which(derive_rule$goaldifsub<0),] #only games behind - ~4500 subs
first_derive_rule_behind = derive_rule_behind[which(derive_rule_behind$numsub==1),] #1st subs
mean(as.numeric((first_derive_rule_behind$goal_diff_improve01))-1) #average success

second_derive_rule_behind =derive_rule_behind[which(derive_rule_behind$numsub==2),] #2nd subs
mean(as.numeric((second_derive_rule_behind$goal_diff_improve01))-1) #average success

third_derive_rule_behind = derive_rule_behind[which(derive_rule_behind$numsub==3),] #3rd subs
mean(as.numeric((third_derive_rule_behind$goal_diff_improve01))-1) #average success

###
#### Auxiliary functions to 'mimic' tree activity and obtain p-values similar to Myers
### (as explained in Appendix A)

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


###
#### Fit trees to training set to find potential decision rule
###

library(rpart)
tree1 = rpart(goal_diff_improve01~time,data=first_derive_rule_behind, method='anova')
tree1
mimic = before_after_minute(first_derive_rule_behind)
round(mimic,3)
#
## Figure in Appendix A
#
plot(mimic$time[6:66],mimic$difference[6:66], xlab='Minute',ylab ='Before-After Mean Success Difference',
     main='First Sub Mean Before-After Success by Minute',pch=16)
abline(v=64.5,lty=2)
text('64.5: largest before-after difference',x=48,y=-0.06)
pvalue(first_derive_rule_behind,64.5) #p-value is 0.0001424 - but this is not the correct metric for tree
#
##
#

tree2 = rpart(goal_diff_improve01~time,data=second_derive_rule_behind, method='anova')
tree2 #obtain before and after proportions
pvalue(second_derive_rule_behind,69.5) #p=3.117e-07

tree3 = rpart(goal_diff_improve01~time,data=third_derive_rule_behind, method='anova')
#tree3_best = rtree
tree3 #obtain before and after proportions
pvalue(third_derive_rule_behind,77.5) #p=6.54e-08

###
#### Bootstrapp the derivation of the three decision points
###

bootstrap_splits = function(N=10){
  # Input: number of bootstrap datasets to build
  # The function simulates N simulated datasets and derives three decision points using
  # trees, recording the first splits if there was one.
  # Output: an array containing the three lists with the split results for the bootstrapped datsets
  
  t1s = c()
  t2s = c()
  t3s = c()
  
  for (i in 1:N)
  {
    #bootstrap dataset
    random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp)), replace=T) #select random mathces with replacement
    nRows = 0 #calcualte the number of rows required for the new dataset based on the number of events per match
    for (g in random_matches){
      nRows = nRows + length(which(allevents$id_odsp==g))
    }
    d = as.list(seq_len(nRows))
    index = 1
    for (g in random_matches){
      # for each match, add its rows to the new list of rows 'd'
      minrow = min(which(allevents$id_odsp==g))
      maxrow = max(which(allevents$id_odsp==g))
      for (i in minrow:maxrow){
        d[[index]] = (allevents[i,])
        index = index + 1
      }
    }
    bootstrapped_data = data.frame(do.call(rbind,d), stringsAsFactors = F)
    #bootstrapped_data$id_odsp = as.character(bootstrapped_data$id_odsp)
    
    derive_rule = BuildTraining(bootstrapped_data) #add relevant variables
    remove = which(derive_rule$id_odsp %in% derive_rule[which(derive_rule$numsub>3),'id_odsp']) #few matches with faulty data
    derive_rule = derive_rule[-remove,] 
    derive_rule$goal_diff_improve01 = as.factor(derive_rule$goal_diff_improve01)
    
    # Create subsets to fit trees per substitute category
    derive_rule_behind = derive_rule[which(derive_rule$goaldifsub<0),] #only games behind 
    first_derive_rule_behind = derive_rule_behind[which(derive_rule_behind$numsub==1),] #1st subs
    second_derive_rule_behind =derive_rule_behind[which(derive_rule_behind$numsub==2),] #2nd subs
    third_derive_rule_behind = derive_rule_behind[which(derive_rule_behind$numsub==3),] #3rd subs

    # Fit trees to the bootstrapped training set
    tree1 = rpart(goal_diff_improve01~time,data=first_derive_rule_behind, method='anova')
    tree2 = rpart(goal_diff_improve01~time,data=second_derive_rule_behind, method='anova')
    tree3 = rpart(goal_diff_improve01~time,data=third_derive_rule_behind, method='anova')
    
    #add the first split of each tree to the results
    if (!is.null(tree1$splits[4])){t1s=c(t1s,tree1$splits[4])}
    if (!is.null(tree2$splits[4])){t2s=c(t2s,tree2$splits[4])}
    if (!is.null(tree3$splits[4])){t3s=c(t3s,tree3$splits[4])}
  }
  x = list()
  x[[1]] = t1s
  x[[2]] = t2s
  x[[3]] = t3s
  return(x)
}

results = bootstrap_splits(N=1000)
results_1000 = results
hist(results_1000[[1]][which(results_1000[[1]]>45)],breaks=20)
hist(results_1000[[2]][which(results_1000[[2]]>45)],breaks=20)$counts
hist(results_1000[[3]][which(results_1000[[3]]>45)],breaks=20)