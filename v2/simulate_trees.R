# Bootstrapp the derivation of the three minutes.
#source('900k - fixed test.R') need to fix the structure 
library(rpart)

simulate_find_splits = function(N=100){
  t1s = c()
  t2s = c()
  t3s = c()
  
  for (i in 1:N)
  {
    #randomly choose 1/2 of all matches  
    random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp))/2) #select half the eliginle match on random
    train_set = allevents[which(allevents$id_odsp %in% random_matches),]
    train_set_testing = train_set
    train_set_testing$id_odsp = as.character(train_set_testing$id_odsp)
    
    train_with_variables = BuildTraining(train_set_testing)
    remove = which(train_with_variables$id_odsp %in% train_with_variables[which(train_with_variables$numsub>3),'id_odsp']) #few matches with faulty data
    train_with_variables = train_with_variables[-remove,] 
    train_with_variables$goal_diff_improve01 = as.factor(train_with_variables$goal_diff_improve01)
    
    #create subsets to fit trees per substitute category
    train_behind = train_with_variables[which(train_with_variables$goaldifsub<0),] #only games behind - ~4500 subs
    first_train_behind = train_behind[which(train_behind$numsub==1),] #1st subs
    second_train_behind =train_behind[which(train_behind$numsub==2),] #2nd subs
    third_train_behind = train_behind[which(train_behind$numsub==3),] #3rd subs
    
    ##
    ###
    #### Fit trees to training set to find potential decision rule
    ###
    ##
    
    tree1 = rpart(goal_diff_improve01~time,data=first_train_behind, method='anova')
    tree2 = rpart(goal_diff_improve01~time,data=second_train_behind, method='anova')
    tree3 = rpart(goal_diff_improve01~time,data=third_train_behind, method='anova')
    
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

results = simulate_find_splits(N=1000)
hist(results[[1]],breaks = 20)
hist(results[[2]],breaks=20)
hist(results[[3]],breaks=20)



#new attempt - bootstrap

bootstrap_splits = function(N=10){
  t1s = c()
  t2s = c()
  t3s = c()
  
  for (i in 1:N)
  {
    #bootstrap dataset
    random_matches = sample(unique(allevents$id_odsp),length(unique(allevents$id_odsp)), replace=T) #select half the eliginle match on random
    nRows = 0
    for (g in random_matches){
      nRows = nRows + length(which(allevents$id_odsp==g))
    }
    d = as.list(seq_len(nRows))
    index = 1
    for (g in random_matches){
      minrow = min(which(allevents$id_odsp==g))
      maxrow = max(which(allevents$id_odsp==g))
      for (i in minrow:maxrow){
        d[[index]] = (allevents[i,c(1,4,6,8,17)])
        index = index + 1
      }
    }
    train_set_testing = data.frame(do.call(rbind,d), stringsAsFactors = F)
    #train_set_testing$id_odsp = as.character(train_set_testing$id_odsp)
    
    train_with_variables = BuildTraining(train_set_testing)
    remove = which(train_with_variables$id_odsp %in% train_with_variables[which(train_with_variables$numsub>3),'id_odsp']) #few matches with faulty data
    train_with_variables = train_with_variables[-remove,] 
    train_with_variables$goal_diff_improve01 = as.factor(train_with_variables$goal_diff_improve01)
    
    #create subsets to fit trees per substitute category
    train_behind = train_with_variables[which(train_with_variables$goaldifsub<0),] #only games behind 
    first_train_behind = train_behind[which(train_behind$numsub==1),] #1st subs
    second_train_behind =train_behind[which(train_behind$numsub==2),] #2nd subs
    third_train_behind = train_behind[which(train_behind$numsub==3),] #3rd subs
    
    ##
    ###
    #### Fit trees to training set to find potential decision rule
    ###
    ##
    
    tree1 = rpart(goal_diff_improve01~time,data=first_train_behind, method='anova')
    tree2 = rpart(goal_diff_improve01~time,data=second_train_behind, method='anova')
    tree3 = rpart(goal_diff_improve01~time,data=third_train_behind, method='anova')
    
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
