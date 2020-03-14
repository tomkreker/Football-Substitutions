opp = function(n){
  if (n==1){return (2)}
  if (n==2){return (1)}
}

hscore_at_t = function(game,t){
  return(sum(game$is_goal==1 & game$time<t & game$side==1) -
    sum(game$is_goal==1 & game$time<t & game$side==2))
}

htimes_behind = function(game){
#input - dataframe of game events
#output - a 100-length array with the hscore (home score) at each minute

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

fell_behind = function(game, t, side){
#return a list of two items:
  #1 - the first time in which the team fell behind, before time t
  #2 - the first sub done after #1 - after trailing
  #######if there was no sub after FB, the 2nd item is NA
  #3 - the score at the time of the first sub done after this point

  
  #if team scored zero goals by this time
  if( sum(game$is_goal==1 & game$time<t & game$side==side) == 0){
    fb = sort(game[which(game$is_goal=='1' & game$time < t & game$side==opp(side)),'time'],
                          decreasing=F)[1]
  }
  else{
    timings = sort(game[which(game$is_goal=='1' & game$time < t & game$side==side),'time'],
                   decreasing=TRUE)
    flag = FALSE
    i = 1
    score_after_last_goal_scored = sum(game$is_goal==1 & game$time<(timings[1]+0.5) & game$side==side) -
      sum(game$is_goal==1 & game$time<(timings[1]+0.5) & game$side==opp(side))
    while (!flag){
      if (score_after_last_goal_scored > 0)  #were leading
      {
        fb = sort(game[which(game$is_goal=='1' & game$time < t & game$time > timings[i] & game$side==opp(side)),'time'],
                  decreasing=F)[score_after_last_goal_scored+1]
        flag = T
          
      }
      else{
        if(score_after_last_goal_scored == 0)  #were tied
        {
          fb = sort(game[which(game$is_goal=='1' & game$time < t & game$time > timings[i] & game$side==opp(side)),'time'],
                    decreasing=F)[1]
          flag = T
          
        }
        else{ #trailing after last goal
          i = i + 1 #move on to an earlier goal
          if (i > length(timings)){ #no more goals left
            flag = T
            fb = sort(game[which(game$is_goal=='1' & game$time < t & game$side==opp(side)),'time'],
                      decreasing=F)[1]
          }
          else{
            score_after_last_goal_scored = sum(game$is_goal==1 & game$time<(timings[i]+0.5) & game$side==side) -
              sum(game$is_goal==1 & game$time<(timings[i]+0.5) & game$side==opp(side))
            
          }
        }
      } 
    }
    
  }
  subs = sort(game[which(game$event_type=='7' & game$time < t & game$side==side),'time'],
                    decreasing=F)
  first_sub_after = sort(game[which(game$event_type=='7' & game$time < t & game$time > fb & game$side==side),'time'],
                         decreasing=F)[1]
  goals_s1 = sum(game$is_goal==1 & game$time<first_sub_after & game$side==side) -
    sum(game$is_goal==1 & game$time<first_sub_after & game$side==opp(side))
  goals_s2 = sum(game$is_goal==1 & game$time<subs[2] & game$side==side) -
    sum(game$is_goal==1 & game$time<subs[2] & game$side==opp(side))
  goals_s3 = sum(game$is_goal==1 & game$time<subs[3] & game$side==side) -
    sum(game$is_goal==1 & game$time<subs[3] & game$side==opp(side))
  
  return (c(fb,first_sub_after,goals_s1))
}

#g = testevents[2000,"id_odsp"]
#game = testevents[which(testevents$id_odsp=='Wn69eU5B/'),]
#print(fell_behind(game,79.5,1))
#htimes_behind(game)
