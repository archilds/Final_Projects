#check availability
check_blank_hard = function(num_point, index, point, player, computer, n){
  i = num_point
  x = point
  if(index == 1)
  {
    left = list(c(x[1]-1, x[2]-1))
    right = list(c(x[1]+i, x[2]+i))
  }
  if(index == 2)
  {
    left = list(c(x[1]-1, x[2]+1))
    right = list(c(x[1]+i, x[2]-i))
  }
  if(index == 3)
  {
    left = list(c(x[1], x[2]-1))
    right = list(c(x[1], x[2]+i))
  }
  if(index == 4)
  {
    left = list(c(x[1]-1, x[2]))
    right = list(c(x[1]+i, x[2]))
  }
  
  left = list(as.double(unlist(left)))
  right = list(as.double(unlist(right)))
  if_left = sum(is.element(left, player),!within_boundary(left, n),is.element(left, computer))
  if_right = sum(is.element(right, player),!within_boundary(right, n), is.element(right,computer))
  
  if(if_left + if_right == 0){return(list(left, right, 2))}
  if(if_left == 0){return(list(left, 1))}
  if(if_right == 0){(return(list(right, 1)))}
  return(0)
}


#Get the avalible spot; if not available , return 0
get_function_hard = function(num, player, computer, n = NULL){
  get_num = lapply(player, judge_hard, num, player, computer, n)
  temp = sapply(get_num, length)
  two_ava = which(temp == 3)
  one_ava = which(temp == 2)
  if(length(two_ava) > 0){
    get_one = sample(1:length(two_ava),1)
    get_list = get_num[[two_ava[get_one]]]
    random = sample(1:2, 1)
    final = get_list[[random]]
    return(final)
  }
  if(length(one_ava) > 0){
    get_one = sample(1:length(one_ava),1)
    get_list = get_num[[one_ava[get_one]]]
    final = get_list[[1]]
    return(final)
  }
  else {return (0)}
}   

#Check if the point is located inside the chessboard
within_boundary = function(x, n){
  x = unlist(x)
  judge_l = x<1
  judge_r = x>n
  if(sum(judge_l)>0 || sum(judge_r)>0){return(0)}
  else{return(1)}
}

##Generate five points based on their x-value and y-value
make_line = function(x,y){
  a = list()
  for(i in 1:length(x)){
    a[[i]] = c(x[i],y[i])
  }
  return(a)
}

#computer play
computer_play_hard = function(player, computer, playlist, n){
  m = n+1
  i = 0
  get_4 = get_function_hard(4, player, computer, n)
  if(!is.list(get_4)){get_3 = get_function_hard(3, player, computer, n)} else{return(get_4)}
  if(!is.list(get_3)){get_2 = get_function_hard(2, player, computer, n)} else{return(get_3)}
  if(!is.list(get_2)){
    repeat{
      spot = vector()
      left = m/2 - i
      right = m/2 +i
      x = rep(left:right,2*i+1)
      y = rep(left:right, each = 2*i+1)
      for(j in 1:length(x)){
        spot[[j]] = paste(list(x[j],y[j]), collapse = ":")
      }
      i = i+1
      index = is.element(spot, playlist)
      temp = which(index == 0)
      if (length(temp)>0) #break when the point had chessman on it
        break
    } }else{return(get_2)}
  random = sample(temp,1)
  return(list(c(x[random],y[random])))
}



#Check if there are num continuous points
judge_hard = function(x, num, player, computer, n = NULL){
  #line1 is a y=x kind of line, the point x is the middle of the five points
  x1 = c(x[1]:(x[1]+num-1))
  x1 = as.double(x1)
  y1 = c(x[2]:(x[2]+num-1))
  y1 = as.double(y1)
  line1 = make_line(x1, y1)
  
  #line2 is a y=-x kind of line, the point x is the middle of the five points
  x2 = x1
  y2 = c(x[2]:(x[2]-num+1))
  line2 = make_line(x2, y2)
  
  #line3 is a horizontal line, the point x is the middle of the five points
  x3 = rep(x[1], num)
  y3 = y1
  line3 = make_line(x3, y3)
  
  #line4 is a vertical line, the point x is the middle of the five points
  x4 = x1
  y4 = rep(x[2], num)
  line4 = make_line(x4, y4)
  
  #save the lines in a list
  line = list(line1, line2, line3, line4)
  
  #check if there are five continuous points in the set
  get_num = list()

  for(i in 1:4){
    if_line = sum(is.element(line[[i]], player))
    if(if_line == num)
    get_num[[i]] = check_blank_hard(num, i, x, player, computer, n)
    else {get_num[[i]] = 0}
  }
  
  temp = sapply(get_num, length)
  two_ava = which(temp == 3)
  one_ava = which(temp == 2)
  
  if(length(two_ava) > 0){
    get_one = sample(1:length(two_ava),1)
    get_list = get_num[[two_ava[get_one]]]
    return(get_list)
  }
  if(length(one_ava) > 0){
    get_one = sample(1:length(one_ava),1)
    get_list = get_num[[one_ava[get_one]]]
    return(get_list)
  }
  return(0)
}