function(word.list){
  length_vec = rep(0,1,length(word.list))
  for (i in 1:length(word.list)) {
    length_vec[i]= length(word.list[[i]])
  }
  return(length_vec)
}