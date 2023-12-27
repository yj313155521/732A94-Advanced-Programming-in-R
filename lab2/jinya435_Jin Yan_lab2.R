name <- "Jin Yan"
liuid <- "jinya425"



#1.1.1 sheldon_game(player1, player2)UNFINISHED
sheldon_game <- function(player1, player2){
  if(player1 == "scissors"){
    if(player2 == "paper"| player2 == "lizard"){
      return("player 1 wins!")
    }else if(player1 == player2){
      return("Draw!")
    }else if(player2 == "rock"| player2 == "spock"){
      return("player 2 wins!")
    }else{
      stop()
    }
  }else if(player1 == "paper"){
    if(player2 == "rock"| player2 == "spock"){
      return("player 1 wins!")
    }else if(player1 == player2){
      return("Draw!")
    }else if(player2 == "lizard"| player2 == "scissors"){
      return("player 2 wins!")
    }else{
      stop()
    }
  }else if(player1 == "rock"){
    if(player2 == "scissors"| player2 == "lizard"){
      return("player 1 wins!")
    }else if(player1 == player2){
      return("Draw!")
    }else if(player2 == "spock"| player2 == "Paper"){
      return("player 2 wins!")
    }else{
      stop()
    }
  }else if(player1 == "lizard"){
    if(player2 == "paper"| player2 == "spock"){
      return("player 1 wins!")
    }else if(player1 == player2){
      return("Draw!")
    }else if(player2 == "scissors"| player2 == "rock"){
      return("player 2 wins!")
    }else{
      stop()
    }
  }else if(player1 == "spock"){
    if(player2 == "scissors"| player2 == "rock"){
      return("player 1 wins!")
    }else if(player1 == player2){
      return("Draw!")
    }else if(player2 == "lizard"| player2 == "paper"){
      return("player 2 wins!")
    }else{
      stop()
    }
  }else{
    stop()
  }
}
player1 <- c("rock", "spock")
player2 <- c("lizard", "spock")
sheldon_game(player1[1],player2[1])
#sheldon_game("Rock","Paper")

#1.2.1 my_moving_median()UNFINISHED
my_moving_median <- function(x,n,...){
  if(is.numeric(x) & is.vector(n) & length(n) == 1){
    count_1 <- 1
    vector_result <- c()
    for(i in x){
      if(count_1 <= (length(x) - n)){
        vector_y <- c(x[count_1 : (count_1 + n)])
        median_1 <- median(vector_y,...)
        vector_result[count_1] <- median_1
        count_1 <- count_1 + 1
      }else{
        return(vector_result)
      }
    }
  }else{
    stop()
  }
}





#1.2.2 for_mult_table()
for_mult_table <- function(from,to){
  if(is.vector(from) & (length(from) == 1) & is.vector(to) &(length(to) == 1)){
    vector_1 <- from:to
    length_1 <- length(vector_1)
    length_2 <- length_1 * length_1
    vector_2 <- 1:length_2
    matrix_1 <- matrix(vector_2,nrow = length_1,dimnames = list(vector_1,vector_1))
    sequence_1 <- 1
    for(i in vector_1){
      matrix_1[,sequence_1] <-  i * vector_1
      sequence_1 <- sequence_1 + 1
    } 
    return(matrix_1)
  }else{
    stop()
  }
}
for_mult_table(10,12)

#1.3.1 find_cumsum()
find_cumsum <- function(x,find_sum){
  if(is.numeric(x) && is.numeric(find_sum) && (length(find_sum)) == 1){
    length_1 <- length(x)
    sum_1 <- 0
    i <- 1
    while(i <= length_1){
      if(sum_1 <= find_sum){
        sum_1 <- sum_1 + x[i]
        i <- i + 1
      }else{
        return(sum_1) 
      }
    }
    return(sum_1)
  }else{
    stop()
  }
}

#1.3.2 while_mult_table()
while_mult_table <- function(from,to){
  if(is.vector(from) & (length(from) == 1) & is.vector(to) &(length(to) == 1)){
    vector_1 <- from:to
    length_1 <- length(vector_1)
    length_2 <- length_1 * length_1
    vector_2 <- 1:length_2
    matrix_1 <- matrix(vector_2,nrow = length_1,dimnames = list(vector_1,vector_1))
    sequence_1 <- 1
    while(sequence_1 <= length(vector_1)){
      matrix_1[,sequence_1] <-  vector_1[sequence_1] * vector_1
      sequence_1 <- sequence_1 + 1
    } 
    return(matrix_1)
  }else{
    stop()
  }
}

#1.4.1 repeat_find_cumsum() UNFINISHED
repeat_find_cumsum <- function(x,find_sum){
  if(is.numeric(x) && is.numeric(find_sum) && (length(find_sum)) == 1){
    length_1 <- length(x)
    sum_1 <- 0
    i <- 1
    repeat{
      sum_1 <- sum_1 + x[i]
      i <- i + 1
      if(sum_1 > find_sum |i == (length_1 + 1)){
        break
      }
    }
    return(sum_1)
  }else{
    stop()
  }
}
#1.4.2 repeat_my_moving_median()
repeat_my_moving_median <- function(x,n,...){
  if(is.numeric(x) & is.vector(n) & length(n) == 1){
    count_1 <- 1
    vector_result <- c()
    repeat{
      vector_y <- c(x[count_1 : (count_1 + n)])
      median_1 <- median(vector_y,...)
      vector_result[count_1] <- median_1
      count_1 <- count_1 + 1
      if(count_1 > (length(x) - n)){
        break
      }
    }
    return(vector_result)
  }else{
    stop()
  }
}

#1.5.1 in_environment()
in_environment <- function(env){
  ls(env)
}
env <- search()[length(search())]
env
funs <- in_environment(env)
funs[1:5]



#1.6.1 cov()
cov <- function(X){
  dim_1 <- dim(X)
  length_1 <- dim_1[2]
  vector_result <- c(1:length_1)
  i <- 1
  while(i <= length_1){
    mean_1 <- mean(X[,i])
    sd_1 <- sd(X[,i])
    coefficient_1 <- sd_1 / mean_1
    vector_result[i] <- coefficient_1
    i <- i + 1
  }
  name_vector <- as.vector(unlist(colnames(X)))
  names(vector_result) <- name_vector
  return(vector_result)
}
data(iris)
cov(X = iris[1:4])

#1.7.1 moment()
moment <- function(i){
  if(is.numeric(i)){
    function(vector_1){
      mean_1 <- mean(vector_1)
      sum_1 <- 0
      length_1 <- length(vector_1)
      for(t in 1:length_1){
      sum_1 <- sum_1 + (vector_1[t] - mean_1) ^ i
      }
    return(sum_1/length_1)
    }
  }else{
    stop()
  }
}  

m1 <- moment(1)
m1(1:100)
m2 <- moment(2)
m2(1:100)

