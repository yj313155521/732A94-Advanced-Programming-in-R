name <- "Jin Yan"
liuid <- "jinya425"

###1.1.1 my_num_vector()
my_num_vector <- function(){
  c(log(11,10),cos(pi/5),exp(pi/3),(1173 %% 7)/19)
   }
#my_num_vector()

###1.1.2 filter_my_vector(x, leq)
filter_my_vector <- function(x,leq){
  x[which(x >= leq)] <- NA
  return(x)
}
#filter_my_vector (x = c(2,9,2,4,102), leq = 4)

###1.1.3 dot_prod(a, b)
dot_prod <- function(a,b){
  amount <- 0
  i <- 1
  length_1 <- length(a)
  while(i <= length_1){
    amount <- amount + a[i] * b[i]
    i <- i + 1
  }
  return(amount)
}
#dot_prod(c(3,1,12,2,4), c(1,2,3,4,5))

###1.1.4 approx_e(N)
approx_e <- function(N){
  
  n <- N
  sum_2 <- 0
  
  while(n > 0){
    amount <- 1
    i <- 1
    
    while(i <= n){
      amount <- i*amount
      i <- i + 1
    }
    
    sum_1 <- 1/amount
    sum_2 <- sum_2 + sum_1
    n <- n - 1
  }
  
  return(sum_2 + 1)
}

#approx_e(N=4) 

###1.2.1 my_magic_matrix()
my_magic_matrix <- function(){
  matrix_1 <- matrix(c(4,9,2,3,5,7,8,1,6),nrow = 3, ncol = 3, byrow = TRUE,dimnames = NULL)
  return(matrix_1)
  
}
#my_magic_matrix()

###1.2.2 calculate_elements(A)
calculate_elements <- function(A){
  a <- nrow(A)
  b <- ncol(A)
  sum_1 <- a * b
  return((sum_1))
}

matrix_2 <- my_magic_matrix()
calculate_elements(matrix_2)

###1.2.3 row_to_zero(A, i)
row_to_zero <- function(A,i){
  A[i,] <- 0
  return(A)
}
#row_to_zero(matrix_2,3)


###1.2.4 add_elements_ to_ matrix(A, x, i, j)
add_elements_to_matrix <- function(A,x,i,j){
  A[i,j] <- A[i,j] + x
  return(A)
  
}
#add_elements_to_matrix(matrix_2,-2,1:3,2:3)

###1.3.1 my_magic_list()
my_magic_list <- function(){
  list_1 <- list(info = "my own list", my_num_vector(), my_magic_matrix())
  
  return(list_1)
}
my_magic_list()

###1.3.2 change_info(x, text)
change_info <- function(x,text){
  x$info <- text
  return(x)
  
}
#change_info(my_magic_list(), text = "Some new info")

###1.3.3 add_note(x, note)
add_note <- function(x,note){
  x[["note"]] <- note
  return(x)
}
add_note(my_magic_list(),"This is a magic list!")

###1.3.4 sum numeric parts(x) UNFINISHED
sum_numeric_parts <- function(x){
  i <- 1
  length_list <- length(x)
  amount <- 0
  #遍历列表里的每一个项目，从而进一步找到列表里面的每一个数字进行加法操作
  while(i <= length_list){
    #找到列表里面的字符串的项目进行跳过操作，从后面项目找到数字进行添加
    if(is.character(x[[i]])){
      i <- i + 1
      next
    }
    #遍历向量和矩阵里面的数字部分，利用amount进行求和
    length_element <- length(x[[i]])
    t <- 1
    
    while(t <= length_element){
      amount <- amount + x[[i]][t]
      t <- t + 1
    }
    
    i <- i + 1
  }
  return(amount)
}

#alist <- my_magic_list()
#sum_numeric_parts(alist[2])


###1.4.1 my_data.frame()
my_data.frame <- function(){
  id <- c(1,2,3)
  name <- c("John","Lisa","Azra")
  income <- c(7.3,0,15.21)
  rich <- c(FALSE,FALSE,TRUE)
  table_1 <- data.frame(id,name,income,rich)
  return(table_1)
}

#my_data.frame()

###1.4.2 sort_head(df, var.name, n)
sort_head <- function(df, var.name, n){
  #print(class(unlist(df[var.name])))
  new_sequence <- order(df[,var.name], decreasing = TRUE)
  data_3 <- df[new_sequence,]
  data_result <- head(data_3,n)
  
  return(data_result)
  
}
#data("iris")
#sort_head(iris,"Petal.Length",5)

###1.4.3 add_median_variable(df, j)
#将对应列向量提取出来

#根据函数中给出的列数 计算中位数数值{将列表所有数据从小到大排列；判断总数奇数还是偶数；奇数的话中位数是坐标是n+1/2 对应的值 偶数的话是n/2 和n/2 +1 坐标对应的值的平均值}

#将向量和中位数进行比较 并进行 大中小的赋值 （用while循环进行操作）
#将赋值好的向量添加到数据框的最后一列

add_median_variable <- function(df,j){
  vector_1 <- df[,j]
  length_vector <- length(vector_1)
  median_1 <- median(vector_1)
  compared_to_median <- c()
  for(i in 1:length_vector){
    if(vector_1[i] == median_1){
      compared_to_median[i] <- "Median"
    }else if(vector_1[i] > median_1){
      compared_to_median[i] <- "Greater"
    }else{
      compared_to_median[i] <- "Smaller"
    }
  }
  df$compared_to_median <- compared_to_median
  return(df)
}
data("ChickWeight")
head(add_median_variable(ChickWeight,1))
add_median_variable(ChickWeight,1)
#data("faithful")
#head(add_median_variable(faithful,1))
#tail(add_median_variable(df = faithful, 2))
#1.4.4 analyze_columns(df,j)
analyze_columns <- function(df,j){
  a <- j[1]
  b <- j[2]
  vector_a <- df[,a]
  vector_b <- df[,b]
  mean_1 <- mean(vector_a)
  mean_2 <- mean(vector_b)
  sd_1 <- sd(vector_a)
  sd_2 <- sd(vector_b)
  median_1 <- median(vector_a)
  median_2 <- median(vector_b)
  vector_1 <- c(mean_1,median_1,sd_1)
  vector_2 <- c(mean_2,median_2,sd_2)
  names(vector_1) <- c("mean","median","sd")
  names(vector_2) <- c("mean","median","sd")
  relation <- cor(vector_a,vector_b)
  matrix_3 <- matrix(c(1,relation,relation,1),2)
  vector_heading <- colnames(df)
  index <- c(vector_heading[a],vector_heading[b])
  rownames(matrix_3) <- index
  colnames(matrix_3) <- index
  list_result <- list(vector_1,vector_2,matrix_3)
  names(list_result) <- c(vector_heading[a],vector_heading[b],"correlation_matrix")
  return(list_result)
}
#data("faithful")
#analyze_columns(df = faithful,1:2)
#data("iris")
#analyze_columns(df = iris,c(1,3))
median(ChickWeight$weight)
print(ChickWeight[,1])

