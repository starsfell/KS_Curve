
rm(list=ls())

# 引入library
library(ggplot2)
library(reshape2)
library(ROCR)
library(stringr)

# 引入样本，划分Train与Test
diamonds$is_expensive <- diamonds$price > 2400
is_test <- runif(nrow(diamonds)) > 0.75
train <- diamonds[is_test==FALSE,]
test <- diamonds[is_test==TRUE,]

# 拟合模型
fit_A <- glm(is_expensive ~ carat + cut + clarity, data=train)

# 预测模型
prob_A <- predict(fit_A, newdata=test, type="response")
pred_A <- prediction(prob_A, test$is_expensive)
perf_A <- performance(pred_A, measure = "tpr", x.measure = "fpr")

# 预测值以概率的形式保存在“pred_B@predictions”中
# 真实值以“TRUE”/“FALSE”的形式保存在“pred_B@labels”中
unlist(pred_A@predictions)
unlist(pred_A@labels)

# 首先，我们需要将TRUE/FALSE转化为0/1

df <- data.frame(rep = unlist(pred_A@labels))
# 替换
df$rep=str_replace(df$rep,'TRUE',"1")
df$rep=str_replace(df$rep,'FALSE',"0")

# 之后，需要再将数据转化为integer
# 所以，as.integer(unlist(df$rep))是真实的Y值


# 定义公式
myKS <- function(pre,label){
  true <- sum(label)
  false <- length(label)-true
  tpr <- NULL
  fpr <- NULL
  KS <- NULL
  o_pre <- pre[order(pre)] # let the threshold in an order from small to large
  for (i in o_pre){
    tp <- sum((pre >= i) & label)
    tpr <- c(tpr,tp/true)
    fp <- sum((pre >= i) & (1-label))
    fpr <- c(fpr,fp/false)
    KS <- tpr-fpr
  }
  
  plot(o_pre,tpr,type = "l",col= "green",xlab="threshold",ylab="tpr,fpr")
  lines(o_pre,fpr,type="l", col = "red")
  lines(o_pre,KS,type="l", col = "pink")
  KSvalue <- max(tpr-fpr)
  sub = paste("KS value =",KSvalue)
  title(sub=sub)
  cutpoint <- which(tpr-fpr==KSvalue)
  thre <- o_pre[cutpoint]
  lines(c(thre,thre),c(fpr[cutpoint],tpr[cutpoint]),col = "blue")  
  cat("KS-value:",KSvalue,mean(thre))
}


# 输出结果
myKS(unlist(pred_A@predictions),as.integer(unlist(df$rep)) )

