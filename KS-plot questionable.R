rm(list=ls())

# 引入library
library(ggplot2)
library(reshape2)
library(ROCR)
library(dplyr)

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


# 将代码封装在函数PlotKS_N里，
# Pred_Var是预测结果，可以是评分或概率形式；
# labels_Var是好坏标签，取值为1或0，1代表坏客户，0代表好客户；
# descending用于控制数据按违约概率降序排列，如果Pred_Var是评分，则descending=0，如果Pred_Var是概率形式，则descending=1；
# N表示在将数据按风险降序排列后，等分N份后计算KS值。


# 注意：由于我们的数据中，真实值Y是分为“TRUE”与“FALSE”，而非1/0.
# 所以在df1$good1与df1$bad1、df2$good2、df2$bad2中要特别注意替换



####################   PlotKS_N ################################
PlotKS_N<-function(Pred_Var, labels_Var, descending, N){
  # Pred_Var is prop: descending=1
  # Pred_Var is score: descending=0
  
  df<- data.frame(Pred=Pred_Var, labels=labels_Var)
  
  if (descending==1){
    df1<-arrange(df, desc(Pred), labels)
  }else if (descending==0){
    df1<-arrange(df, Pred, labels)
  }
  
  df1$good1<-ifelse(df1$labels=="TRUE",1,0)   # 如果实际是True，则给1，否则给0
  df1$bad1<-ifelse(df1$labels=="FALSE",1,0)   # 如果实际是False，则给1，否则给0
  df1$cum_good1<-cumsum(df1$good1)
  df1$cum_bad1<-cumsum(df1$bad1)
  df1$rate_good1<-df1$cum_good1/sum(df1$good1)
  df1$rate_bad1<-df1$cum_bad1/sum(df1$bad1)
  
  if (descending==1){
    df2<-arrange(df, desc(Pred), desc(labels))
  }else if (descending==0){
    df2<-arrange(df, Pred, desc(labels))
  }
  
  df2$good2<-ifelse(df2$labels=="TRUE",1,0)
  df2$bad2<-ifelse(df2$labels=="FALSE",1,0)
  df2$cum_good2<-cumsum(df2$good2)
  df2$cum_bad2<-cumsum(df2$bad2)
  df2$rate_good2<-df2$cum_good2/sum(df2$good2)
  df2$rate_bad2<-df2$cum_bad2/sum(df2$bad2)
  
  rate_good<-(df1$rate_good1+df2$rate_good2)/2
  rate_bad<-(df1$rate_bad1+df2$rate_bad2)/2
  df_ks<-data.frame(rate_good,rate_bad)
  
  df_ks$KS<-df_ks$rate_bad-df_ks$rate_good
  
  L<- nrow(df_ks)
  if (N>L){ N<- L}
  df_ks$tile<- 1:L
  qus<- quantile(1:L, probs = seq(0,1, 1/N))[-1]
  qus<- ceiling(qus)
  df_ks<- df_ks[df_ks$tile%in%qus,]
  df_ks$tile<- df_ks$tile/L
  df_0<-data.frame(rate_good=0,rate_bad=0,KS=0,tile=0)
  df_ks<-rbind(df_0, df_ks)
  
  M_KS<-max(df_ks$KS)
  Pop<-df_ks$tile[which(df_ks$KS==M_KS)]
  M_good<-df_ks$rate_good[which(df_ks$KS==M_KS)]
  M_bad<-df_ks$rate_bad[which(df_ks$KS==M_KS)]
  
  library(ggplot2)
  PlotKS<-ggplot(df_ks)+
    geom_line(aes(tile,rate_bad),colour="pink",size=0.7)+
    geom_line(aes(tile,rate_good),colour="darkblue",size=0.7)+
    geom_line(aes(tile,KS),colour="yellow",size=0.7)+
    
    geom_vline(xintercept=Pop,linetype=2,colour="gray",size=0.6)+
    geom_hline(yintercept=M_bad,linetype=2,colour="pink",size=0.6)+
    geom_hline(yintercept=M_good,linetype=2,colour="darkblue",size=0.6)+
    geom_hline(yintercept=M_KS,linetype=2,colour="yellow",size=0.6)+
    
    annotate("text", x = 0.5, y = 1.05, label=paste("KS=", round(M_KS, 4), "at Pop=", round(Pop, 4)), size=4, alpha=0.8)+
    
    scale_x_continuous(breaks=seq(0,1,.2))+
    scale_y_continuous(breaks=seq(0,1,.2))+
    
    xlab("of Total Population")+
    ylab("of Total Bad/Good")+
    
    ggtitle(label="KS - Chart")+
    
    theme_bw()+
    
    theme(
      plot.title=element_text(colour="gray24",size=12,face="bold"),
      plot.background = element_rect(fill = "gray90"),
      axis.title=element_text(size=10),
      axis.text=element_text(colour="gray35")
    )
  
  result<-list(M_KS=M_KS,Pop=Pop,PlotKS=PlotKS,df_ks=df_ks)
  return(result)
}


# 输入参数，得到模型结果
# 预测值：unlist(pred_B@predictions)
# 真实值：unlist(pred_B@labels)
# descending=0
# N=10000

PlotKS_N(unlist(pred_A@predictions),unlist(pred_A@labels), descending=0, 10000)


# PlotKS_N函数返回的结果为一列表，
# 列表中的元素依次为KS最大值、KS取最大值的人数百分位置、KS曲线对象、KS数据框。

 