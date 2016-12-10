### CUTOFF OF 10 with N = 8
dta = read.csv("home_games_with_cutoff.csv")
dta_no_cutoff = read.csv("home_games_with_no_cutoff.csv")

#season1
train = dta[which(dta$season_id < 22010 & dta$season_id >= 22005),]
validate = dta_no_cutoff[which(dta_no_cutoff$season_id >= 22010 & dta_no_cutoff$season_id <= 22012),]
test = dta_no_cutoff[which(dta_no_cutoff$season_id == 22015),] #& dta_no_cutoff$season_id <= 22015),]
attach(dta)
attach(season1)

#Initial Eda for Modeling
all.vars = cbind(plus_minus,home_win_pct,away_win_pct,home_avg_pt_diff,away_avg_pt_diff,
                home_win_pct_N,away_win_pct_N,away_win_pct_as_away,home_win_pct_as_home,
                home_back_to_back,away_back_to_back)
all.names =  c("plus_minus","home_win_pct","away_win_pct","home_avg_pt_diff","away_avg_pt_diff",
               "home_win_pct_N","away_win_pct_N","away_win_pct_as_away","home_win_pct_as_home",
               "home_back_to_back","away_back_to_back")
uni.eda = function(data,vars,label){
  predvars = vars
  
  n.row = ceiling(sqrt(ncol(predvars)+1))
  par(mfrow=c(3,4))
  for(i in 1:ncol(predvars)){
    #if (class(data[,i]) == "integer" | class(data[,i]) == "numeric"){ #continuous predictors
      hist(predvars[,i],xlab = label[i],main=label[i],
           col="light gray",cex.main=1.25,cex.lab=1.20)
    #}
    #else{ #categorical predictors
    #  plot(dta[,i],xlab = label[i],main=label[i],
    #       col="dark gray",cex.main=1.25,cex.lab=1.2)
    #}
  }
}
uni.eda(fin,all.vars,all.names)

###Multivariate EDA
check_cors = function(vars) {
  pairs(vars);cor(vars)
  source("panelfxns.R")
  pairs(vars,upper.panel = panel.smooth,lower.panel = panel.cor)
}
check_cors(all.vars)
init_lm = lm(plus_minus~home_avg_pt_diff+
               away_avg_pt_diff+home_win_pct_N+away_win_pct_N+
               away_win_pct_as_away+home_win_pct_as_home+
               home_back_to_back+away_back_to_back+home_win_pct*away_back_to_back)
summary(init_lm)


## Initial Diagnostics
par(mfrow=c(1,1))
qqnorm(init_lm$res,main="Q-Q Plot for Number of Shares")
qqline(init_lm$res,col="red")
library(MASS)
plus_minus.t = plus_minus + abs(min(plus_minus)) + 1
check = boxcox(plus_minus.t~home_win_pct+away_win_pct+home_avg_pt_diff+
                 away_avg_pt_diff+home_win_pct_N+away_win_pct_N+
                 away_win_pct_as_away+home_win_pct_as_home+
                 home_back_to_back+away_back_to_back+home_win_pct*away_back_to_back)
# additional ideas
step(init_lm,direction="backward")
y = lm(formula = plus_minus ~ home_avg_pt_diff + away_avg_pt_diff + 
         home_win_pct_N + away_win_pct_N + home_win_pct_as_home + 
         home_back_to_back + away_back_to_back)
summary(y)

####
accuracy_check = function(lm,data){ #takes in a linear model and dataframe
  target_season = data #data[which(data$season_id == 22015),]
  print(nrow(target_season))
  names = as.character(unique(target_season$team_abbreviation))
  total = 0
  for(i in 1:length(names)){
    if (names[i] %in% target_season$team_abbreviation) {
      season.dal.test = target_season[which(target_season$team_abbreviation == names[i]),]
      predicts = ifelse(predict(lm,season.dal.test) >= 0,1,0)
      #print(length(predicts))
      win.loss = ifelse(season.dal.test$wl=="W",1,0)
      compare = predicts==win.loss
      #print(paste(names[i],toString(sum(compare)/length(predicts))))
      total = total + sum(compare)/length(predicts)
    }
  }
  paste("Avg Accuracy Rate",toString(total/length(names)))
}
####
#Model - Naive
naive_lm = lm(plus_minus~home_win_pct + away_win_pct,data=season1)
summary(naive_lm)
#Model - all variables
init_lm = lm(plus_minus~home_avg_pt_diff+
               away_avg_pt_diff+home_win_pct_N+away_win_pct_N+
               away_win_pct_as_away+home_win_pct_as_home+
               home_back_to_back+away_back_to_back,data=season1)#+home_win_pct*away_back_to_back)
summary(init_lm)
#Model - Special case 1
new_model1 = lm(plus_minus~home_avg_pt_diff+
                  away_avg_pt_diff+home_win_pct_N+away_win_pct_N+
                  home_win_pct_as_home+
                  home_back_to_back+away_back_to_back+home_win_pct*away_back_to_back,data=season1)
summary(new_model1)

home_avg_pt_diff_norm = (home_avg_pt_diff - mean(home_avg_pt_diff))/sd(home_avg_pt_diff)
away_avg_pt_diff_norm = (away_avg_pt_diff - mean(away_avg_pt_diff))/sd(hom_avg_pt_diff)
#Model - Special case 2
home_avg_pt_diff = ifelse(train$home_avg_pt_diff >= 0,1,0)
away_avg_pt_diff = ifelse(train$away_avg_pt_diff <= 0,1,0)
new_model2 = lm(plus_minus~home_win_pct+away_win_pct+home_win_pct_N+away_win_pct_N+
                  home_back_to_back+away_back_to_back+home_win_pct_as_home+
                  away_win_pct_as_away+away_back_to_back:away_win_pct_as_away,data=train)#+home_avg_pt_diff+away_avg_pt_diff,data=train)
summary(new_model2)

plot(new_model2$fit, new_model2$res)
accuracy_check(naive_lm,dta_no_cutoff)
accuracy_check(init_lm,dta_no_cutoff)
accuracy_check(new_model1,dta_no_cutoff)
accuracy_check(naive_lm,dta)
accuracy_check(new_model2,validate)

###Outlier Removal (Code from Rebecca Nugent (36-401))
back_to_back_interact = train$away_back_to_back*train$away_win_pct_as_away
X<- cbind(1,train$home_win_pct+train$away_win_pct+train$home_win_pct_N+train$away_win_pct_N+
            train$home_back_to_back+train$away_back_to_back+train$home_win_pct_as_home+
            train$away_win_pct_as_away+back_to_back_interact)
H<-X%*%solve(t(X)%*%X)%*%t(X) 
n<-nrow(X)
p<-ncol(X)
SSE<-sum(new_model2$res^2) 
MSE<-SSE/(n-p)
res<-new_model2$res 
del.res<-res*sqrt((n-p-1)/(SSE*(1-diag(H))-res^2)) 
#sort(abs(del.res))[1:10]
out.cand = sort(abs(del.res))[(n-10):n] #abs for two sided comp.
alpha<-0.2
cutoff = qt(1-alpha/(2*n),n-p-1)

mean.h<-p/n 
which(diag(H)>2*mean.h) 
u = sort(diag(H))[(n-10):n]
(u - mean(u))/sd(u)
order(diag(H))[(n-10):n]

train = train[-2492,]

### logistic regression
wl = ifelse(train$wl == "W",1,0)
log_model2 = glm(wl~home_win_pct+away_win_pct+home_win_pct_N+away_win_pct_N+
                  home_back_to_back+away_back_to_back+home_win_pct_as_home+
                  away_win_pct_as_away+away_back_to_back:away_win_pct_as_away,data=train,family=binomial(link="logit"))
summary(log_model2)
accuracy_check(log_model2,validate)


#distance from previous game