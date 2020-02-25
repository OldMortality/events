library(lme4)
# 
# run countEvents first


# subsets the file to CASES, CONTROLS
getGroup <- function(theFile,group) {
  result <- NULL
  if (group == 'CASES') {
    result <- theFile[which(theFile$id < 2000),] 
  }
  if (group == 'CONTROLS') {
    result <- theFile[which(theFile$id >= 2000),]
  }
  return(result)
}


# computes sum of answers to survey questions, and 
#  puts those in a column q.sum
addTotalColumn <- function(fi,parent) {
  
  stopifnot(parent %in% c('mother','father'))
  if (parent=='mother') {
    prefix <- 'mv'
  } else {
    prefix <- 'fv'
  }
  
  colnums <- paste0(prefix,130)
  for (i in 2:18) {
    colnums <- c(colnums,paste0(prefix,num.toColnum(i)))
  }
  fi$q.sum <- apply(fi[,colnums],1,sum,na.rm=T)
  return(fi)
}


f <- addTotalColumn(f,parent='father')
m <- addTotalColumn(m,parent='mother')

# get answers for a given question
getDataByQuestion <- function(n,f,m) {
  colname.f <- paste('fv', num.toColnum(n), sep='')
  colname.m <- paste('mv',num.toColnum(n),sep='')
  resp.f <- f[,c('id',colname.f,'type','c_ses','married')]
  resp.m <- m[,c('id',colname.m,'type','c_ses','married')]
  
  # set NA to 0 for response
  resp.f[which(is.na(resp.f[,colname.f])),colname.f] <- 0
  resp.m[which(is.na(resp.m[,colname.m])),colname.m] <- 0 
  
  cols <- c('id','response','type','c_ses','married','parent')
  resp.f$parent <- 'father'
  colnames(resp.f) <- cols
  resp.m$parent <- 'mother'
  colnames(resp.m) <- cols
  all.resp <- rbind(resp.f,resp.m)
  # all.resp$class <- "healthy"
  # all.resp[which(all.resp$id < 2000),"class"] <- "disease"
  # all.resp$class <- factor(all.resp$class)
  # all.resp$class <- relevel(all.resp$class,ref='healthy')
  # group marital status into two groups
  return(all.resp)
}


  
testDifferenceFatherMother <- function(all.resp) { 

  all.resp$c_ses <- factor(all.resp$c_ses)
  model1 <- glmer(response ~   c_ses + class * parent + (1|id),data=all.resp,
             family='binomial')
  # model2 <- glmer(response ~  class * parent + (1|id),data=all.resp,
  #               family='binomial')
  # a <- anova(model2,model1,test='Chisq')
  summary(model1)

  
  return(model1)

}
dim(m)

testByQuestion <- function(n,f,m) {
  q1 <- getDataByQuestion(n,f,m)
  q1$c_ses <- paste0('DEP',all.q$c_ses)
  q1$type <- factor(all.q$type,ordered=F)
  q1$type <- relevel(all.q$type,ref='CONTROL')
  model1 <- glmer(cbind(q1$response) ~  c_ses + parent +  type + (1|id)  ,
                  data=q1,
                family='binomial')
  return(summary(model1))
}

testByQuestion(4,f,m) 

for (i in 1:18) {
  print(paste('==========',i,'==============='))
  
  print(testByQuestion(i,f,m) )
}
  


getDataAllQuestions <- function(f,m) {
  
  colnames <- c('id','q.sum','type','c_ses','married')
  fathers <- f[,colnames]
  fathers$parent <- 'father'
  mothers <- m[,colnames]
  mothers$parent <- 'mother'
  return(rbind(fathers,mothers))  
  
}


mean(getDataAllQuestions(f[which(f$type=='CASE'),],
                         m[which(m$type=='CASE'),])$q.sum)
sqrt(var(getDataAllQuestions(f[which(f$type=='CASE'),],
                         m[which(m$type=='CASE'),])$q.sum))

mean(getDataAllQuestions(f[which(f$type=='CONTROL'),],
                         m[which(m$type=='CONTROL'),])$q.sum)
sqrt(var(getDataAllQuestions(f[which(f$type=='CONTROL'),],
                         m[which(m$type=='CONTROL'),])$q.sum))

mean(getDataAllQuestions(f,m)$q.sum)
sqrt(var(getDataAllQuestions(f,m)$q.sum))

mean(all.q$q.sum)

all.q <- getDataAllQuestions(f=f,m=m)
all.q$c_ses <- paste0('DEP',all.q$c_ses)
colnames(all.q)
dim(all.q)
all.q$type

all.q$type <- factor(all.q$type,ordered=F)
all.q$type <- relevel(all.q$type,ref='CONTROL')
model1 <- glmer(cbind(q.sum,18-q.sum) ~   c_ses + type + parent + (1|id),data=all.q,
                family='binomial')
summary(model1)
model2 <- glmer(cbind(q.sum,18-q.sum) ~   c_ses + (1|id),data=all.q,
                family='binomial')
exp(0.05)

summary(model1)
confint(model1)
plot(model1)
r <- residuals(model1,scale=T)
plot(qqnorm(r))
r <- residuals(model1,type='pearson')
sqrt(mean(r^2))

abline(0,1,col='red')
summary(model1) 
anova(model1,model2,test='Chisq')
 
summary(model1)

ci <- exp(confint(model1))
s <- summary(model1)
round(exp(s$coefficients),2)
round(ci,2)

dim(all.q)
colnames(all.q)

all.resp <- getData(n=4,f,m)
d1 <- testDifferenceFatherMother(theData)



d1$model
mod <- d1$model
summary(mod)

f <- addTotalColumn(f,parent='father')
m <- addTotalColumn(m,parent='mother')

f.cases <- getGroup(f,'CASES')
f.controls <- getGroup(f,'CONTROLS')
m.cases <- getGroup(m,'CASES')
m.controls <- getGroup(m,'CONTROLS')
round(prop.table(table(f.cases$q.sum)),2)
round(prop.table(table(m.cases$q.sum)),2)
round(prop.table(table(f.controls$q.sum)),2)
round(prop.table(table(m.controls$q.sum)),2)


round(prop.table(table(f$q.sum)),2)
round(prop.table(table(m$q.sum)),2)

library(effects)
invlogit<- function(x) {
  return(1/(1+exp(-x)))
}


e <- allEffects(model1,confint=T)
plot(e)

round(invlogit(cbind(e$c_ses$fit,e$c_ses$lower,e$c_ses$upper)),2)
round(invlogit(cbind(e$type$fit,e$type$lower,e$type$upper)),2)
