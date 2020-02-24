library(nlme)
#library(lme4)
#library(MASS)
# 
# run countEvents first

table(f$fv13_all)
table(m$mv13_all)

testDifferenceFatherMother <- function(n, group='ALL',f,m) {
  
theFile.f <- f
theFile.m <- m  
if (group == 'CASES') {
  theFile.f <- theFile.f[which(theFile.f$id < 2000),]
  theFile.m <- theFile.m[which(theFile.m$id < 2000),]
}
if (group == 'CONTROLS') {
  theFile.f <- theFile.f[which(theFile.f$id >= 2000),]
  theFile.m <- theFile.m[which(theFile.m$id >= 2000),]
}

dim(theFile.f)
dim(theFile.m)

descs[n]
colname.f <- paste('fv',num.toColnum(n),sep='')
colname.m <- paste('mv',num.toColnum(n),sep='')

head(theFile.f[,colname.f])
head(theFile.m[,colname.m])
resp.f <- theFile.f[,c('id',colname.f)]
resp.m <- theFile.m[,c('id',colname.m)]
resp.f[which(is.na(resp.f[,colname.f])),colname.f] <- 0
resp.m[which(is.na(resp.m[,colname.m])),colname.m] <- 0
table(resp.f[,colname.f])
table(resp.m[,colname.m])
# number of children (unique id's)
children <- unique(c(resp.f$id,resp.m$id))
length(children)
# subtract the 2nd set from the first
#     response of father, not of mother
f.not.m <- setdiff(resp.f$id,resp.m$id)
length(f.not.m)
#     response of mother, not of father
m.not.f <- setdiff(resp.m$id,resp.f$id)
length(m.not.f)
# id's of children for whom we have both responses
m.and.f <- setdiff(setdiff(children,f.not.m),m.not.f)
length(m.and.f)

# response of father, where we also have the mother
f.both <- resp.f[which(resp.f$id %in% m.and.f),]
m.both <- resp.m[which(resp.m$id %in% m.and.f),]
dim(f.both)
dim(m.both)
both <- merge(f.both,m.both,by='id')
head(both)
dim(both)
t.both <-table(both[,colname.f],both[,colname.m])

both.test <- mcnemar.test(t.both)
both.p <- both.test$p.value

# m <- matrix(c(44,99,359,385),nrow=2)
# mcnemar.test(m)

f.singles <- resp.f[-which(resp.f$id %in% m.and.f),]
colnames(f.singles) <- c('id','response')
f.singles$parent <- 'father'
dim(f.singles)

m.singles <- resp.m[-which(resp.m$id %in% m.and.f),]
colnames(m.singles) <- c('id','response')
m.singles$parent <- 'mother'
dim(m.singles)
head(f.singles)
head(m.singles)
singles <- rbind(f.singles,m.singles)
head(singles)
singles.table <- table(singles$response,singles$parent)
singles.test <- fisher.test(singles.table,simulate.p.value = F)
#singles.test <- chisq.test(singles.table,simulate.p.value = T)
singles.p <- singles.test$p.value


# 
# Here is some stuff to work out marital status in the 3 
# groups (m.and.f, f.not.m m.not.f)
marital.fm <- f.extra[which(f.extra$id %in% m.and.f),]
marital.fm2<- m.extra[which(m.extra$id %in% m.and.f),]

marital.f <-  f.extra[which(f.extra$id %in% f.not.m),]
marital.m <-  m.extra[which(m.extra$id %in% m.not.f),]

table(marital.fm$fv13_all)
table(marital.fm2$mv13_all)


table(marital.f$fv13_all)
table(marital.m$mv13_all)

#
mm <- matrix(c(348,0,29,1,15,3,5,2,47,8,32,19),nrow=4)
# all
mm <- matrix(c(348,30,62,69),nrow=2)
# cases
mm <- matrix(c(154,11,30,28),nrow=2)
mm
fisher.test(mm)
sum(mm)


if (group == 'ALL') {

resp.f$parent <- 'father'
colnames(resp.f) <- c('id','response','parent')

resp.m$parent <- 'mother'
colnames(resp.m) <- c('id','response','parent')

resp.f2 <- resp.f
resp.m2 <- resp.m

colnames(resp.f2)
colnames(resp.m2)
all.resp <- rbind(resp.f2,resp.m2)
all.resp$class <- "healthy"
all.resp[which(all.resp$id < 2000),"class"] <- "disease"
head(all.resp)
all.resp$class <- factor(all.resp$class)
all.resp$class <- relevel(all.resp$class,ref='healthy')

all.resp$marital.status <- F
all.resp$c_ses <- NA
for (i in 1:dim(all.resp)[1]){
  id <- all.resp$id[i]
  if (all.resp$parent[i] == "father") {
    marital.status <- f.extra[which(f.extra$id==id),"fv13_all"]
    c_ces <- f[which(f$id==id),"c_ces"]
  } else {
    marital.status <- m.extra[which(m.extra$id==id),"mv13_all"]
    c_ces <- m[which(m$id==id),"c_ces"]
  }
  all.resp$marital.status[i] <- marital.status
  all.resp$c_ces[i] <- c_ces
}
table(all.resp$marital.status)
table(all.resp$c_ces)
all.resp$married <- F
all.resp[which(all.resp$marital.status %in% c(1,2)),"married"] <- T



model1 <- glmer(response ~  class * parent + (1|id),data=all.resp,
             family='binomial')
model2 <- glmer(response ~  class * parent + (1|id),data=all.resp,
                family='binomial')
a <- anova(model2,model1,test='Chisq')
summary(model1)

} else {
  model <- NULL
}

return(list(both.p,singles.p,model=model))

}

groups <- c("ALL","CASES","CONTROLS")

d1 <- testDifferenceFatherMother(n=4,group='ALL',f=f,m=m)
d1$model
mod <- d1$model
summary(mod)





for (n in 1:18) {
  print(n)
  ps <- testDifferenceFatherMother(n)
  print(ps)
}

