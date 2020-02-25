rm(list=ls())
setwd('~/Documents/dockerty')
f <- read.csv('old/fathmentalh.csv',header=T)
m <- read.csv('old/mothmentalh.csv',header=T)
dim(f)
# extra's, for marital status only
f.extra <- read.csv('fathmentalh.csv',header=T)
m.extra <- read.csv('mothmentalh.csv',header=T)
f.extra <- f.extra[,c('id',"fv13_all")]
m.extra <- m.extra[,c('id',"mv13_all")]
f$type = "CONTROL"
f[which(f$id < 2000),'type'] <- "CASE"
m$type = "CONTROL"
m[which(m$id < 2000),'type'] <- "CASE"
table(f$id<2000,f$type)
table(m$id<2000,m$type)
f <- merge(f,f.extra[, c("id","fv13_all")], by.x="id",by.y='id')
m <- merge(m,m.extra[, c("id","mv13_all")], by.x="id",by.y='id')
f$marital.status <- f$fv13_all
m$marital.status <- m$mv13_all
f$married <- F
f[which(f$marital.status %in% c(1,2)),"married"] <- T
m$married <- F
m[which(m$marital.status %in% c(1,2)),"married"] <- T
f$marital.status <- NULL
m$marital.status <- NULL


dim(f.extra)
dim(m.extra)


colnames(f)
dim(f)
table(f$fv130,useNA='always')
round(prop.table(table(f$fv130,useNA='always')),2)

descs <- c( 'Serious illness or injury',
            'Serious illness or injury in close relative',
            'Death of 1st degree relative',
            'Death in more distant relative',
            'Marital separation',
            'Breaking off steady relationship',
            'Serious problem with close friend, neighbour,..',
            'Unemployment for more than 1 month',
            'Being sacked from your job',
            'Major financial crisis',
            'Problems with police or court appearance',
            'Something valuable lost of stolen',
            'Change of job, including quitting,..',
            'Marriage',
            'Arrival of a baby',
            'Someone else moving in or out of your home',
            'Other major life change',
            'None of the above')

# n should range from [1:18]
num.toColnum<- function(n) {
  stopifnot(n >= 1)
  stopifnot(n <= 18)
  result <- n+129 
  # skip variable 147 (please specify..)
  if (n==18)  { 
    result <- result + 1 
  }
  stopifnot(result >= 130)
  stopifnot( result <= 148)
  return(result)
}

num.toDesc <- function(n) {
  stopifnot(n >= 1)
  stopifnot( n <= 18)
  descs <- c( 'Serious illness or injury',
              'Serious illness or injury in close relative',
              'Death of 1st degree relative',
              'Death in more distant relative',
              'Marital separation',
              'Breaking off steady relationship',
              'Serious problem with close friend, neighbour,..',
              'Unemployment for more than 1 month',
              'Being sacked from your job',
              'Major financial crisis',
              'Problems with police or court appearance',
              'Something valuable lost of stolen',
              'Change of job, including quitting,..',
              'Marriage',
              'Arrival of a baby',
              'Someone else moving in or out of your home',
              'Other major life change',
              'None of the above')
      return(descs[n])  
}

getEventCounts <- function(n,f,m) {
  #father
  colname <- paste('fv',num.toColnum(n),sep='')
  f[which(is.na(f[,colname])),colname] <- 0
  f1 <- table(f[,colname],useNA='always')
  f2 <- round(prop.table(f1),2)
  f3 <- margin.table(f1)
  #mother
  colname <- paste('mv',num.toColnum(n),sep='')
  m[which(is.na(m[,colname])),colname] <- 0
  
  m1 <- table(m[,colname],useNA='always')
  m2 <- round(prop.table(m1),2)
  m3 <- margin.table(m1)
  return(c(round(f1[2],0),
           f2[2],
           round(f1[1],0),
           f2[1],
         m1[2],m2[2],m1[1],m2[1],
         # total father and mother
         f1[2]+m1[2],
         round((f1[2]+m1[2])/(f3[1]+m3[1]),2),
         f1[1]+m1[1],
         round((f1[1]+m1[1])/(f3[1]+m3[1]),2)))
  
}


f.cases <- f[which(f$id < 2000),]
f.controls <- f[which(f$id >= 2000),]
m.cases <- m[which(m$id < 2000),]
m.controls <- m[which(m$id >= 2000),]

e.cases <- getEventCounts(1,f=f.cases,m=m.cases)
e.controls <- getEventCounts(1,f=f.controls,m=m.controls)

getAllEventCounts <- function(f,m) {

  for (i in 1:18) {
    if (i == 1) {
      events <- getEventCounts(1,f=f,m=m)
    } else {
      events <- rbind(events,getEventCounts(i,f=f,m=m))
    }
  }
  return(events)
}


 
#dim(events)
#eventsTable <- table(events)
#eventsTable


table(f$fv130,useNA='always')
 table(f$fv131,useNA='always')
# table(f$fv132,useNA='always')
# table(f$fv133,useNA='always')
# table(f$fv134,useNA='always')
# table(f$fv135,useNA='always')
# table(f$fv136,useNA='always')
# table(f$fv137,useNA='always')
# table(f$fv138,useNA='always')
# table(f$fv139,useNA='always')
# table(f$fv140,useNA='always')
# table(f$fv141,useNA='always')
# table(f$fv142,useNA='always')
# table(f$fv143,useNA='always')
# table(f$fv144,useNA='always')
# table(f$fv145,useNA='always')
# table(f$fv146,useNA='always')
# table(f$fv147,useNA='always')
# table(f$fv148,useNA='always')
# 
# 
# table(f$fv132,useNA='always')
# table(f$fv132,useNA='always')
# table(f$fv132,useNA='always')
# table(f$fv132,useNA='always')
# table(f$fv132,useNA='always')
 
 
 table(m$mv130,useNA='always')
 table(m$mv148,useNA='always')
 
 # 146
 head(f$fv146)
 head(f$fv146)
