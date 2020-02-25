# number of children (unique id's)
# children <- unique(c(resp.f$id,resp.m$id))
# length(children)
# # subtract the 2nd set from the first
#     response of father, not of mother
# f.not.m <- setdiff(resp.f$id,resp.m$id)
# length(f.not.m)
#     response of mother, not of father
# m.not.f <- setdiff(resp.m$id,resp.f$id)
# length(m.not.f)
# # id's of children for whom we have both responses
# m.and.f <- setdiff(setdiff(children,f.not.m),m.not.f)
# length(m.and.f)
# 
# # response of father, where we also have the mother
# f.both <- resp.f[which(resp.f$id %in% m.and.f),]
# m.both <- resp.m[which(resp.m$id %in% m.and.f),]
# dim(f.both)
# dim(m.both)
# both <- merge(f.both,m.both,by='id')
# head(both)
# dim(both)
# t.both <-table(both[,colname.f],both[,colname.m])
# 
# both.test <- mcnemar.test(t.both)
# both.p <- both.test$p.value
# 
# # m <- matrix(c(44,99,359,385),nrow=2)
# # mcnemar.test(m)
# 
# f.singles <- resp.f[-which(resp.f$id %in% m.and.f),]
# colnames(f.singles) <- c('id','response')
# f.singles$parent <- 'father'
# dim(f.singles)
# 
# m.singles <- resp.m[-which(resp.m$id %in% m.and.f),]
# colnames(m.singles) <- c('id','response')
# m.singles$parent <- 'mother'
# dim(m.singles)
# head(f.singles)
# head(m.singles)
# singles <- rbind(f.singles,m.singles)
# head(singles)
# singles.table <- table(singles$response,singles$parent)
# singles.test <- fisher.test(singles.table,simulate.p.value = F)
# #singles.test <- chisq.test(singles.table,simulate.p.value = T)
# singles.p <- singles.test$p.value
# 
# 
# 
# Here is some stuff to work out marital status in the 3 
# groups (m.and.f, f.not.m m.not.f)
# marital.fm <- f.extra[which(f.extra$id %in% m.and.f),]
# marital.fm2<- m.extra[which(m.extra$id %in% m.and.f),]
# 
# marital.f <-  f.extra[which(f.extra$id %in% f.not.m),]
# marital.m <-  m.extra[which(m.extra$id %in% m.not.f),]
# 
# table(marital.fm$fv13_all)
# table(marital.fm2$mv13_all)
# 
# 
# table(marital.f$fv13_all)
# table(marital.m$mv13_all)

#
#mm <- matrix(c(348,0,29,1,15,3,5,2,47,8,32,19),nrow=4)
# all
#mm <- matrix(c(348,30,62,69),nrow=2)
# cases
#mm <- matrix(c(154,11,30,28),nrow=2)
#mm
#fisher.test(mm)
#sum(mm)



f.cases <- getGroup(f,'CASES')
length(unique(f.cases$id))
m.cases <- getGroup(m,'CASES')
length(unique(m.cases$id))
# unique children in cases
length(unique(c(m.cases$id,f.cases$id)))
# father not mother
length(unique(setdiff(f.cases$id,m.cases$id)))
length(unique(setdiff(m.cases$id,f.cases$id)))

## number of responses by parent/controls
f.controls <- getGroup(f,'CONTROLS')
length(unique(f.controls$id))
m.controls <- getGroup(m,'CONTROLS')
length(unique(m.controls$id))
length(unique(c(m.controls$id,f.controls$id)))
# father not mother
f.not.m <- unique(setdiff(f.controls$id,m.controls$id))
length(f.not.m)
m.not.f <- unique(setdiff(m.controls$id,f.controls$id)) 
length(m.not.f)
all.ids <- unique(c(f.controls$id, m.controls$id))
length(all.ids)
length(setdiff(setdiff(all.ids,f.not.m),m.not.f))
