E.score<-bfi[, 2]+bfi[, 7]
N.score<-bfi[, 6]+bfi[, 11]
C.score<-bfi[, 5]+bfi[, 10]
A.score<-bfi[, 3]+bfi[, 8]+bfi[, 13]
O.score<-bfi[, 4]+bfi[, 9]+bfi[, 12]
bfi.score<-data.frame(E.score, N.score, C.score, A.score, O.score)
E.level.kr<-ifelse(E.score <= 4, "하", ifelse(E.score <=6, "중하", ifelse(E.score <=8, "중상", "상")))
N.level.kr<-ifelse(N.score <= 4, "하", ifelse(N.score <=6, "중하", ifelse(N.score <=8, "중상", "상")))
C.level.kr<-ifelse(C.score <= 4, "하", ifelse(C.score <=6, "중하", ifelse(C.score <=8, "중상", "상")))
A.level.kr<-ifelse(A.score <= 10, "하", ifelse(A.score <=12, "중하", ifelse(A.score <=13, "중상", "상")))
O.level.kr<-ifelse(O.score <= 8, "하", ifelse(O.score <=10, "중하", ifelse(O.score <=12, "중상", "상")))
bfi.level.kr<-data.frame(lapply(data.frame(E.level.kr, N.level.kr, C.level.kr, A.level.kr, O.level.kr), function(x) factor(x, levels=c("하", "중하", "중상", "상"), ordered=TRUE)))
sapply(bfi.level.kr, table)
load("class_roll.rda")
library(plyr)
bfi.level.kr.2<-data.frame(ID = bfi$ID, bfi.level.kr)
bfi.full.kr<-join(class.roll, bfi.level.kr.2, by="ID")
music.score<-MI[, 2]+MI[, 10]+MI[, 18]+MI[, 26]+MI[, 34]+MI[, 42]+MI[, 50]
body.score<-MI[, 3]+MI[, 11]+MI[, 19]+MI[, 27]+MI[, 35]+MI[, 43]+MI[, 51]
logic.score<-MI[, 4]+MI[, 12]+MI[, 20]+MI[, 28]+MI[, 36]+MI[, 44]+MI[, 52]
spatial.score<-MI[, 5]+MI[, 13]+MI[, 21]+MI[, 29]+MI[, 37]+MI[, 45]+MI[, 53]
verbal.score<-MI[, 6]+MI[, 14]+MI[, 22]+MI[, 30]+MI[, 38]+MI[, 46]+MI[, 54]
people.score<-MI[, 7]+MI[, 15]+MI[, 23]+MI[, 31]+MI[, 39]+MI[, 47]+MI[, 55]
self.score<-MI[, 8]+MI[, 16]+MI[, 24]+MI[, 32]+MI[, 40]+MI[, 48]+MI[, 56]
nature.score<-MI[, 9]+MI[, 17]+MI[, 25]+MI[, 33]+MI[, 41]+MI[, 49]+MI[, 57]
MI.score<-data.frame(ID=MI$ID, music.score, body.score, logic.score, spatial.score, verbal.score, people.score, self.score, nature.score)
MI.names.kr<-c("음악", "신체운동", "논리수학", "공간", "언어", "인간친화", "자기성찰", "자연친화")
MI.score.kr<-MI.score
dimnames(MI.score.kr)[[2]]<-c("ID", MI.names.kr)
MI.order.kr<-apply(MI.score.kr[, -1], 1, order, decreasing = TRUE)
MI.sort.kr<-matrix(MI.names.kr[MI.order.kr], ncol=8, byrow=T, dimnames=list(MI.score.kr$ID, 1:8))
present<-which(class.roll$ID %in% bfi$ID)
absent<-(1:58)[-present]
class.roll$ID[absent]
class.roll$name[absent]
MI.present<-which(class.roll$ID %in% MI$ID)
MI.absent<-(1:58)[-MI.present]
MI.absent
class.roll$ID[MI.absent]
class.roll$name[MI.absent]
dimnames(MI.sort.kr)
MI.sort.df.kr<-data.frame(ID = MI$ID, MI.sort.kr, row.names=1:47)
MI.sort.full.kr<-join(class.roll, MI.sort.df.kr, by="ID")
dimnames(MI.sort.full.kr)[[2]][-(1:2)]<-paste(1:8)
bfi.MI.full<-data.frame(bfi.full.kr, MI.sort.full.kr[, 3:5])
bfi.MI.full











