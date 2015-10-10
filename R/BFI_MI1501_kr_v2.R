## BFI
attach(BFI)
E.score<-E1+E2
N.score<-N1+N2
C.score<-C1+C2
A.score<-A1+A2+A3
O.score<-O1+O2+O3
BFI.score<-data.frame(E.score, N.score, C.score, A.score, O.score)
BFI.type.kr<-c("외향성", "신경성", "성실성", "친화성", "개방성")
dimnames(BFI.score)<-list(ID, BFI.type.kr)
BFI.score
criteria<-function(x, a=c(4, 6, 8)){
  ifelse(x<=a[1], "하", ifelse(x<=a[2], "중하", ifelse(x<=a[3], "중상", "상")))
}
a<-list(c(4,6,8), c(4,6,8), c(4,6,8), c(10,12,13), c(8,10,12))
BFI.level<-data.frame(mapply(criteria, BFI.score, a=a))
BFI.level.kr<-data.frame(lapply(BFI.level, function(x) factor(x, levels=c("하", "중하", "중상", "상"), ordered=TRUE)))
dimnames(BFI.level.kr)<-list(ID, BFI.type.kr)
BFI.level.kr
BFI.level.table<-sapply(BFI.level.kr, table)
BFI.level.table
## join with class.roll
BFI.level.kr.2<-data.frame(ID = ID, BFI.level.kr, row.names=1:nrow(BFI.level.kr))
BFI.level.kr.2
BFI.full.kr<-join(class.roll, BFI.level.kr.2, by="ID")
BFI.full.kr
detach()
## MI
A<-matrix(numeric(0), nrow=nrow(MI), ncol=8)
for(j in 1:8) {A[,j]<-MI[,j+1]
for(i in 1:6) {
A[,j]<-A[,j]+MI[, j+1+i*8]
}}
MI.score.kr<-A
MI.names.kr<-c("음악", "신체운동", "논리수학", "공간", "언어", "인간친화", "자기성찰", "자연친화")
dimnames(MI.score.kr)<-list(MI$ID, MI.names.kr)
MI.score.kr
MI.order.kr<-apply(MI.score.kr, 1, order, decreasing = TRUE)
MI.sort.kr<-matrix(MI.names.kr[MI.order.kr], ncol=8, byrow=T, dimnames=list(MI$ID, paste("제",1:8,"순위", sep="")))
MI.sort.kr
## join with class.roll
MI.sort.kr.2<-data.frame(ID = MI$ID, MI.sort.kr, row.names=1:nrow(MI.sort.kr))
MI.sort.kr.2
MI.sort.full.kr<-join(class.roll, MI.sort.kr.2, by="ID")
MI.sort.full.kr
## Merge 2 data frames into one
BFI.MI.full<-data.frame(BFI.full.kr, MI.sort.full.kr[, 3:5])
BFI.MI.full
## ID only
BFI.MI.full.ID<-BFI.MI.full[,-2]
BFI.MI.full.ID
## identify absentees
BFI.present<-which(class.roll$ID %in% BFI$ID)
BFI.absent<-(1:nrow(class.roll))[-BFI.present]
BFI.absent
class.roll$ID[BFI.absent]
class.roll$Name[BFI.absent]
MI.present<-which(class.roll$ID %in% MI$ID)
MI.absent<-(1:nrow(class.roll))[-MI.present]
MI.absent
class.roll$ID[MI.absent]
class.roll$Name[MI.absent]












