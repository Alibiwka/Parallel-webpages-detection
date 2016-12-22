#clean the memory and use libraries
#rm(list=ls())

#Read data
strand = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/egov//df_percentage", sep=" ")
test_set = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/egov//test_set.csv", sep=",")
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/egov//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_kz)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

#Features
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en+1)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(L[d]))
#mtext(expression(P[d]), side=2, line=2.2, cex=2)
#t=0.2

#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
#hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))

#L_d boundary estimation
subL=subset(strand, strand$P_d<0.2)

thres=0.01
bor=0.01

thrL=thres
step=thrL
tempL1=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
border=1

deltaEgov<-0
i<-0

while(border>bor){
  thrL=thrL+step
  tempL2=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
  border=tempL2/tempL1-1
  tempL1=tempL2
  
  deltaEgov[i]<-border
  i=i+1
  
}






#Read data
strand = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/akorda//df_percentage", sep=" ")
test_set = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/akorda//test_set.tagged.upd.renamed.csv", sep=",")
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/akorda//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_kz)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

#Features
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en+1)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(L[d]))
#mtext(expression(P[d]), side=2, line=2.2, cex=2)
#t=0.2

#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
#hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))

#L_d boundary estimation
subL=subset(strand, strand$P_d<0.2)

thres=0.01
bor=0.01

thrL=thres
step=thrL
tempL1=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
border=1

deltaAkorda<-0
j<-0

while(border>bor){
  thrL=thrL+step
  tempL2=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
  border=tempL2/tempL1-1
  tempL1=tempL2
  
  deltaAkorda[j]<-border
  j=j+1
}



#Read data
strand = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/mfa//df_percentage", sep=" ")
test_set = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/mfa//mfa.output", sep=",")
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/mfa//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_kz)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

#Features
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en+1)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(L[d]))
#mtext(expression(P[d]), side=2, line=2.2, cex=2)
#t=0.2

#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
#hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))

#L_d boundary estimation
subL=subset(strand, strand$P_d<0.2)

thres=0.01
bor=0.01

thrL=thres
step=thrL
tempL1=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
border=1

deltaMfa<-0
k<-0

while(border>bor){
  thrL=thrL+step
  tempL2=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
  border=tempL2/tempL1-1
  tempL1=tempL2
  
  deltaMfa[k]<-border
  k=k+1
}



#Read data
strand = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/pm.gc.ca//df_percentage", sep=" ")
test_set = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/pm.gc.ca//pm.gc.ca.output", sep="")
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/pm.gc.ca//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_fr)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

#Features
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en+1)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(L[d]))
#mtext(expression(P[d]), side=2, line=2.2, cex=2)
#t=0.2

#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
#hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))

#L_d boundary estimation
subL=subset(strand, strand$P_d<0.2)

thres=0.01
bor=0.01

thrL=thres
step=thrL
tempL1=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
border=1

deltaPm<-0
l<-0

while(border>bor){
  thrL=thrL+step
  tempL2=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
  border=tempL2/tempL1-1
  tempL1=tempL2
  
  deltaPm[l]<-border
  l=l+1
}


#Read data
strand = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/presidencia//df_percentage", sep=" ")
test_set = read.csv("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/presidencia//test_set.csv", sep=",")
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/presidencia//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_pt)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

#Features
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en+1)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(L[d]))
#mtext(expression(P[d]), side=2, line=2.2, cex=2)
#t=0.2

#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
#hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))

#L_d boundary estimation
subL=subset(strand, strand$P_d<0.2)

thres=0.01
bor=0.01

thrL=thres
step=thrL
tempL1=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
border=1

deltaPres<-0
m<-0

while(border>bor){
  thrL=thrL+step
  tempL2=nrow(subL[which(abs(strand$L_d-mu)<thrL),])
  border=tempL2/tempL1-1
  tempL1=tempL2
  
  deltaPres[m]<-border
  m=m+1
}



plot(deltaAkorda, xlab="Iterations", ylab="Delta values", ylim=range(c(0,0.5)), xlim=range(c(1,22)),cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1)
lines(deltaAkorda, col="blue")

par(new = TRUE)
#plot(deltaEgov, xlab="", ylab="", ylim=range(c(0,0.5)), xlim=range(c(1,22)))
lines(deltaEgov, col="red")

par(new = TRUE)
plot(deltaMfa, xlab="", ylab="", ylim=range(c(0,0.5)), xlim=range(c(1,22)),cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1)
lines(deltaMfa, col="green")

par(new = TRUE)
plot(deltaPm, xlab="", ylab="", ylim=range(c(0,0.5)), xlim=range(c(1,22)),cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1)
lines(deltaPm, col="yellow")

par(new = TRUE)
plot(deltaPres, xlab="", ylab="", ylim=range(c(0,0.5)), xlim=range(c(1,22)),cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1)
lines(deltaPres, col="violet")

segments(1, 0.01, 22,  0.01, col="black")

#makePlot()
legend("topright", legend=c("akorda.kz", "egov.kz", "mfa.kz", "presidencia.pt", "pm.gc.ca"),
       col=c("blue", "red", "green", "yellow", "violet"), lty=1, cex=1.4)

temp <- locator(1) # On the chart, click where you would like the text to appear
text(temp,"akorda.kz", col="blue")
text(temp,"egov.kz", col="red")
text(temp,"mfa.kz", col="green")
text(temp,"presidencia.pt", col="yellow")
text(temp,"pm.gc.ca", col="violet")

Corner_text <- function(text, location="topright"){
  legend(location, legend=text, bty ="n", pch=NA) 
}
