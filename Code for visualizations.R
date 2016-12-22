#clean the memory
rm(list=ls())

# Initialize all the data
strand = read.csv("/home/alibi/Desktop/Long-term/Practice/Completed work/Parallel Webpages/New data/akorda/df_percentage", sep=" ")
# Initialize labeled test_set sample and total lengths of characters
test_set = read.csv("/home/alibi/Desktop/Long-term/Practice/Completed work/Parallel Webpages/New data/akorda/test_set.tagged.upd.renamed.csv", sep="")
# Load total lengths of characters
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Short-term/Current Practice/R/Parallel Webpages/New data/akorda//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_kz)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

# Initialize our main features 
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en)

# L_d visualization
hist(strand$L_d, breaks=30, main="", cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1, ylab="Frequency", xlab=expression(L[d]))
mean(strand$L_d)
median(strand$L_d)

# Subsets of L_d visualization
subMean=subset(strand, strand$P_d==0)
hist(subMean$L_d, breaks=20)
mean(subMean$L_d)
median(subMean$L_d)
subL=subset(strand, strand$P_d<0.2)
subofsubL=subset(subL, abs(subL$L_d-mean(subL$L_d))<0.2)

#Initial data visualization and threshold selection
#plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylab="", xlab=expression(paste("|", L[d], "|")))
par(mar=c(4,5,1.1,0.1))
hist(subofsubL$L_d, breaks=15, main="", cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1, ylab="Frequency", xlab=expression(L[d]))
plot(strand$L_d, strand$P_d, cex.lab=2, cex.axis=1.5, cex.main=1, cex.sub=1, ylab="", xlab=expression(L[d]))
mtext(expression(P[d]), side=2, line=2.2, cex=2)

t=0.2
help(hist)
#Subset selection visualization
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)
hist(strand$L_d, breaks=20, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4, main = paste(""), xlab=expression(L[d]))
