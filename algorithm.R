#clean the memory
rm(list=ls())

# Initialize all the data
strand = read.csv("/home/alibi/Desktop/Long-term/Practice/Completed work/Parallel Webpages/New data/akorda/df_percentage", sep=" ")
# Initialize labeled test_set sample and total lengths of characters
test_set = read.csv("/home/alibi/Desktop/Long-term/Practice/Completed work/Parallel Webpages/New data/akorda/test_set.tagged.upd.renamed.csv", sep=",")
# Load total lengths of characters
for(filename in strand$filename) {
  lengths = read.csv(paste("/home/alibi/Desktop/Long-term/Practice/Completed work/Parallel Webpages/New data/akorda//chunks_output//",as.character(filename),".chunks",sep=""), sep=",")
  strand$L_kz[strand$filename==filename] = sum(lengths$length_kz)
  strand$L_en[strand$filename==filename] = sum(lengths$length_en)
}

# Initialize our main features 
strand$P_d <- strand$alignment_cost/(strand$M+strand$N)
strand$L_d <- (strand$L_kz-strand$L_en)/(strand$L_kz+strand$L_en)

# L_d boundary estimation:
# L_d Subset selection for unbiased mean calculation
subMean=subset(strand, strand$alignment_cost==0)
mu=mean(subMean$L_d)
print(mu)

# initialization of parameters for the while loop
# S<-subset of all pairs with P_d<0.2
S=subset(strand, strand$P_d<0.2)
# threshold, step values can be tuned, delta=1 just to pass first iteration
threshold=0.01
step=0.01
delta=1
# calculating the first number of pairs in the presumably parallel region
tempL1=nrow(S[which(abs(strand$L_d-mu)<threshold),])

# 0.01 threshold for delta may be tuned here
while(delta>0.01){
  # expanding the presumably parallel region by step
  threshold=threshold+step
  # calculating the next number of pairs in the presumably parallel region
  tempL2=nrow(S[which(abs(strand$L_d-mu)<threshold),])
  # calculating the delta=percentage difference in number of pairs between two consequtive parallel regions
  delta=tempL2/tempL1-1
  # setting the next number of pairs for calcuation in the next loop
  tempL1=tempL2
}

# printing the obtained results
print("Threshold for Ld")
print(tempL1)
print(delta)
print(threshold)

# Classification according to decision boundary
for(a in 1:nrow(strand)){
  if(strand$P_d[a]<.2 && abs(strand$L_d[a]-mu)<threshold){
    strand$my_par[a] = 1
  }else{
    strand$my_par[a] = 0
  }
}

# merging strand with test_set in order to do evaluation
m1<-merge(test_set, strand, by.x="filename", by.y = "filename")

# algorithm's performance evaluation in terms of precision recall and F-score
prec=nrow(subset(m1, my_par==1 & is_parallel==1))/(nrow(subset(m1, my_par==1 & is_parallel==1))+nrow(subset(m1, my_par==1 & is_parallel==0)))
rec=nrow(subset(m1, my_par==1 & is_parallel==1))/(nrow(subset(m1, my_par==1 & is_parallel==1))+nrow(subset(m1, my_par==0 & is_parallel==1)))
f=2*prec*rec/(prec+rec)
print(prec)
print(rec)
print(f)

