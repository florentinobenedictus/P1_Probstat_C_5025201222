#No. 6
#6a
soal_6a<-function(n, mean, sd){
  #set.seed(1)
  nums<-rnorm(n,mean,sd)
  X1=-2147483647
  X2=2147483647
  z_scores <- (nums-mean(nums))/sd(nums)
  rata_rata=mean(nums)
  for(i in 1:n){
    #Cari nilai X1 & X2
    if(nums[i]>X1 && nums[i]<rata_rata)X1=nums[i]
    if(nums[i]<X2 && nums[i]>rata_rata)X2=nums[i]
  }
  P_ans=pnorm(X2,mean(nums),sd(nums),lower.tail=TRUE)-pnorm(X1,mean(nums),sd(nums),lower.tail=TRUE)
  cat("Peluang dari",X1,"< x <",X2,"adalah:",P_ans)
  plot(nums,z_scores,ylab="Z-score",xlab="Data Nilai")
}
soal_6a(100, 50, 8)

#6b
soal_6b<-function(n, mean, sd){
  nums=rnorm(n,mean,sd)
  hist(nums,breaks=50,main="5025201222_Florentino Benedictus_Probstat_C_DNHistogram")
}
soal_6b(100, 50, 8)

#6c
soal_6c<-function(n, mean, sd){
  varian=sd^2
  cat("6C. Varian:",varian,"\n")
}
soal_6c(100, 50, 8)
