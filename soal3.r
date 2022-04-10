#No. 3
#3A
soal_3a<-function(x, lambd){
  p = dpois(x, lambd)
  cat("3A. Peluang",x,"bayi lahir di rumah sakit besok adalah",p,"\n")
}
soal_3a(6, 4.5)

#3B
soal_3b<-function(x, lambd){
  vector_year<-c()
  for(i in 1:365)vector_year[i]<-i
  vector_pois<-c()
  for(i in 1:365)vector_pois[i]<-dpois(x,lambd)
  plot(vector_year,vector_pois,type='h',main="3B. Distribusi Poisson",
       ylab="Peluang",xlab="Hari",lwd=1)
}
soal_3b(6, 4.5)

#3C
#Nilai peluang dengan distribusi poisson hanya bergantung pada nilai x dan mean
#sehingga tidak bergantung waktu dan peluang dalam satu tahun akan bernilai sama

#3D
soal_3d<-function(lambd){
  rataan=lambd
  varian=lambd
  cat("3D. Rataan:",rataan,", dan varian:",varian,"\n")
}
soal_3d(4.5)
