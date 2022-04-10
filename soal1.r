#No. 1
#1A
soal_1a<-function(x, p){
  cat("1A. Peluang penyurvei bertemu", x,"orang yang tidak menghadiri vaksinasi
    sebelum keberhasilan pertama sebesar", dgeom(x, p),"\n")
}
soal_1a(3, 0.2)

#1B
soal_1b<-function(rnd,p,x){
  cat("1B. Mean distribusi geometrik:", mean(rgeom(rnd,p)==x),"\n")
}
soal_1b(10000,0.2,3)

#1C
#Nilai hasil 1A dan 1B tidak jauh berbeda meskipun nilai 1B sedikit
#berubah-ubah akibat adanya faktor random

#1D
soal_1d<-function(x, p, num_x){
  vector_iter<-0:num_x
  vector_ans<-c()
  for(i in -1:num_x)vector_ans[i+1]=dgeom(i, p)
  my_colors<-c()
  for(i in 0:num_x){
    if(i==x+1)my_colors[i]="Red"
    else my_colors[i]="Black"
  }
  plot(vector_iter,vector_ans,type='h',main='Distribusi Geometrik'
       ,ylab="Peluang",xlab="X = ...",col=my_colors, lwd=3)
}
soal_1d(3, 0.2, 20)

#1E
soal_1e<-function(p){
  q=1-p
  rataan=1/p
  varian=q/p^2
  cat("1E. Rataan adalah:", rataan,",dan varian adalah:", varian,"\n")
}
soal_1e(0.2)
