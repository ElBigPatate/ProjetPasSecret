#generer grille
plot.new()
plot(42,xlim=c(-10,10),ylim=c(-10,10))

abline(v=(seq(-10,10,1)), col="lightgray", lty="dotted")
abline(h=(seq(-10,10,1)), col="lightgray", lty="dotted")

#entité
t=xy.coords(0,0)
tdir='d'

testMur=function(t){
  if(abs(t$x)>=11 | abs(t$y)>=11){
    return(T)
  }
  return(F)
}


