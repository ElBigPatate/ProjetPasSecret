#generer grille
plot.new()
plot(42,xlim=c(-10,10),ylim=c(-10,10))

abline(v=(seq(-10,10,1)), col="lightgray", lty="dotted")
abline(h=(seq(-10,10,1)), col="lightgray", lty="dotted")

#création matrice de direction
dirVect=c("z","q","s","d")
dirMatrix=matrix(rep(T,16),4,dimnames = list(dirVect,dirVect))
dirMatrix["z","s"]=F
dirMatrix["s","z"]=F
dirMatrix["d","q"]=F
dirMatrix["q","d"]=F

#entité
t=xy.coords(0,0)
tdir='d'

testMur=function(t){
  if(abs(t$x)>=11 | abs(t$y)>=11){
    return(T)
  }
  return(F)
}

testDir=function(depart, demande){
  if(dirMatrix[demande,depart]){
    return(demande)
  }
  return(depart)
}
