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

#création matrice changement coordonnees
coor=matrix(c(0,-1,0,1,+1,0,-1,0), byrow=T, nrow=2, dimnames = list(c("x","y"), dirVect))

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

demande=scan(what=character(0), nmax=1, multi.line = F)

move=function(direction){
  x=t$x+coor["x",direction]
  y=t$y+coor["y",direction]
  return(xy.coords(x,y))
}
