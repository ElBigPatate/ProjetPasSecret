#--------------------------------------------------------------------------------------------------
#Fonctions
#--------------------------------------------------------------------------------------------------

#rend une tete en 0.0 allant vers la droite
#entree : -
#sortie : objet tete
initTete=function(){
  return( list(coord=xy.coords(0,0),
               dir='d') 
        )
}


#cree un nouveau plot
#entree : -
#sortie : -
initGrille=function(){
  plot(42,xlim=c(-10,10),ylim=c(-10,10))
  
  abline(v=(seq(-10,10,1)), col="lightgray", lty="dotted")
  abline(h=(seq(-10,10,1)), col="lightgray", lty="dotted")
}

#test si la position actuelle est sur ou en dehors des bords
#entree : objet tete
#sortie : booléen
testMur=function(t){
  if(abs(t$coord$x)>=11 | abs(t$coord$y)>=11){
    return(T)
  }
  return(F)
}

#test si la direction demandé est possible et rend la 
#entree : caractere, caractere
#sortie : caractere
testDir=function(depart, demande){
  if(dirMatrix[demande,depart]){
    return(demande)
  }
  return(depart)
}

#calcul nouvelle coordonées de la tete selon la direction
#entree : tete
#sortie : objet coordonnées
move=function(t){
  x=t$coord$x+coor["x",t$dir]
  y=t$coord$y+coor["y",t$dir]
  return(xy.coords(x,y))
}

#--------------------------------------------------------------------------------------------------
#Initialisation
#--------------------------------------------------------------------------------------------------

#liste des directions possibles
dirVect=c("z","q","s","d")

#matrice des directions 
dirMatrix=matrix(rep(T,16),
                 nrow=4,
                 dimnames = list(dirVect,dirVect))
#insertion des imcompatibilités
dirMatrix["z","s"]=F
dirMatrix["s","z"]=F
dirMatrix["d","q"]=F
dirMatrix["q","d"]=F


#matrice changement coordonnees selon direction
coor=matrix(c(0,-1,0,1,
              +1,0,-1,0),
            byrow=T,
            nrow=2,
            dimnames = list(c("x","y"), dirVect))

#entité tete
tete=initTete()


#--------------------------------------------------------------------------------------------------
#Main
#--------------------------------------------------------------------------------------------------

#boucle de test mv
for(i in seq(1,100)){
  tete=initTete()
  initGrille()
  while (! testMur(tete)){
    #bouger
    tete$coord=move(tete)
    #afficher point
    points(tete$coord)
    #choisir direction
    nouvelleDir=sample(dirVect,1)
    #tester direction
    tete$dir=testDir(tete$dir,nouvelleDir)
  }
}
