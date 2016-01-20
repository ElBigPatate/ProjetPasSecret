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

#Déplacement du corps
#entree : objet corps, objet tete
#sortie : objet corps
moveCorps=function(corps, tete){
  vecCorps=names(corps)
  invCorps=rev(vecCorps)
  l=length(invCorps)
  for(i in 1:(l-1)){
    # print(paste("Voilà ce que je prend : ", invCorps[i]))
    # print(paste("ce que je met : ", invCorps[i+1]))
    corps[[ invCorps[i] ]] = corps[[ invCorps[i+1] ]]
  }
  corps[[ vecCorps[1] ]]=tete$coord
  return(corps)
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
corps=list(A=xy.coords(-1,0), 
           B=xy.coords(-2,0),
           C=xy.coords(-3,0))

#--------------------------------------------------------------------------------------------------
#Main
#--------------------------------------------------------------------------------------------------

#boucle de test mv

tete=initTete()
initGrille()
while (! testMur(tete)){
  #bouger
  tete$coord=move(tete)
  #afficher point
  points(tete$coord)
  #afficher corps
  for(i in corps){
    points(i, col='red')
  }
  #choisir direction
  nouvelleDir=sample(dirVect,1)
  #tester direction
  tete$dir=testDir(tete$dir,nouvelleDir)
  #position du corps au prochain tour
  corps=moveCorps(corps, tete)
  Sys.sleep(0.3)
  initGrille()
}
