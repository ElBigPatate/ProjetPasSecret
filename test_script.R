#--------------------------------------------------------------------------------------------------
#Fonctions
#--------------------------------------------------------------------------------------------------

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
  if(abs(t$x)>=11 | abs(t$y)>=11){
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
#entree : objet coordonnées, direction
#sortie : objet coordonnées
move=function(t,d){
  x=t$x+coor["x",d]
  y=t$y+coor["y",d]
  return(xy.coords(x,y))
}

#Déplacement du corps
#entree : liste position
#sortie : liste position
moveCorps=function(lp){
  vecCorps=names(lp)
  invCorps=rev(vecCorps)
  l=length(invCorps)
  for(i in 1:(l-1)){
    # print(paste("Voilà ce que je prend : ", invCorps[i]))
    # print(paste("ce que je met : ", invCorps[i+1]))
    lp[[ invCorps[i] ]] = lp[[ invCorps[i+1] ]]
  }
  return(lp)
}

#test si la tete entre en colision avec le corps
#entree : liste position
#sortie : booléen
testCollision=function(lp){
  
  for(i in seq(1 , length(lp)-1) ) {
    if(lp[[as.character(i)]]$x==lp[["0"]]$x){
      if(lp[[as.character(i)]]$y==lp[["0"]]$y){
        return(T)
      }
    }
  }
  return(F)
}

#creer un objet snake avec un corps de taille n
#entree : chiffre
#sortie : objet snake
initSnake=function(n){
  listElem=list()
  for(i in seq(0,n)){
    listElem[[as.character(i)]]=xy.coords(-i,0)
  }
  return(listElem)
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

#entité
lp=initSnake(5)
d="d"
initGrille()
#--------------------------------------------------------------------------------------------------
#Main
#--------------------------------------------------------------------------------------------------

#boucle de test mv
main=function(){
  while ( (! testMur(lp[['0']]) ) & (! testCollision(lp) ) ){
    
    ##MOUVEMENT
    #snake entier
    lp=moveCorps(lp)
    #tete
    lp[['0']]=move(lp[['0']],d)
    
    ##TEST CROQUE POMME
    
    
    ##AFFICHAGE
    for(i in names(lp)){
      #tete noire
      if(i=='0'){
        points(lp[[i]], col='black')
      }
      #corps rouge
      else{
        points(lp[[i]], col='red')
      }
    }
    #pomme
    
    ##DIRECTION
    #choisir direction
    nouvelleDir=sample(dirVect,1)
    #tester direction
    d=testDir(d,nouvelleDir)
    
    #animation
    Sys.sleep(0.3)
    initGrille()
  }
}

main()
