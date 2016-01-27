#--------------------------------------------------------------------------------------------------
#Fonctions
#--------------------------------------------------------------------------------------------------

#cree un nouveau plot
#entree : position des pommes
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

#test si la direction demandé est possible et la rend
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

#test si une position entre en collision avec une liste de position
#entree : liste position, position
#sortie : booléen
testCollision=function(lp,p){
  for(i in lp) {
    if(i$x==p$x && i$y==p$y){
      return(T)
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

#rempli une liste de pomme aléatoirement
#entree : chiffre
#sortie : liste position
initPomme=function(n){
  listElem=list()
  for(i in 1:n){
    listElem[[as.character(i)]]=xy.coords(sample(-10:10,1),sample(-10:10,1))
  }
  return(listElem)
}

#rend une ligne csv du jeu
#entree : liste position, position
#sortie : string
gameToString = function(lp,d,p){
  line=''
  line=paste(d,p$x,p$y,sep="," )
  for (i in lp) {
    line=paste(line,i$x,i$y,sep=",")
  }
  line=paste(line)
  return(line)
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
lp_snake=initSnake(3)
lp_pomme=initPomme(1)
d="d" 
initGrille()
#--------------------------------------------------------------------------------------------------
#Main
#--------------------------------------------------------------------------------------------------

#boucle de test mv
main=function(){
  while ( (! testMur(lp_snake[['0']]) ) & (! testCollision(lp_snake) ) ){
    ##DIRECTION
    #choisir direction
    nouvelleDir = sample(dirVect,1)
    #tester direction
    d = testDir(d,nouvelleDir)
    
    ##MOUVEMENT
    queue=lp_snake[[as.character(length(lp_snake)-1)]]
    #snake entier
    lp_snake = moveCorps(lp_snake)
    #tete
    lp_snake[['0']] = move(lp_snake[['0']],d)
    #allongement si pomme
    
    
    ##TEST CROQUE POMME
    if (sample(c(T,F),1,prob=c(0.2,0.8))){
      lp_snake[[as.character(length(lp_snake))]]=queue
    }
    
    ##SAUVEGARDE ETAT
    
    print(gameToString(lp_snake,d,xy.coords(42,42)))
    
    ##AFFICHAGE
    for (i in names(lp_snake)) {
      #tete noire
      if (i == '0') {
        points(lp_snake[[i]], col = 'black')
      }
      #corps rouge
      else{
        points(lp_snake[[i]], col = 'red')
      }
    }
    points(queue,col='green')
    #pomme
    
    #animation
    Sys.sleep(0.3)
    initGrille()
  }
}

main()

