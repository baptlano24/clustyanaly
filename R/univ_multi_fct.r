
#' Create an object "ClusteringData"
#' @import ggplot2
#' @import FactoMineR
#' @import gridExtra
#' @import ggrepel
#' @import stringr
#'
#' @param df a data frame (quantitative variable with type 'numeric')
#' @param groupe a vector with the value of the group for each individual
#'
#' @return an object of "ClusteringData" class
#' @export
#' @examples
#' \dontrun{
#' objet<-data_manager(apb,apb_class)}
#'
data_manager <- function(df,groupe) {
  #Col quanti : type numeric (as.numeric) col quali : type character ou integer (as.charcter/as.factor)
  if (typeof(groupe)=="list") {
    groupe=unlist(groupe)
  }
  #if there aren't the same number of individuals between group variable and data
  if (length(groupe) != length(df[,1])) {
    stop("Different length !")
  }
  ##Separation of qualitative and quantitative variables
  instance=list()
  #Instantiating dataframes, indices, and col name vectors
  data_quali=data.frame(df[,1])
  data_quanti=data.frame(df[,1])
  j=1
  k=1
  colquali<-c()
  colquanti<-c()
  #BLoop that will store each variable in different data frames according to the type of variables (character/integer or double)
  for (i in 1:length(df[1,])) {
    #Selection of qualitative variable :
    if (typeof(df[,i])=="character" | typeof(df[,i])=="integer") {
      data_quali[,j]<-df[,i]
      #retrieving column names
      colquali<-c(colquali,colnames(df)[i])
      j=j+1

      #Selection of quantitative variable :
    } else if (typeof(df[,i])=="double") {
      data_quanti[k]<-df[,i]
      #retrieving column names
      colquanti<-c(colquanti,colnames(df)[i])
      k=k+1
    }
  }
  #Assignment of respective column names
  colnames(data_quanti)<-colquanti
  colnames(data_quali)<-colquali
  #assignment of the output list
  instance$data_quanti<-data_quanti
  instance$data_quali<-data_quali
  instance$group<-groupe
  class(instance)<- "ClusteringData"

  return(instance)
}


#' Graph on classified data
#' @import ggplot2
#' @import FactoMineR
#' @import gridExtra
#' @import ggrepel
#' @import stringr
#'
#' @param objet object of 'ClusteringData' class
#' @param choice_graph vector for choose the graph with qualitative or quantitatives data, by default there are both
#'
#' @return some ggplot2 graph
#' @export
#'
#' @examples
#' \dontrun{
#' objet<-data_manager(apb,apb_class)
#' #If object have qualitative and quantitatives variables all graph are display :
#' get_graph_car_univ(objet)
#' #If object have qualitative variable :
#' get_graph_car_univ(objet,choice_graph=c('quali'))
#' }
#'
get_graph_car_univ<- function(objet,choice_graph=c('quali','quanti')) {
  #if object without quali data :
  if (length(objet$data_quali)<=1 | !('quali' %in% choice_graph)) {
    #print result for univariate quanti
    grid.arrange(tableGrob(t(vtest_quanti(objet$data_quanti,objet$group))),
                 graph(objet$data_quanti,objet$group),top="Vtest table for quantitative variables")
    #if object without quanti data :
  } else if (length(objet$data_quanti)<=1 | !('quanti' %in% choice_graph)){
    #print result for univariate quali
    grid.arrange( graph_mod1(objet$data_quali,objet$group),
                  graph_mod2(objet$data_quali,objet$group),top="Graphic for qualitative variables")
    #if object with quali and quanti data :
  } else {
    #print result for univariate quanti and quali
    grid.arrange( graph_mod1(objet$data_quali,objet$group),
                  graph_mod2(objet$data_quali,objet$group),
                  #Var quanti :
                  tableGrob(vtest_quanti(objet$data_quanti,objet$group)),
                  graph(objet$data_quanti,objet$group),top="Graphic for qualitative and quantitatives variables")
  }
}


#multivariate

#' Graph of a PCA on classified data
#' @import ggplot2
#' @import FactoMineR
#' @import gridExtra
#' @import ggrepel
#' @import stringr
#'
#' @param objet object of 'ClusteringData' class
#' @param axes vector of two elements, containing the value of the dimensions represented in the graphs, by default dimension 1 and 2 are displayed
#' @param graph allows to choose the graph(s) you want to display
#' @param colorvector vector colors used for graphics
#'
#' @return some ggplot2 graph
#' @export
#'
#' @examples
#' \dontrun{
#'  objet<-data_manager(apb,apb_class)
#'  pca_graph(objet)
#'  pca_graph(objet,graph=c(ind","var"))
#'  }
#'
pca_graph<- function(objet,axes=c(1,2),graph=c("screeplot","ind","var"),colorvector=c("#00AFBB", "#E7B800","#DE42DE","#425CDE","#8BDE42","#DE9F42","#DE425C","#229A9A","#863100","#002486")) {
  if (length(colorvector)<length(levels(as.factor(objet$group)))) {
    stop("not enough color")
  }
  #colors for graphic
  couleur2<-c(colorvector[1:length(levels(as.factor(objet$group)))])
  #check of data
  #if data with quanti data :
  if (length(objet$data_quanti)>2) {
    #print result for univariate quanti
    ACP(objet$data_quanti,objet$group,axes=axes,couleur2=couleur2,graph=graph)
    #if object without quanti data :
  } else {
    stop("not enough variables for pca")
  }
}



#' Graph of a MCA on classified data
#' @import ggplot2
#' @import FactoMineR
#' @import gridExtra
#' @import ggrepel
#' @import stringr
#'
#' @param objet object of 'ClusteringData' class
#' @param axes vector of two elements, containing the value of the dimensions represented in the graphs, by default dimension 1 and 2 are displayed
#' @param graph allows to choose the graph(s) you want to display
#' @param colorvector vector colors used for graphics
#'
#' @return some ggplot2 graph
#' @export
#'
#' @examples
#' \dontrun{
#' objet<-data_manager(apb,apb_class)
#' mca_graph(objet)
#' mca_graph(objet,graph=c("ind","var"))
#' mca_graph(objet,graph=c("screeplot","indpvar"))}
mca_graph<- function(objet,axes=c(1,2),graph=c("screeplot","ind","var","indpvar"),colorvector=c("#00AFBB", "#E7B800","#DE42DE","#425CDE","#8BDE42","#DE9F42","#DE425C","#229A9A","#863100","#002486")) {
  if (length(colorvector)<length(levels(as.factor(objet$group)))) {
    stop("not enough color")
  }

  #colors for graphic
  couleur2<-c(colorvector[1:length(levels(as.factor(objet$group)))])
  #check of data
  #if data with quali data :
  if (length(objet$data_quali)>2) {
    #print result for univariate quanti
    ACM(objet$data_quali,objet$group,axes=axes,couleur2=colorvector,graph=graph)
    #if object without quanti data :
  } else {
    stop("not enough variables for mca")
  }


}


#' Graph of a FAMD on classified data
#'
#' @import ggplot2
#' @import FactoMineR
#' @import gridExtra
#' @import ggrepel
#' @import stringr
#'
#' @param objet object of 'ClusteringData' class
#' @param axes vector of two elements, containing the value of the dimensions represented in the graphs, by default dimension 1 and 2 are displayed
#' @param graph allows to choose the graph(s) you want to display
#' @param colorvector vector colors used for graphics
#'
#' @return some ggplot2 graph
#' @export
#'
#' @examples
#' \dontrun{
#' objet<-data_manager(apb,apb_class)
#' famd_graph(objet)
#' famd_graph(objet,axes=c(1,3))}
famd_graph<- function(objet,axes=c(1,2),graph=c("screeplot","ind","var","indpvar"),colorvector=c("#00AFBB", "#E7B800","#DE42DE","#425CDE","#8BDE42","#DE9F42","#DE425C","#229A9A","#863100","#002486")) {
  if (length(colorvector)<length(levels(as.factor(objet$group)))) {
    stop("not enough color")
  }
  #concat two type of variables :
  new_df<-cbind(objet$data_quanti,objet$data_quali)
  #colors for graphic
  couleur2<-c(colorvector[1:length(levels(as.factor(objet$group)))])
  ADM(new_df,objet$group,axes=axes,couleur2=couleur2,graph=graph)

}


###########################################
#Private function :
###########################################
##Univariate
#Quantitaive :

#Creation of the conditional table of averages and the proportion of var x explained
stat.comp <- function(x,y){
  #sample size
  n <- length(x)
  #total variability
  SCT <- sum((x-mean(x))^2)
  #conditional table
  nk <- table(y)
  #conditional mean
  mk <- tapply(x,y,mean)
  #variability explained
  SCE <- sum(nk * (mk - mean(x))^2)
  #squared correlation ratio: proportion of variance of x explained
  result <- c(mk,100.0*SCE/SCT)
  nbG<-length(unique(y))
  names(result) <- c(paste("G",1:nbG),"Var explained")

  return(result) #return table create
}

#Contribution graphs of quantitative variables to the creation of classes
graph<-function(df,groupes.cah) {

  #call function for some statistics:
  statuni<-sapply(df,stat.comp,y=groupes.cah)
  statunicontrib<-statuni[length(statuni[,1]),]

  #creation of the dataframe necessary for the realization of the barplot
  dfstat<-data.frame(statunicontrib)
  dfstat[2]<-names(statunicontrib)
  dfstat[3]<-dfstat[1]
  dfstat[3][dfstat[3]>=80]<-"Quantitative variables \nthat contributed the most"
  dfstat[3][dfstat[3]<80]<-"Quantitative variables \nthat contributed less"
  #rename the columns of the dataframe
  colnames(dfstat)<-c("contrib","nom","Caption")

  #creation of barplot :
  ggplot(data= dfstat, aes(x=nom,y=contrib , fill=Caption)) +
    geom_bar(stat="identity")+
    scale_x_discrete(limits=dfstat[order(dfstat[1],decreasing = T),]$nom) +
    geom_text(aes(label=round(contrib,2)), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5) +
    scale_fill_manual(values = c("#333366", "#9999CC")) +
    labs(title="Contribution of quantitative variables for the creation of groups",
         x="", y = "Variance proportion explained (%)")

}

#Table of Vtest for quantitatives variables
vtest_quanti<-function(df, groupe) {
  #Concatenation of data and value of classes
  dfVt<-df
  dfVt[length(df[1,])+1]<-groupe
  res<-cbind()
  #Vt by classes and by variables
  for (i in levels(as.factor(groupe)) ) {
    #by classes
    vt<-c()
    for (j in 1:(length(dfVt)-1) ) {
      #by variable
      #mean
      xbar<-mean(dfVt[,j])
      #size
      n<-length(df[,j])
      #mean by classes
      xg<-mean(dfVt[j][dfVt[length(dfVt[1,])]==i])
      #size by classes
      ng<-length(xg)
      #Vtest calculate :
      vt<-c(vt,round((xbar-xg)/sqrt(((n-ng)/(n-1))*(var(dfVt[,j])/ng)),4))
    }
    res<-cbind(res,vt)
  }
  colnames(res)<-levels(as.factor(groupe))
  rownames(res)<-colnames(df)
  return(res) #a table of Vtest for each classes by variables
}

#Qualitative :

#Function that creates the contingency table and row and column profiles, then brings out the vtest
vtest<-function(var2,groupe){
  #display these tables in the console
  var<-var2[-length(var2)]
  print("Table for variable : ")
  print(var2[length(var2)])
  print("Contingency table")
  Total<-sum
  contingence<-addmargins(table(groupe,as.factor(var) ),FUN = Total,c(1,2))

  print(contingence)

  coteprop<-addmargins(round(prop.table(addmargins(table(groupe,as.factor(var) ),
                                                   FUN = Total,1),1)*100,2) ,2,FUN = Total)
  print("Row profiles table")
  print(coteprop)

  basprop<-t(addmargins(round(prop.table(addmargins(table(as.factor(var),groupe ),
                                                    FUN = Total,1),1)*100,2) ,2,FUN = Total))
  print("Column profiles table")
  print(basprop)
  #number of class levels
  nlev<-length(levels(as.factor(groupe)))+1
  #number of variable levels
  nvar<-length(levels(as.factor(var)))+1
  #number of boxes
  n<-length(coteprop)
  #calculation of the vtest for qualitative variable
  vt<-c()
  nom<-c()
  #loop for browsing the column profile table
  for (i in 1:(nlev-1)) {
    for (j in 1:(nvar-1)) {
      p<-coteprop[nlev,j]/100
      vt<-c(vt,sqrt(contingence[i,nvar])*((coteprop[i,j]/100-p)/sqrt(((contingence[n]-contingence[i,nvar])/(contingence[n]-1))*p*(1-p))))
      nom<-c(nom,(paste(levels(as.factor(groupe))[i],"_",levels(as.factor(var))[j])))
    }
  }
  names(vt)<-nom
  return(vt) #the value of vtest for each modality of the variable
}

#creation of data for graphics :
profil_df<-function(data,groupe) {
  #variable containing all test values
  data2<-data
  data2[(dim(data)[1]+1),]<-colnames(data)
  res<-sapply(data2,vtest,groupe)
  #data frame which makes it possible to differentiate the modalities for which the vtest is significant
  res2<-data.frame(unlist(res))
  #list of Index
  res2[2]<-as.numeric(paste(1:length(res2[,1])))
  #Initialization
  res2[3]<-res2[1]
  res2[4]<-rownames(res2)
  #loop on row of the table
  for (i in 1:length(res2[,1])) {
    #if vtest is significant
    if (abs(res2[i,1])>=1.96) {
      #The name of each modalitiy
      res2[i,3]<-rownames(res2)[i]
      #recover the value of the group
      xch<-str_split(rownames(res2)[i], " ")
      res2[i,4]<-str_sub(xch[[1]][1], -1)
    } else { #if vtest isn't significant
      #value empty
      res2[i,3]<-""
      #value "not significant"
      res2[i,4]<-"Not significant"
    }

  }
  #renaming of created variables
  colnames(res2)<-c("Vtest","Index","Mod","Classe")
  return(res2)
}

#cloud of point of vtest
graph_mod1<-function(data,groupe) {
  res2<-profil_df(data,groupe)
  #Vector of colors
  couleur<-c("#00AFBB", "#E7B800","#DE42DE","#425CDE","#8BDE42","#DE9F42","#DE425C","#229A9A","#863100","#002486")
  couleur2<-c(couleur[1:length(levels(as.factor(groupe)))])
  #creation of graph :
  ggplot(res2, aes( y = Vtest, x=Index)) +
    geom_point(aes(color = Classe)) +
    geom_hline(yintercept=1.96,color = "red") +
    geom_hline(yintercept=-1.96,color = "red") +
    geom_text_repel(aes(label = Mod,  color = Classe), size = 3) +
    scale_color_manual(breaks = c(levels(as.factor(groupe)), "Not significant"),
                       values = c(couleur2, "#999999"))+
    ggtitle("Modalities significantly over or under represent depending on their group")

}

#Barplot sorted by abs(vtest)
graph_mod2<-function(data,groupe) {
  #data with significant vtest :
  res2<-profil_df(data,groupe)
  res3<-res2[res2$Mod!="",]
  colorvector=c("#00AFBB", "#E7B800","#DE42DE","#425CDE","#8BDE42","#DE9F42","#DE425C","#229A9A","#863100","#002486")
  couleur2<-c(colorvector[1:length(levels(as.factor(groupe)))])
  if ("TRUE" %in% (res2$Classe!="Not significant" )) {
    #creation of barplot :
    ggplot(data= res3, aes(x=Mod,y=Vtest , fill=Classe)) +
      geom_bar(stat="identity")+
      scale_x_discrete(limits=res3[order(abs(res3[1]),decreasing = T),]$Mod) +
      geom_text(aes(label=round(Vtest,2)), color="black",
                position = position_dodge(0.9), size=3.5) +
      scale_fill_manual(values = couleur2) +
      labs(title="Mode sorted by the absolute value of the Vtest",
           x="", y = "Value Vtest")
  } else {
    text_grob("no significant modalities, \nfor qualitative variables",)
  }

}

##Multivariate

#Screeplot :
screeploty<-function(sdev) {
  dfgraph<-data.frame(data.frame(sdev),1:length(sdev))
  colnames(dfgraph)<-c("Number_of_eigvalue","Number_of_factor")
  ggplot(dfgraph, aes(x=Number_of_factor, y=Number_of_eigvalue)) +
    ggtitle("Screeplot") +
    xlab("Number of factor") +
    ylab("Eigvalue") +
    geom_point() + geom_line()
}

#Individuals graph colored by classes
graphIndGrp<-function(analyse,groupes.cah,axes,inertie,couleur2) {
  dfgraph<-data.frame(analyse$ind$coord[,axes],as.factor(groupes.cah))
  colnames(dfgraph)<-c("Dim.1","Dim.2","group")
  ggplot(dfgraph, aes(x=Dim.1,y=Dim.2))+
    geom_point(aes(color = group)) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    scale_color_manual(values = couleur2) +
    ggtitle("Individuals factor map")+
    xlab(paste("Dim",axes[1]," (",round(inertie[axes[1]],2),"%)"))+
    ylab(paste("Dim",axes[2]," (",round(inertie[axes[2]],2),"%)"))
}

#function for PCA and graph
ACP<-function(df,group,axes,couleur2,graph=c("screeplot","ind","var")) {
  #axes : axes for graphic representation
  #start PCA :
  pca<-PCA(df)
  inertie=pca$eig[,2]
  #scree plot
  d1=screeploty(pca$svd$vs^2)
  #Individuals by size and colored by their class
  d2=graphIndGrp(pca,as.factor(group),axes,inertie,couleur2)
  #graph of variables :
  d3=plot(pca,choix="var",axes = axes)

  #What graph was choose ?
  nomgraph=c(F,F,F)
  for (i in 1:length(graph)) {
    if ('screeplot' %in% graph[i]) {
      nomgraph[1]=T }
    if ('ind' %in% graph[i]) {
      nomgraph[2]=T  }
    if ('var' %in% graph[i]) {
      nomgraph[3]=T }
  }
  #displays of all graph :
  plot_list<- list(d1, d2, d3)[nomgraph]
  do.call("grid.arrange", c(plot_list, top="PCA Graph colored by classes"))
}

#projections of modalities according to dimensions:
graphVarGrp<-function(analyse,groupes.cah,axes,inertie) {
  dfgraph<-data.frame(analyse$var$coord[,axes])
  colnames(dfgraph)<-c("Dim.1","Dim.2")
  ggplot(dfgraph, aes(x=Dim.1,y=Dim.2))+
    geom_point() +
    geom_text_repel(aes(label = rownames(dfgraph)), size = 3) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    ggtitle("Variables factor map")+
    xlab(paste("Dim",axes[1]," (",round(inertie[axes[1]],2),"%)"))+
    ylab(paste("Dim",axes[2]," (",round(inertie[axes[2]],2),"%)"))
}

#Projection of individuals and modalities : barycentric representation
graphdoubleGrp<-function(analyse,group,axes,inertie,couleur2) {
  #merge data with a variable that differentiates between ind groups and modalities
  mod<-data.frame(analyse$var$coord[,axes],"Modality")
  ind<-data.frame(analyse$ind$coord[,axes],as.factor(group))
  colnames(mod)<- c("Dim.1","Dim.2","Class")
  colnames(ind)<-colnames(mod)
  concat<-rbind(mod,ind)

  #creation of cloud of point
  ggplot(concat, aes(x=Dim.1,y=Dim.2))+
    geom_point(aes(color = Class)) +
    geom_text_repel(aes(label = rownames(concat),color = Class), size = 3) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    scale_color_manual(breaks = c(levels(as.factor(group)), "Modality"),
                       values = c(couleur2, "black")) +
    ggtitle("Individuals color by class and variables factor map")+
    xlab(paste("Dim",axes[1]," (",round(inertie[axes[1]],2),"%)"))+
    ylab(paste("Dim",axes[2]," (",round(inertie[axes[2]],2),"%)"))
}

#function for MCA et graph
ACM<-function(df,group,axes,graph=c("screeplot","ind","var","indpvar"),couleur2) {
  #axes : axes of graphical analysis
  #Start MCA
  mca <- MCA(df,ncp=2)
  inertie=mca$eig[,2]
  #scree plot
  d1=screeploty(mca$eig[,1])
  #Individuals by size and colored by their class
  d2=graphIndGrp(mca,group,axes,inertie,couleur2)
  #projections of modalities according to dimensions:
  d3=graphVarGrp(mca,group,axes,inertie)
  #Projection of individuals and modalities:
  # barycentric representation
  d4=graphdoubleGrp(mca,group,axes,inertie,couleur2)

  #What graph was choose ?
  nomgraph=c(F,F,F,F)
  if ('screeplot' %in% graph) {
    nomgraph[1]=T }
  if ('ind' %in% graph) {
    nomgraph[2]=T  }
  if ('var' %in% graph) {
    nomgraph[3]=T }
  if ('indpvar' %in% graph) {
    nomgraph[4]=T  }

  #displays of all graph :
  plot_list<- list(d1, d2, d3, d4)[nomgraph]
  do.call("grid.arrange", c(plot_list, top="MCA Graph colored by classes"))
}

#function for FAMD et graph
ADM<-function(df,group,graph=c("screeplot","ind","var","indpvar"),couleur2,axes) {

  #Start FAMD
  AFDM<-FAMD(df)
  inertie=AFDM$eig[,2]

  #scree plot
  d1=screeploty(AFDM$svd$vs^2)
  #Individuals by size and colored by their class
  d2=graphIndGrp(AFDM,group,axes,inertie,couleur2)
  #projections of modalities according to dimensions:
  d3=graphVarGrp(AFDM,group,axes,inertie)
  #Projection of individuals and modalities:
  # barycentric representation
  d4=graphdoubleGrp(AFDM,group,axes,inertie,couleur2)

  #What graph was choose ?:
  nomgraph=c(F,F,F,F)
  if ('screeplot' %in% graph) {
    nomgraph[1]=T }
  if ('ind' %in% graph) {
    nomgraph[2]=T  }
  if ('var' %in% graph) {
    nomgraph[3]=T }
  if ('indpvar' %in% graph) {
    nomgraph[4]=T  }

  #displays of all graph :
  plot_list<- list(d1, d2, d3, d4)[nomgraph]
  do.call("grid.arrange", c(plot_list, top="FAMD Graph colored by classes"))

}

