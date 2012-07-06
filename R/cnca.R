cnca <-
function(fespecies, fvambientales)
{

#############################################################################
#########libraries
#############################################################################

require(tcltk)
library(tcltk2)
  library(tkrplot)
library(rgl)
tclRequire("BWidget")


#############################################################################
### Informative window
#############################################################################
symbolos <- c("*",".", "o","O","0","+","-","|","%","#")
nejes<<-3  
dim1<<-1
dim2<<-2
dim3<<-3   



winfor<-tktoplevel()
tkwm.title(winfor,"CNCA")
OnOKinf <- function()
{
#############################################################################
### Names of label window
#############################################################################

tkdestroy(winfor)
wnames<-tktoplevel()
tkwm.title(wnames,gettext("Enter names",domain="R-cncaGUI"))      
OnOKnames<-function()
{


Namesit <<- tclvalue(sitesname)
Namespe <<- tclvalue(speciesname)
Namevar <<- tclvalue(variablesname)


tkdestroy(wnames)


##We save the dimensions
dimespec<<-dim(fespecies)
dimvambien<<-dim(fvambientales)

##We create the data matrices
especies<<-array(data=unlist(fespecies), dim=c(dimespec[1],(dimespec[2])))
vambientales<<-array(data=unlist(fvambientales),dim=c(dimvambien[1],(dimvambien[2])))

##We sum all elements of species
sumatotal<<-sum(especies)

Fe<<-especies/sumatotal

fn<<-array(dim=c(dimespec[1],1))

##We calculate the marginal totals and we save it in fn and fq
Idq<<-as.matrix(rep(1,times=dimespec[2]))
fn<<-Fe%*%Idq

Idn<<-as.matrix(rep(1,times=dimespec[1]))
fq<<-t(Fe)%*%Idn


#Matrix whose diagonal is the marginals fn 
Dn<<-diag(as.vector(t(fn)))




###########################################################################################
##We create the vectors of the labels
###########################################################################################

textlugares<<-rownames(fespecies)
textespecies<<-colnames(fespecies)
textvariables<<-colnames(fvambientales)


###########################################################################################
##We create the vectors of the colors
###########################################################################################

collugares<<-rep("green",dimespec[1])
colespecies<<-rep("blue",dimespec[2])
colvariables<<-rep("red",dimvambien[2])



###########################################################################################
##We create the vectors of the symbols
###########################################################################################

simlugares<<-rep("*",dimespec[1])
simespecies<<-rep("+",dimespec[2])
simvariables<<-rep(" ",dimvambien[2])


###########################################################################################
##We create the vectors of the character size
###########################################################################################

cexlugares<<-rep(1,dimespec[1])
cexespecies<<-rep(1,dimespec[2])
cexvariables<<-rep(1,dimvambien[2])





#####Window to change labels and colors#######


tt<-tktoplevel()
tkwm.title(tt,gettext("Options",domain="R-cncaGUI"))



#####Dropdown menu #############################


#### List of transformations
framett<-tkframe(tt, relief = "flat", borderwidth = 2,
  background = "white")

  framett1<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")

  framett2<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")

  framett3<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")

  framett4<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")



  framet1<-tkframe(framett1, relief = "ridge", borderwidth = 2, 
        background = "white")

frametext1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")


frameok1<-tkframe(framett1, relief = "ridge", borderwidth = 2, 
        background = "white")

framecol1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")

framecol11<-tkframe(framecol1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecol12<-tkframe(framecol1, relief = "flat", borderwidth = 2, 
        background = "white")

  framename1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")

framename11<-tkframe(framename1, relief = "flat", borderwidth = 2, 
        background = "white")

  framename12<-tkframe(framename1, relief = "flat", borderwidth = 2, 
        background = "white")

  framecex1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")
                                
  framecex11<-tkframe(framecex1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecex12<-tkframe(framecex1, relief = "flat", borderwidth = 2, 
        background = "white")
  
  frames1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")      
  
  frames11<-tkframe(frames1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames12<-tkframe(frames1, relief = "flat", borderwidth = 2, 
        background = "white")
  

  framet2<-tkframe(framett2, relief = "ridge", borderwidth = 2, 
        background = "white")

frametext2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

frameok2<-tkframe(framett2, relief = "ridge", borderwidth = 2, 
        background = "white")

framecol2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

framecol21<-tkframe(framecol2, relief = "flat", borderwidth = 2, 
        background = "white")

  framecol22<-tkframe(framecol2, relief = "flat", borderwidth = 2, 
        background = "white")

  framename2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

framename21<-tkframe(framename2, relief = "flat", borderwidth = 2, 
        background = "white")

  framename22<-tkframe(framename2, relief = "flat", borderwidth = 2, 
        background = "white")

  framecex2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")
                                
  framecex21<-tkframe(framecex2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecex22<-tkframe(framecex2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames21<-tkframe(frames2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framet3<-tkframe(framett3, relief = "ridge", borderwidth = 2, 
        background = "white")

frametext3<-tkframe(framett3, relief = "flat", borderwidth = 2, 
        background = "white")

frameok3<-tkframe(framett3, relief = "ridge", borderwidth = 2, 
        background = "white")

framecol3<-tkframe(framett3, relief = "flat", borderwidth = 2, 
        background = "white")

framecol31<-tkframe(framecol3, relief = "flat", borderwidth = 2, 
        background = "white")

  framecol32<-tkframe(framecol3, relief = "flat", borderwidth = 2, 
        background = "white")

  framename3<-tkframe(framett3, relief = "flat", borderwidth = 2, 
        background = "white")

framename31<-tkframe(framename3, relief = "flat", borderwidth = 2, 
        background = "white")

  framename32<-tkframe(framename3, relief = "flat", borderwidth = 2, 
        background = "white")

  framecex3<-tkframe(framett3, relief = "flat", borderwidth = 2, 
        background = "white")
                                
  framecex31<-tkframe(framecex3, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecex32<-tkframe(framecex3, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames3<-tkframe(framett3, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames31<-tkframe(frames3, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames32<-tkframe(frames3, relief = "flat", borderwidth = 2, 
        background = "white")
  
  
  framet4<-tkframe(framett4, relief = "ridge", borderwidth = 2, 
        background = "white")

frametext4<-tkframe(framett4, relief = "flat", borderwidth = 2, 
        background = "white")

frameok4<-tkframe(framett4, relief = "ridge", borderwidth = 2, 
        background = "white")

framecol4<-tkframe(framett4, relief = "flat", borderwidth = 2, 
        background = "white")

  framecol41<-tkframe(framecol4, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecol42<-tkframe(framecol4, relief = "flat", borderwidth = 2, 
        background = "white")

framename4<-tkframe(framett4, relief = "flat", borderwidth = 2, 
        background = "white")

framename41<-tkframe(framename4, relief = "flat", borderwidth = 2, 
        background = "white")

  framename42<-tkframe(framename4, relief = "flat", borderwidth = 2, 
        background = "white")

  framecex4<-tkframe(framett4, relief = "flat", borderwidth = 2, 
        background = "white")
                                
  framecex41<-tkframe(framecex4, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framecex42<-tkframe(framecex4, relief = "flat", borderwidth = 2, 
        background = "white")
        
  frames4<-tkframe(framett4, relief = "flat", borderwidth = 2, 
        background = "white")
  
  frames41<-tkframe(frames4, relief = "flat", borderwidth = 2, 
        background = "white")
  
  

framegraphic<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")


scrtrans <- tkscrollbar(framet4, repeatinterval=5,
   command=function(...)tkyview(tltrans,...))
tltrans<-tklistbox(framet4,height=6,width=42,selectmode="single",yscrollcommand=function(...)tkset(scrtrans,...),background="white")
tipostd <- c(gettext("Subtract the global mean",domain="R-cncaGUI"),gettext("Column centering",domain="R-cncaGUI"),gettext("Standardize columns",domain="R-cncaGUI"),gettext("Row centering",domain="R-cncaGUI"),gettext("Standardize rows",domain="R-cncaGUI"),gettext("Raw data",domain="R-cncaGUI"))
for (i in (1:6))
{
    tkinsert(tltrans,"end",tipostd[i])
}
tkselection.set(tltrans,5)  # Default Raw Data.
OnOKtrans <- function()
{                                 
tChoice <<- tipostd[as.numeric(tkcurselection(tltrans))+1]

###We standardize the matrix 
if (tChoice==gettext("Subtract the global mean",domain="R-cncaGUI")){
media<<-mean(vambientales)
vambientalesst<<-vambientales




for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalesst[i,j]<<-(vambientales[i,j]-media)
}
}
}


if (tChoice==gettext("Column centering",domain="R-cncaGUI")){
media<<-colSums(Dn%*%vambientales)
 vambientalesst<<-vambientales


for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalesst[i,j]<<-(vambientales[i,j]-media[j])
}
}


}
if (tChoice==gettext("Standardize columns",domain="R-cncaGUI")){

 media<<-colSums(Dn%*%vambientales)
 vambientalescentr<<-vambientales


for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalescentr[i,j]<<-(vambientales[i,j]-media[j])
}
}



vambientalescentrcuad<<-vambientalescentr^2
varvaria<<-colSums(Dn%*%vambientalescentrcuad)

vambientalesst<<-vambientales

for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalesst[i,j]<<-(vambientales[i,j]-media[j])/(varvaria[j])^(1/2)
}
}


}


if (tChoice==gettext("Row centering",domain="R-cncaGUI")){

mediav<<-rowMeans(vambientales)
vambientalesst<<-vambientales


for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalesst[i,j]<<-(vambientales[i,j]-mediav[i])
}
}
}


if (tChoice==gettext("Standardize rows",domain="R-cncaGUI")){
mediav<<-rowMeans(vambientales)
desvvar<<-mediav
for (i in 1:dim(vambientales)[1])
{
desvvar[i]<<-sqrt(var(vambientales[i,]))
}


vambientalesst<<-vambientales


for(j in 1:dim(vambientales)[2])
{for(i in 1:dim(vambientales)[1])
{
vambientalesst[i,j]<<-(vambientales[i,j]-mediav[i])/desvvar[i]
}
}
}

if (tChoice==gettext("Raw data",domain="R-cncaGUI")){

vambientalesst<<-vambientales

}
}

OK.buttrans <-tkbutton(frameok4,text="    OK    ",command=OnOKtrans)


tkpack(OK.buttrans,expand = "TRUE", side="right", fill = "both")







##### Checkbox to show the axes or not  #######

cb <- tkcheckbutton(framecol42)
cbValue <- tclVar("0")
tkconfigure(cb,variable=cbValue)

##### Checkbox to show the sites or not  #######

cbl <- tkcheckbutton(framename42)
cblug <- tclVar("0")
tkconfigure(cbl,variable=cblug)



##### Checkbox to show the labels of the sites or not  #######

cbll <- tkcheckbutton(framecex42)
cblablug <- tclVar("0")
tkconfigure(cbll,variable=cblablug)


##### List of species###########################



indicee<-NULL
Namee<-NULL
Cexe<-1
NameVale<-NULL
NameCexe<-NULL


scre <- tkscrollbar(framet1, repeatinterval=5,
   command=function(...)tkyview(tle,...))

tle<-tklistbox(framet1,height=6,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scre,...),background="white")

tkpack(tklabel(frametext1,text=Namespe),side="left",expand = "TRUE",fill="both")

for (i in 1:(dimespec[2]))
{
    tkinsert(tle,"end",textespecies[i])
}
tkselection.set(tle,0) #  Indexing starts at zero.

OnOKe <- function()
{
Choicee <<- textespecies[as.numeric(tkcurselection(tle))+1]

##### Color of the selected variable #############
indicee<<-as.numeric(tkcurselection(tle))+1
colore <- colespecies[indicee[1]]
 tkconfigure(canvase,bg=colore)


##### Text of the selected variable###############

Namee <<- tclVar(textespecies[indicee[1]])
tkconfigure(entry.Namee,textvariable=Namee)
   
##### Size of the selected variable###############

Cexe <<- tclVar(cexespecies[indicee[1]])
tkconfigure(entry.Cexe,textvariable=Cexe) 
}

OK.bute <-tkbutton(frameok1,text="    OK    ",command=OnOKe)
tkpack(tle,scre,expand = "TRUE", side="left", fill = "both")
tkpack.configure(scre,side="left")

tkpack(OK.bute,expand = "TRUE", side="left", fill = "both")
tkfocus(tt)


    


#######Color#######################################
indicee<-as.numeric(tkcurselection(tle))+1
colore <<- colespecies[indicee[1]]
canvase <- tkcanvas(framecol11,width="57",height="20",bg=colore)

ChangeColore <- function()
{
 
colore <<- tclvalue(tcl("tk_chooseColor",initialcolor=colespecies[indicee[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
 if (nchar(colore)>0)
    {
tkconfigure(canvase,bg=colore)
 colespecies[indicee]<<-colore
}
}

ChangeColor.buttone<- tkbutton(framecol12,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColore,width=4)
tkpack(canvase,expand = "TRUE", side="left", fill = "both")
  tkpack(ChangeColor.buttone,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
Namee <- textespecies[indicee[1]]
entry.Namee <-tkentry(framename11,width="10",textvariable=Namee)

OnOKle <- function()
{
NameVale <<- tclvalue(Namee)
textespecies[indicee[1]]<<-NameVale

#####Values of listbox###############################
for (i in 1:(dimespec[2]))
{
tkdelete(tle,0)
}

for (i in (1:(dimespec[2])))
{
   tkinsert(tle,"end",textespecies[i])
}


}

OK.butle <-tkbutton(framename12,text=gettext("Change label",domain="R-cncaGUI"),command=OnOKle,width=4)
tkbind(entry.Namee, "<Return>",OnOKle)
tkpack(entry.Namee,expand = "TRUE", side="left", fill = "both")
tkpack(OK.butle,expand = "TRUE", side="left", fill = "both")


###### Sizes  ###################################
Cexe <- cexespecies[indicee[1]]
entry.Cexe <-tkentry(framecex11,width="10",textvariable=Cexe)

OnOKce <- function()
{
NameCexe <<- tclvalue(Cexe)
cexespecies[indicee[1]]<<-NameCexe

}

OK.butce <-tkbutton(framecex12,text=gettext("Change size",domain="R-cncaGUI"),command=OnOKce,width=4)
tkbind(entry.Cexe, "<Return>",OnOKce)
tkpack(entry.Cexe,expand = "TRUE", side="left", fill = "both")
tkpack(OK.butce,expand = "TRUE", side="left", fill = "both")


######Symbols###################################

comboBoxe <- tkwidget(frames11,"ComboBox",editable=FALSE,values=symbolos,width=7)

chang.syme <- function()
{
    simChoicee <<- symbolos[as.numeric(tclvalue(tcl(comboBoxe,"getvalue")))+1]
simespecies[indicee]<<-simChoicee
 }
Change.symbole <-tkbutton(frames12,text=gettext("Change symbol",domain="R-cncaGUI"),command=chang.syme,width=4)
tkpack(comboBoxe,side="left",expand="TRUE", fill="both")
tkpack(Change.symbole,side="left",expand="TRUE", fill="both")








##### List of environmental v. ###########################


indicev<-NULL
Namev<-NULL
NameValv<-NULL
Cexv<-1
NameCexv<-NULL


scrv <- tkscrollbar(framet2, repeatinterval=5,
   command=function(...)tkyview(tlv,...))

tlv<-tklistbox(framet2,height=6,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scrv,...),background="white")

tkpack(tklabel(frametext2,text=Namevar),side="left",expand = "TRUE",fill="both")

for (i in (1:(dimvambien[2])))
{
    tkinsert(tlv,"end",textvariables[i])
}
tkselection.set(tlv,0) #  Indexing starts at zero.

OnOKv <- function()
{
Choicev <<- textvariables[as.numeric(tkcurselection(tlv))+1]

##### Color of the selected variable  #############
indicev<<-as.numeric(tkcurselection(tlv))+1
colorv <- colvariables[indicev[1]]
 tkconfigure(canvasv,bg=colorv)


##### Text of the selected variable  ###############

Namev <<- tclVar(textvariables[indicev[1]])
tkconfigure(entry.Namev,textvariable=Namev)

##### Size of the selected variable  ###############

Cexv <<- tclVar(cexvariables[indicev[1]])
tkconfigure(entry.Cexv,textvariable=Cexv)
    
}

OK.butv <-tkbutton(frameok2,text="    OK    ",command=OnOKv)

tkpack(OK.butv,expand = "TRUE", side="left", fill = "both")

Graphics <- function()
{

invD<<-solve(Dn)


Dq<<-diag(as.vector(t(fq)))
invDq<<-solve(Dq)


P<<-invD%*%Fe
Pb<<-P-(Idn%*%t(fq))

covar<<-t(vambientalesst)%*%Dn%*%vambientalesst
descovar<<-La.svd(covar)


Pi<<-vambientalesst%*%(descovar$u%*%(solve(diag(descovar$d)))%*%descovar$v)%*%t(vambientalesst)%*%Dn
Pbest<<-Pi%*%Pb

barvp<-tktoplevel()
      tkwm.title(barvp,gettext("Eigenvalues",domain="R-cncaGUI"))
    
    plotbar<-function()
    {
##########################################################################################
###Matrix Lp
##########################################################################################

    Fb<<-Fe-(fn%*%t(fq))
    L<<-t(Fb)%*%vambientalesst
    Lp<<-((descovar$u%*%(solve(diag(descovar$d)^(1/2)))%*%descovar$v))%*%t(L)
       descom<<-svd(Lp,nu=nejes,nv=nejes)
        suma2valprop<<-sum((descom$d[1:length(descom$d)])^2)
sumaRvalprop<<-sum((descom$d)^2)
inerciatot<<-(descom$d[1:length(descom$d)])^2/sumaRvalprop

        barplot(descom$d, col="blue", xlab="", ylab="", names.arg=round(inerciatot, digits=2))
      }
    
     imgbar <- tkrplot(barvp,fun=plotbar,hscale=1.5,vscale=1.5)
     
     msginertia<-"Proportion of inertia explained by each axis:"
     for (i in 1:length(descom$d))
      {
      msginertia<-paste(msginertia, "\n",i, "\t", round(inerciatot[i]*100, digits=2), "%")
      }
      tk2tip(imgbar, msginertia)


  Onaxis <- function()
   {
nejes <<- tclvalue(numaxis)
nejes<<-as.numeric(nejes)
    if (nejes > length(descom$d))
 {
        msg <- paste(gettext("The maximum number of dimensions is ",domain="R-cncaGUI"),length(descom$d))
    tkmessageBox(message=msg)
       
 }else{

tkdestroy(barvp)
nejes <<- as.integer(nejes)
   


##########################################################################################
###Singular Value Decomposition of Pbest
##########################################################################################

descpbest<<-La.svd(Pbest,nu=nejes,nv=nejes)

R<<-descpbest$u
    #V<<-diag(descpbest$d[1:nejes]) 
  #Tn<<-t(descpbest$v)
  


##########################################################################################
###Singular Value Decomposition of Lp
##########################################################################################

desclp<<-svd(Lp,nu=nejes,nv=nejes)


V<<-diag(desclp$d[1:nejes]) 

Tn<<-desclp$v
  
    A<<-desclp$u

    #RV<<-Pbest %*% Tn

###########################################################################################
##Principal coordinates of sites
###########################################################################################

colugares<<-R%*%V 

###########################################################################################
##Standard coordinates of species
###########################################################################################

coespecies<<-Tn


###########################################################################################
##Principal coordinates of environmental variables
###########################################################################################

covambien<<-((descovar$u%*%(diag(descovar$d)^(1/2))%*%descovar$v))%*%A%*%V


##########################################################################
#### Contributions and qualities of representation
##########################################################################
ejes<<-c()
    for (i in 1:nejes)
    {
      ejes<<-c(ejes, paste(gettext("Axis",domain="R-cncaGUI"),i))
    }
    
####Contributions of the sites
colugarescuad<<-(R%*%V)^2
fixi<<-colugarescuad
Calphai<<-fixi
coespeciescuad<<-(Tn%*%V)^2
Calphak<<-coespeciescuad

for (i in 1:nejes)
    {
   fixi[,i]<<-fn[i]*colugarescuad[,i]
Calphai[,i]<<-(fixi[,i]*1000)/(diag(V)[i])^2

####Contributions of the species
Calphak[,i]<<-(coespeciescuad[,i]*1000)/(diag(V)[i])^2

}
rownames(Calphai)<<-textlugares
colnames(Calphai)<<-ejes

rownames(Calphak)<<-textespecies
colnames(Calphak)<<-ejes


####qualities of representation respect to the projected space

variabilidadps<<-sum(diag(V)[1:nejes])

calidadpst<<-diag(V)[1:nejes]/variabilidadps
calidadps<<-t(calidadpst)*1000
colnames(calidadps)<<-ejes


####qualities of representation respect to the original space

variabilidados<<-sum(descom$d)

calidadost<<-diag(V)[1:nejes]/variabilidados
calidados<<-t(calidadost)*1000
colnames(calidados)<<-ejes





#### Distances

invfn<<-fn^(-1)

fifk<<-fn%*%t(fq)

resta<<-Fe-fifk

restacuad<<-resta^2

especiesos<<-restacuad

for (i in 1:dim(Fe)[1])
{
for(j in 1:dim(Fe)[2])
{
especiesos[i,j]<<-invfn[i]*restacuad[i,j]
}
}

dok<<-colSums(especiesos)




lugaresos<<-restacuad

for (i in 1:dim(Fe)[1])
{
for(j in 1:dim(Fe)[2])
{
lugaresos[i,j]<<-((invfn[i])^2)*restacuad[i,j]
}
}

doi<<-rowSums(lugaresos)


pbestcuad<-Pbest^2


especiespr<<-pbestcuad

for (i in 1:dim(Pbest)[1])
{
for(j in 1:dim(Pbest)[2])
{
especiespr[i,j]<<-fn[i]*pbestcuad[i,j]
}
}

dprk<<-colSums(especiespr)



lugarespr<<-pbestcuad

dpri<<-rowSums(lugarespr)


##### Relative to sites


qalphaips<<-colugarescuad
dalphaios<<-colugarescuad

##### Relative to species
qalphakps<<-coespeciescuad
qalphakos<<-coespeciescuad



for (i in 1: nejes)
{
qalphaips[,i]<<-(colugarescuad[,i]*1000)/dpri
dalphaios[,i]<<-(colugarescuad[,i]*1000)/doi
qalphakps[,i]<<-(coespeciescuad[,i]*1000)/dprk
qalphakos[,i]<<-(coespeciescuad[,i]*1000)/dok
}

rownames(qalphaips)<<-textlugares
colnames(qalphaips)<<-ejes

rownames(dalphaios)<<-textlugares
colnames(dalphaios)<<-ejes

rownames(qalphakps)<<-textespecies
colnames(qalphakps)<<-ejes

rownames(qalphakos)<<-textespecies
colnames(qalphakos)<<-ejes

 ##### retained inertia
    
    eigencuad<<-(diag(V))^2
    inertia<<-cbind(eigencuad,eigencuad)
    
    inertia[,1]<<-(eigencuad/sum(eigencuad))*100
    inertia[,2]<<-cumsum(inertia[,1])
    colnames(inertia)<<-c("Retained inertia (%)","Cumulative retained inertia(%)")

coindividuosnam<<-as.data.frame(coespecies)
rownames(coindividuosnam)<<-textespecies
colnames(coindividuosnam)<<-ejes

covariablesnam<<-as.data.frame(covambien)
rownames(covariablesnam)<<-textvariables
colnames(covariablesnam)<<-ejes

colugaresnam<<-as.data.frame(colugares)
rownames(colugaresnam)<<-textlugares
colnames(colugaresnam)<<-ejes


cat(gettext("File saved in:    ",domain="R-cncaGUI"),file=gettext("Results.txt",domain="R-cncaGUI"))
cat(getwd(),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("CONTRIBUTIONS:\n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(gettext("Contributions to",domain="R-cncaGUI"), Namesit,":\n"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(Calphai, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")

cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(gettext("Contributions to",domain="R-cncaGUI"), Namespe,":\n"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(Calphak, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("QUALITIES OF REPRESENTATION\n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")

cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("Proportion of inertia explained for each axis (projected space):\n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(calidadps, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("Proportion of inertia explained for each axis (original space):\n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(calidados, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("Qualities of representation relative to elements:\n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namesit,gettext("respect to projected space:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(qalphaips, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namesit,gettext("respect to original space:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(dalphaios, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")



cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namespe,gettext("respect to projected space:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(qalphakps, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namespe,gettext("respect to original space:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(qalphakos, digits=5),file="temp.txt", sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")



cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namespe,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(coindividuosnam, digits=5),file="temp.txt",sep="\t",dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")




cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namevar,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(covariablesnam, digits=5), file="temp.txt", sep="\t", dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(paste(Namesit,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(colugaresnam, digits=5), file="temp.txt", sep="\t", dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


cat("\n",file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
cat(gettext("Eigen values: \n",domain="R-cncaGUI"),file="temp.txt")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")
write.table(round(descom$d, digits=5), file="temp.txt", sep="\t", dec=",")
file.append(gettext("Results.txt",domain="R-cncaGUI"),"temp.txt")


file.show(gettext("Results.txt",domain="R-cncaGUI"))
file.remove("temp.txt")

###########################################################################################
##Rescale
###########################################################################################


sumalugares<<-sum(colugares^2)
sumaespecies<<-sum(coespecies^2)
sumavariables<<-sum(covambien^2)



slg<<-sumalugares/(dim(colugares)[1])
sesp<<-sumaespecies/(dim(coespecies)[1])
svar<<-sumavariables/(dim(covambien)[1])

scfev<<-((sesp/svar)^(1/2))^(1/2)
scflv<<-((slg/sesp)^(1/2))^(1/2)


coespecies<<-coespecies/scfev
covambien<<-covambien*scfev
colugares<<-(colugares/scflv)*scfev



datos<<-rbind(coespecies,covambien,colugares)
textos<<-datos
datosr<<-rbind(coespecies,covambien)
textosr<<-datosr
centro<<-c(0,0)





################ Show axes or not
cbVal <<- as.character(tclvalue(cbValue))

cblugares <<- as.character(tclvalue(cblug))

cblablugares<<-as.character(tclvalue(cblablug))

simbolos<<-c(simespecies, simvariables, simlugares)

################ Show sites or not
if (cblugares=="1"){

xCoords<<-textos[,dim1]
yCoords<<-textos[,dim2]
zCoords<<-textos[,dim3]

if (cblablugares=="1"){
labelsVec <<- c(textespecies, textvariables,textlugares)
sizesVec <<- c(cexespecies, cexvariables,cexlugares)
}else{
labelsVec <<- c(textespecies, textvariables)
      sizesVec <<- c(cexespecies, cexvariables)
}


colores<<-c(colespecies,colvariables,collugares)


}else{
xCoords<<-textosr[,dim1]
yCoords<<-textosr[,dim2]
zCoords<<-textosr[,dim3]
labelsVec <<- c(textespecies, textvariables)
sizesVec <<- c(cexespecies, cexvariables)
colores<<-c(colespecies,colvariables)

}

indexLabeled<-c(1:length(xCoords))
indexLabeledaux<-c()
labeledPoints <- list()


wgr <- tktoplevel()
tkwm.title(wgr,gettext("Graph",domain="R-cncaGUI"))
parPlotSize <- c()
usrCoords <- c()

plotFunction <- function(screen=TRUE)
{

  params <- par(bg="white")
 if (cblugares=="1"){
xCoords<<-textos[,dim1]
yCoords<<-textos[,dim2]
 plot(datos[,c(dim1,dim2)],main= gettext("Graph",domain="R-cncaGUI"),type="n",xlab=paste(round(inerciatot[dim1]*100, digits=2),"%"),ylab=paste(round(inerciatot[dim2]*100,digits=2),"%"))


}else{
 xCoords<<-textosr[,dim1]
yCoords<<-textosr[,dim2]
plot(datosr[,c(dim1,dim2)],main= gettext("Graph",domain="R-cncaGUI"),type="n",xlab=paste(round(inerciatot[dim1]*100, digits=2),"%"),ylab=paste(round(inerciatot[dim2]*100,digits=2),"%"))


}

 
points(coespecies[,dim1],coespecies[,dim2],pch=simespecies, col=colespecies)

arrows(centro[1],centro[2],covambien[,dim1],covambien[,dim2],col=colvariables,lty="dotted", length=0.05)



points(centro[1],centro[2],pch=18,col="black")

if (cbVal=="1"){

abline(h=centro[2],v=centro[1],lty="dotted")
}


if (cblugares=="1"){
points(colugares[,dim1],colugares[,dim2],pch=simlugares, col=collugares)

if (cblablugares=="1"){
}

}

  if (length(indexLabeled)>0)
    for (i in (1:length(indexLabeled)))
    {
      indexClosest <- indexLabeled[i]
      text(xCoords[indexClosest],yCoords[indexClosest],
           labels=labelsVec[indexClosest], col= colores[indexClosest], cex= as.numeric(sizesVec)[indexClosest])
    }
  parPlotSize <<- par("plt")
  usrCoords   <<- par("usr")
  par(params)
}

img <- tkrplot(wgr,fun=plotFunction,hscale=1.5,vscale=1.5)
framedim1<-tkframe(wgr, relief = "flat", borderwidth = 2, 
        background = "whitesmoke")

 
  
  
  comboBoxdim1 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim1)
  comboBoxdim2 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim2)
  comboBoxdim3 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim3)


chang.symdim1 <- function()
{
    dim1 <<-as.numeric(tclvalue(tcl(comboBoxdim1,"getvalue")))+1
dim2 <<-as.numeric(tclvalue(tcl(comboBoxdim2,"getvalue")))+1
dim3 <<-as.numeric(tclvalue(tcl(comboBoxdim3,"getvalue")))+1
      tkrreplot(img)


 }
Change.symboldim1 <-tkbutton(framedim1,text=gettext("Choose",domain="R-cncaGUI"),command=chang.symdim1, bg= "lightblue", width=10, foreground = "navyblue")

  
 tkpack(tklabel(framedim1, text=gettext("Select X, Y and Z axes numbers:",domain="R-cncaGUI")),
    expand="FALSE", side= "left", fill ="both")
  tkpack(comboBoxdim1, comboBoxdim2, comboBoxdim3, Change.symboldim1, side="left", expand="FALSE")
  tkpack(img, side="top", expand="TRUE", fill="both")
  tkpack(framedim1, side="top", expand="FALSE", fill="both")
             



  
labelClosestPointd <- function(xClick,yClick,imgXcoords,imgYcoords)
{
  squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  indexClosest <<- which.min(squared.Distance)
mm<-tktoplevel() 
tkwm.title(mm, labelsVec[indexClosest])


framemm1<-tkframe(mm, relief = "groove", borderwidth = 2, 
        background = "white")

framemm2<-tkframe(mm, relief = "groove", borderwidth = 2, 
        background = "white")

framemm3<-tkframe(mm, relief = "groove", borderwidth = 2, 
        background = "white")
        
    framemm4<-tkframe(mm, relief = "groove", borderwidth = 2, 
        background = "white")



colori <- colores[indexClosest]
canvasi <- tkcanvas(framemm1,width="70",height="20",bg=colori)

ChangeColori <- function()
{
 
colori <<- tclvalue(tcl("tk_chooseColor",initialcolor=colores[indexClosest],title=gettext("Choose a color",domain="R-cncaGUI")))
 
 if (nchar(colori)>0)
    {
tkconfigure(canvasi,bg=colori)
 colores[indexClosest]<<-colori
if (cblugares=="1"){
colespecies<<-colores[1:length(colespecies)]
colvariables<<-colores[(length(colespecies)+1):(length(colespecies)+length(colvariables))]
collugares<<-colores[(length(colespecies)+length(colvariables)+1):(length(colespecies)+length(colvariables)+length(collugares))]
}else{
colespecies<<-colores[1:length(colespecies)]
colvariables<<-colores[(length(colespecies)+1):(length(colespecies)+length(colvariables))]
}

}
tkrreplot(img)
tkdestroy(mm)
}

ChangeColor.buttoni<- tkbutton(framemm1,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColori,width=12)
tkpack(canvasi,ChangeColor.buttoni,expand = "TRUE", side="left", fill = "both")

Namei <<- labelsVec[indexClosest]

tclvalue(Namei) <<- labelsVec[indexClosest]
entry.Namei <<-tkentry(framemm2,width="11",textvariable=Namei)
NameVali <<- Namei 

OnOKli <- function()
{
NameVali <<- tclvalue(Namei)
labelsVec[indexClosest]<<-NameVali

if (cblugares=="1"){
textespecies<<-labelsVec[1:length(textespecies)]
textvariables<<-labelsVec[(length(textespecies)+1):(length(textespecies)+length(textvariables))]
textlugares<<-labelsVec[(length(textespecies)+length(textvariables)+1):(length(textespecies)+length(textvariables)+length(collugares))]
}else{
textespecies<<-labelsVec[1:length(textespecies)]
textvariables<<-labelsVec[(length(textespecies)+1):(length(textespecies)+length(textvariables))]
}


#####Values of listbox###############################

for (i in 1:(dimespec[2]))
{
tkdelete(tle,0)
}

for (i in (1:(dimespec[2])))
{
   tkinsert(tle,"end",textespecies[i])
}




for (i in 1:dimvambien[2])
{
tkdelete(tlv,0)
}

for (i in (1:(dimvambien[2])))
{
   tkinsert(tlv,"end",textvariables[i])
}



for (i in 1:dimespec[1])
{
tkdelete(tll,0)
}

for (i in (1:(dimespec[1])))
{
   tkinsert(tll,"end",textlugares[i])
}


tkrreplot(img)
tkdestroy(mm)

}

OK.butli <-tkbutton(framemm2,text=gettext("Change label",domain="R-cncaGUI"),command=OnOKli,width=12)
tkbind(entry.Namei, "<Return>",OnOKli)
tkpack(entry.Namei,OK.butli,expand = "TRUE", side="left", fill = "both")


  Cexi <<- sizesVec[indexClosest]

tclvalue(Cexi) <<- sizesVec[indexClosest]
entry.Cexi <<-tkentry(framemm3,width="11",textvariable=Cexi)
NameCexi <<- Cexi 

OnOKci <- function()
{
NameCexi <<- tclvalue(Cexi)
sizesVec[indexClosest]<<-NameCexi

if (cblugares=="1"){
cexespecies<<-sizesVec[1:length(cexespecies)]
cexvariables<<-sizesVec[(length(cexespecies)+1):(length(cexespecies)+length(cexvariables))]
cexlugares<<-sizesVec[(length(cexespecies)+length(cexvariables)+1):(length(cexespecies)+length(cexvariables)+length(cexlugares))]
}else{
cexespecies<<-sizesVec[1:length(cexespecies)]
cexvariables<<-sizesVec[(length(cexespecies)+1):(length(cexespecies)+length(cexvariables))]
}


tkrreplot(img)
tkdestroy(mm)

}

OK.butci <-tkbutton(framemm3,text=gettext("Change size",domain="R-cncaGUI"),command=OnOKci,width=12)
tkbind(entry.Cexi, "<Return>",OnOKci)
tkpack(entry.Cexi,OK.butci,expand = "TRUE", side="left", fill = "both")



comboBox <- tkwidget(framemm4,"ComboBox",editable=FALSE,values=symbolos,width=8, text= simbolos[indexClosest])

chang.sym <- function()
{
    simChoice <<-symbolos[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
simbolos[indexClosest]<<-simChoice

if (cblugares=="1"){
simespecies<<-simbolos[1:length(simespecies)]
simvariables<<-simbolos[(length(simespecies)+1):(length(simespecies)+length(simvariables))]
simlugares<<-simbolos[(length(simespecies)+length(simvariables)+1):(length(simespecies)+length(simvariables)+length(collugares))]
}else{
simespecies<<-simbolos[1:length(simespecies)]
simvariables<<-simbolos[(length(simespecies)+1):(length(simespecies)+length(simvariables))]
}



tkrreplot(img)
tkdestroy(mm)


 }
Change.symbol <-tkbutton(framemm4,text=gettext("Change symbol",domain="R-cncaGUI"),command=chang.sym,width=12)
tkpack(comboBox,Change.symbol,side="left",expand="TRUE", fill="both")


tkpack(framemm1,framemm2,framemm3,framemm4, expand = "TRUE", side="top", fill = "both")


}

  
#############################################################################
### Functions to save the graph
#############################################################################
  SaveFileJPG <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Jpeg files} {.jpg .jpeg}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".jpg") 
                FileName <- paste(FileName, ".jpg", sep = "")
            jpeg(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96, quality = 50)
            plotFunction(screen = FALSE)
            dev.off()
        }
    }
    SaveFileMetafile <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Metafiles} {.wmf}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".wmf") 
                FileName <- paste(FileName, ".wmf", sep = "")
            win.metafile(FileName, width = 8, height = 8, restoreConsole = FALSE)
            plotFunction(screen = FALSE)
            dev.off()
        }
    }
    SaveFilePDF <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{PDF files} {.pdf}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".pdf") 
                FileName <- paste(FileName, ".pdf", sep = "")
            pdf(FileName, width = 7, height = 7)
            plotFunction(screen = FALSE)
            dev.off()
        }
    }
    SaveFileBmp <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Bitmap files} {.bmp}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".bmp") 
                FileName <- paste(FileName, ".bmp", sep = "")
            bmp(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96)
            plotFunction(screen = FALSE)
            dev.off()
        }
    }
    SaveFilePng <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Png files} {.png}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".png") 
                FileName <- paste(FileName, ".png", sep = "")
            png(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96)
            plotFunction(screen = FALSE)
            dev.off()
        }
    }

    Print <- function() {
        try(win.print(), silent = TRUE)
        if (geterrmessage() != "Error in win.print() : unable to start device devWindows\n") {
            plotFunction(screen = FALSE)
            dev.off()
        }
    }

  g3d<-function()
  {
      if (nejes>2)
     { 
      if (cblugares=="1"){
zCoords<<-datos[,dim3]
if(cblablugares=="1"){
labelsVec3d<<-c(textespecies, textvariables, textlugares)
sizesVec3d<<-c(cexespecies, cexvariables, cexlugares)
}else{
labelsVec3d<<-c(textespecies, textvariables, rep("", times =length(textlugares)))
  sizesVec3d<<-c(cexespecies, cexvariables, rep("", times =length(cexlugares)))
}
}else{
zCoords<<-datosr[,dim3]
labelsVec3d<<-c(textespecies, textvariables)
sizesVec3d<<-c(cexespecies, cexvariables)
}

bg3d("white")
      aspect3d("iso")
      lims <- par3d("bbox")
    
if (cbVal=="1"){
          axes3d()
          }
      points3d(xCoords,yCoords,zCoords, color=colores)
      texts3d(xCoords, yCoords, zCoords,labelsVec3d,color=colores, cex = as.numeric(sizesVec3d))
  
      
     for (i in 1:(dim(covambien)[1]))
     {
       linea<-rbind(covambien[i,c(dim1, dim2, dim3)],c(0,0,0))
       segments3d(linea[,1],linea[,2], linea[,3],color=colvariables[i])

     }
     rgl.bringtotop()
   }else{
        msg <- gettext("You have selected less than 3 dimensions. 3D-graph not available",domain="R-cncaGUI")
    tkmessageBox(message=msg)
      }
  }
  
   
    topMenugr <- tkmenu(wgr)
              tkconfigure(wgr, menu = topMenugr)
                menuFile <- tkmenu(topMenugr, tearoff = FALSE)
                menuSaveAs <- tkmenu(topMenugr, tearoff = FALSE)
                menu3d <- tkmenu(topMenugr, tearoff = FALSE)
                
                tkadd(menuFile, "command", label = gettext("Copy image",domain="R-cncaGUI"),
                  command = function() {
                    tkrreplot(img)
                  })
                tkadd(menuFile, "cascade", label = gettext("Save image",domain="R-cncaGUI"),
                  menu = menuSaveAs)
                tkadd(menuSaveAs, "command", label = gettext("PDF file",domain="R-cncaGUI"),
                  command = function() {
                    SaveFilePDF()
                  })
                tkadd(menuSaveAs, "command", label = gettext("Metafile",domain="R-cncaGUI"),
                  command = function() {
                    SaveFileMetafile()
                  })
                tkadd(menuSaveAs, "command", label = gettext("Bmp file",domain="R-cncaGUI"),
                  command = function() {
                    SaveFileBmp()
                  })
                tkadd(menuSaveAs, "command", label = gettext("Png file",domain="R-cncaGUI"),
                  command = function() {
                    SaveFilePng()
                  })
                tkadd(menuSaveAs, "command", label = gettext("Jpg/Jpeg file",domain="R-cncaGUI"),
                  command = function() {
                    SaveFileJPG()
                  })
                tkadd(menuFile, "command", label = gettext("Print image",domain="R-cncaGUI"),
                  command = function() {
                    Print()
                  })
                tkadd(menuFile, "separator")
                tkadd(menuFile, "command", label = gettext("Exit",domain="R-cncaGUI"), command = function() {
                  tkdestroy(wgr)
                })
                tkadd(menu3d, "command", label = "3D", command = function() {
                  g3d()
                })
                tkadd(topMenugr, "cascade", label = gettext("File",domain="R-cncaGUI"), menu = menuFile)
                tkadd(menuFile, "separator")
                tkadd(topMenugr, "cascade", label = "3D", menu = menu3d)
                
                 
OnLeftClick.up <- function(x,y)
{
  msg <- (gettext("-To change the label press Yes.\n-To remove it press No.\n-If you do not want to do anything press Cancel.",domain="R-cncaGUI"))
  mbval<<- tkmessageBox(title=gettext("Change of label",domain="R-cncaGUI"),
                       message=msg,type="yesnocancel",icon="question")
   if (tclvalue(mbval)=="yes"){  
indexLabeled <<- c(indexLabeled,indexClosest)
 }

if(tclvalue(mbval)=="no"){

 indexLabeledaux<<-c()
 for (i in (1:length(indexLabeled)))
    {
     if (indexLabeled[i]!=indexClosest)
indexLabeledaux <<- c(indexLabeledaux,indexLabeled[i])
    }
 indexLabeled<<-indexLabeledaux 
}

if(tclvalue(mbval)=="cancel"){
      if (cblugares=="1"){

textos[indexClosest,dim1] <<- anteriorx
textos[indexClosest,dim2] <<- anteriory
}else{
textosr[indexClosest,dim1] <<- anteriorx
textosr[indexClosest,dim2] <<- anteriory
}
 
}
  
tkrreplot(img)
   
   }


  OnLeftClick.move <- function(x,y)
{
  xClick <<- x
  yClick <<- y
  width  <<- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  height <<- as.numeric(tclvalue(tkwinfo("reqheight",img)))

  xMin <<- parPlotSize[1] * width
  xMax <<- parPlotSize[2] * width
  yMin <<- parPlotSize[3] * height
   yMax <<- parPlotSize[4] * height

  rangeX <<- usrCoords[2] - usrCoords[1]
 rangeY <<- usrCoords[4] - usrCoords[3]

  imgXcoords <<- (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <<- (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

 xClick <<- as.numeric(xClick)+0.5
  yClick <<- as.numeric(yClick)+0.5
  yClick <<- height - yClick

  xPlotCoord <<- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
   yPlotCoord <<- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)
   
  if (cblugares=="1"){

textos[indexClosest,dim1]<<-xPlotCoord
textos[indexClosest,dim2]<<-yPlotCoord
}else{
textosr[indexClosest,dim1]<<-xPlotCoord
textosr[indexClosest,dim2]<<-yPlotCoord
}
############################### 
tkrreplot(img) 
}


OnLeftClick.down <- function(x,y)
{
  xClick <<- x
  yClick <<- y
  width  <<- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  height <<- as.numeric(tclvalue(tkwinfo("reqheight",img)))

  xMin <<- parPlotSize[1] * width
  xMax <<- parPlotSize[2] * width
  yMin <<- parPlotSize[3] * height
   yMax <<- parPlotSize[4] * height

  rangeX <<- usrCoords[2] - usrCoords[1]
 rangeY <<- usrCoords[4] - usrCoords[3]

  imgXcoords <<- (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <<- (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

 xClick <<- as.numeric(xClick)+0.5
  yClick <<- as.numeric(yClick)+0.5
  yClick <<- height - yClick

  xPlotCoord <<- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
   yPlotCoord <<- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

  squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  indexClosest <<- which.min(squared.Distance) 
  
  if (cblugares=="1"){

anteriorx <<- textos[indexClosest,dim1]
anteriory <<- textos[indexClosest,dim2]
}else{
  anteriorx <<- textosr[indexClosest,dim1]
anteriory <<- textosr[indexClosest,dim2]
}
  
  }

OnRightClick <- function(x,y)
{
  xClick <<- x
  yClick <<- y
  width  <<- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  height <<- as.numeric(tclvalue(tkwinfo("reqheight",img)))

  xMin <<- parPlotSize[1] * width
  xMax <<- parPlotSize[2] * width
  yMin <<- parPlotSize[3] * height
   yMax <<- parPlotSize[4] * height

  rangeX <<- usrCoords[2] - usrCoords[1]
 rangeY <<- usrCoords[4] - usrCoords[3]

  imgXcoords <<- (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <<- (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

 xClick <<- as.numeric(xClick)+0.5
  yClick <<- as.numeric(yClick)+0.5
  yClick <<- height - yClick

  xPlotCoord <<- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
   yPlotCoord <<- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)


  labelClosestPointd(xClick,yClick,imgXcoords,imgYcoords)



}
  tkbind(img, "<B1-Motion>",OnLeftClick.move)
tkbind(img, "<ButtonPress-1>",OnLeftClick.down)
tkbind(img, "<ButtonRelease-1>",OnLeftClick.up)
tkconfigure(img,cursor="pencil")

tkbind(img, "<Button-3>",OnRightClick)
tkconfigure(img,cursor="pencil")

 }
    }  
    numaxis <- tclVar( 1 )
    enumaxis <-tkentry(barvp,width="50",textvariable=numaxis)
    but.axis <-tkbutton(barvp,text=gettext("Choose",domain="R-cncaGUI"),command=Onaxis, bg= "lightblue", width=10, foreground = "navyblue")
    tkpack(imgbar, expand="TRUE", fill="both")  
    
    tkpack(tklabel(barvp,text=gettext("Number of axes:",domain="R-cncaGUI")),
    enumaxis,but.axis,expand="FALSE", side= "left", fill ="both")
 
    tkfocus(barvp)
   }

graphic.button <-tkbutton(framegraphic,text=gettext("Graph",domain="R-cncaGUI"),command=Graphics, bg= "lightblue", width=20, foreground = "navyblue")
tkpack(graphic.button, expand="TRUE", side= "left", fill ="both")


tkpack(tlv,scrv,expand = "TRUE", side="left", fill = "both")
tkpack.configure(scrv,side="left")

tkpack(OK.butv,expand = "TRUE", side="left", fill = "both")
tkfocus(tt)



#######Color#######################################
indicev<-as.numeric(tkcurselection(tlv))+1
colorv <- colvariables[indicev[1]]
canvasv <- tkcanvas(framecol21,width="57",height="20",bg=colorv)

ChangeColorv <- function()
{
 
colorv <<- tclvalue(tcl("tk_chooseColor",initialcolor=colvariables[indicev[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
 if (nchar(colorv)>0)
    {
tkconfigure(canvasv,bg=colorv)
 colvariables[indicev]<<-colorv
}
}

ChangeColor.buttonv<- tkbutton(framecol22,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColorv,width=4)
tkpack(canvasv,expand = "TRUE", side="left", fill = "both")
  tkpack(ChangeColor.buttonv,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
Namev <- textvariables[indicev[1]]
entry.Namev <-tkentry(framename21,width="10",textvariable=Namev)

OnOKlv <- function()
{
NameValv <<- tclvalue(Namev)
textvariables[indicev]<<-NameValv

#####Values of listbox###############################
for (i in 1:dimvambien[2])
{
tkdelete(tlv,0)
}

for (i in (1:(dimvambien[2])))
{
   tkinsert(tlv,"end",textvariables[i])
}


}

OK.butlv <-tkbutton(framename22,text=gettext("Change label",domain="R-cncaGUI"),command=OnOKlv,width=4)
tkbind(entry.Namev, "<Return>",OnOKlv)
tkpack(entry.Namev,expand = "TRUE", side="left", fill = "both")
  tkpack(OK.butlv,expand = "TRUE", side="left", fill = "both")

###### Sizes  ###################################
Cexv <- cexvariables[indicev[1]]
entry.Cexv <-tkentry(framecex21,width="10",textvariable=Cexv)

OnOKcv <- function()
{
NameCexv <<- tclvalue(Cexv)
cexvariables[indicev]<<-NameCexv

}

OK.butcv <-tkbutton(framecex22,text=gettext("Change size",domain="R-cncaGUI"),command=OnOKcv,width=4)
tkbind(entry.Cexv, "<Return>",OnOKlv)
tkpack(entry.Cexv,expand = "TRUE", side="left", fill = "both")
  tkpack(OK.butcv,expand = "TRUE", side="left", fill = "both")


######Symbols###################################

tkpack(tklabel(frames21,text="      ",width=27),expand = "TRUE", side="left", fill = "both")

##### List of sites ###########################

indicel<-NULL
Namel<-NULL
NameVall<-NULL
Cexl<-1
NameCexl<-NULL


scrl <- tkscrollbar(framet3, repeatinterval=5,
   command=function(...)tkyview(tll,...))

tll<-tklistbox(framet3,height=6,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scrl,...),background="white")

tkpack(tklabel(frametext3,text=Namesit),side="left",expand = "TRUE",fill="both")

for (i in (1:(dimespec[1])))
{
    tkinsert(tll,"end",textlugares[i])
}
tkselection.set(tll,0) #  Indexing starts at zero.

OnOKl <- function()
{
Choicel <<- textespecies[as.numeric(tkcurselection(tll))+1]

##### Color of the selected variable #############
indicel<<-as.numeric(tkcurselection(tll))+1
colorl <- collugares[indicel[1]]
 tkconfigure(canvasl,bg=colorl)


##### Text of the selected variable  ###############

Namel <<- tclVar(textlugares[indicel[1]])
tkconfigure(entry.Namel,textvariable=Namel)
   tkconfigure(comboBoxl,text=simlugares[indicel[1]])

##### Size of the selected variable  ###############

Cexl <<- tclVar(cexlugares[indicel[1]])
tkconfigure(entry.Cexl,textvariable=Cexl)
   tkconfigure(comboBoxl,text=simlugares[indicel[1]])


}

OK.butl <-tkbutton(frameok3,text="    OK    ",command=OnOKl)
tkpack(tll,scrl,expand = "TRUE", side="left", fill = "both")
tkpack.configure(scrl,side="left")

tkpack(OK.butl,expand = "TRUE", side="left", fill = "both")
tkfocus(tt)



#######Color#######################################
indicel<-as.numeric(tkcurselection(tll))+1
colorl <- collugares[indicel[1]]
canvasl <- tkcanvas(framecol31,width="57",height="20",bg=colorl)

ChangeColorl <- function()
{
 
colorl <<- tclvalue(tcl("tk_chooseColor",initialcolor=collugares[indicel[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
 if (nchar(colorl)>0)
    {
tkconfigure(canvasl,bg=colorl)
 collugares[indicel]<<-colorl
}
}

ChangeColor.buttonl<- tkbutton(framecol32,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColorl,width=4)
tkpack(canvasl,expand = "TRUE", side="left", fill = "both")
  tkpack(ChangeColor.buttonl,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
Namel <- textlugares[indicel[1]]
entry.Namel <-tkentry(framename31,width="10",textvariable=Namel)

OnOKll <- function()
{
NameVall <<- tclvalue(Namel)
textlugares[indicel]<<-NameVall

#####Values of listbox###############################
for (i in 1:dimespec[1])
{
tkdelete(tll,0)
}

for (i in (1:(dimespec[1])))
{
   tkinsert(tll,"end",textlugares[i])
}


}

OK.butll <-tkbutton(framename32,text=gettext("Change label",domain="R-cncaGUI"),command=OnOKll,width=4)
tkbind(entry.Namel, "<Return>",OnOKll)
tkpack(entry.Namel,expand = "TRUE", side="left", fill = "both")
tkpack(OK.butll,expand = "TRUE", side="left", fill = "both")

######Labels  ###################################
Cexl <- cexlugares[indicel[1]]
entry.Cexl <-tkentry(framecex31,width="10",textvariable=Cexl)

OnOKcl <- function()
{
Namecexl <<- tclvalue(Cexl)
cexlugares[indicel]<<-NameCexl
}

OK.butcl <-tkbutton(framecex32,text=gettext("Change size",domain="R-cncaGUI"),command=OnOKcl,width=4)
tkbind(entry.Cexl, "<Return>",OnOKll)
tkpack(entry.Cexl,expand = "TRUE", side="left", fill = "both")
tkpack(OK.butcl,expand = "TRUE", side="left", fill = "both")

  
  ######Symbols###################################

comboBoxl <- tkwidget(frames31,"ComboBox",editable=FALSE,values=symbolos,width=7)

chang.syml <- function()
{
    simChoicel <<-symbolos[as.numeric(tclvalue(tcl(comboBoxl,"getvalue")))+1]
simlugares[indicel]<<-simChoicel

 }
Change.symboll <-tkbutton(frames32,text=gettext("Change symbol",domain="R-cncaGUI"),command=chang.syml,width=4)
tkpack(comboBoxl,side="left",expand="TRUE", fill="both")
tkpack(Change.symboll,side="left",expand="TRUE", fill="both")





                                                                      
tkpack(tklabel(frametext4,text=gettext("Transformations",domain="R-cncaGUI")),side="right",expand = "TRUE",fill="both")
tkpack(tltrans,scrtrans,expand = "TRUE", side="left", fill = "both")
tkpack.configure(scrtrans,side="left")

  tkpack(tklabel(framecol41,text=gettext("Show axes",domain="R-cncaGUI")),expand = "TRUE", side="left",expand="TRUE", fill = "both")
tkpack(cb,expand = "TRUE", side="left",expand="TRUE", fill = "both")

  tkpack(tklabel(framename41,text=paste(gettext("Show",domain="R-cncaGUI"), Namesit)),expand = "TRUE", side="left", fill = "both")
tkpack(cbl,expand = "TRUE", side="left", fill = "both")

  tkpack(tklabel(framecex41,text=paste(gettext("Show labels for",domain="R-cncaGUI"), Namesit)),expand = "TRUE", side="left", fill = "both")
  tkpack(cbll,expand = "TRUE", side="left", fill = "both")
  tkpack(tklabel(frames41,text="      ",width=27),expand = "TRUE", side="left", fill = "both")

  tkpack(framecol41,framecol42, side="left", expand = "TRUE", fill="both")
tkpack(framename41,framename42, side="left", expand = "TRUE", fill="both")
tkpack(framecex41,framecex42, side="left", expand = "TRUE", fill="both")
tkpack(frames41, side="left", expand = "TRUE", fill="both")

  tkpack(framecol11,framecol12, side="left", expand = "TRUE", fill="both")
tkpack(framename11,framename12, side="left", expand = "TRUE", fill="both")
tkpack(framecex11,framecex12, side="left", expand = "TRUE", fill="both")
tkpack(frames11,frames12, side="left", expand = "TRUE", fill="both")

  tkpack(framecol21,framecol22, side="left", expand = "TRUE", fill="both")
tkpack(framename21,framename22, side="left", expand = "TRUE", fill="both")
tkpack(framecex21,framecex22, side="left", expand = "TRUE", fill="both")
tkpack(frames21, side="left", expand = "TRUE", fill="both")

  tkpack(framecol31,framecol32, side="left", expand = "TRUE", fill="both")
tkpack(framename31,framename32, side="left", expand = "TRUE", fill="both")
tkpack(framecex31,framecex32, side="left", expand = "TRUE", fill="both")
tkpack(frames31,frames32, side="left", expand = "TRUE", fill="both")

  
  tkpack(frametext1,framet1,frameok1,framecol1,framename1,framecex1,frames1,expand = "TRUE", fill="both")
tkpack(frametext2,framet2,frameok2,framecol2,framename2,framecex2,frames2,expand = "TRUE", fill="both")
tkpack(frametext3,framet3,frameok3,framecol3,framename3,framecex3,frames3,expand = "TRUE", fill="both")
tkpack(frametext4,framet4,frameok4,framecol4,framename4,framecex4,frames4,expand = "TRUE", fill="both")

tkpack(framett1,framett2,framett3, framett4,expand = "TRUE",side="left", fill="both")
  tkpack(framett,framegraphic,expand = "TRUE",side="top", fill="y")


}
  framenames<-tkframe(wnames, relief = "flat", borderwidth = 2, 
        background = "white")

framee<-tkframe(framenames, relief = "flat", borderwidth = 2, 
        background = "white")

  framel<-tkframe(framenames, relief = "flat", borderwidth = 2, 
        background = "white")

  frameoknames<-tkframe(wnames, relief = "flat", borderwidth = 2, 
        background = "white")






 OK.butnames<-tkbutton(frameoknames,text="   OK   ", command=OnOKnames,  bg= "lightblue", width=20, foreground = "navyblue")
tkbind(OK.butnames, "<Return>",OnOKnames)

sitesname<-tclVar(gettext("Sites",domain="R-cncaGUI"))
speciesname<-tclVar(gettext("Species",domain="R-cncaGUI"))
variablesname<-tclVar(gettext("Environmental v",domain="R-cncaGUI"))

entry.Namesit <-tkentry(framee,width="50",textvariable=sitesname)
tkbind(entry.Namesit, "<Return>",OnOKnames)

entry.Namespec <-tkentry(framee,width="50",textvariable=speciesname)
tkbind(entry.Namespec, "<Return>",OnOKnames)

entry.Namev <-tkentry(framee,width="50",textvariable=variablesname)
tkbind(entry.Namev, "<Return>",OnOKnames)

tkpack(tklabel(framel,text=gettext("Names for sites:",domain="R-cncaGUI")),
  tklabel(framel,text=gettext("Names for species:",domain="R-cncaGUI")),
tklabel(framel,text=gettext("Names for environmental v:",domain="R-cncaGUI")), expand = "TRUE", side="top", fill = "both")
tkpack(OK.butnames)
  tkpack(entry.Namesit, entry.Namespec, entry.Namev, expand = "TRUE",side="top", fill="both")
  tkpack(framel, framee, expand = "TRUE",side="left", fill="y")
  tkpack(framenames, frameoknames, expand = "TRUE",side="top", fill="y")

  
  
  tkfocus(wnames)

}



OK.butinf <-tkbutton(winfor,text="   OK   ",command=OnOKinf, bg= "lightblue", width=20, foreground = "navyblue")
tkbind(OK.butinf, "<Return>",OnOKinf)

fontHeading <- tkfont.create(family="times",size=24,weight="bold",slant="italic")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)
tkgrid(tklabel(winfor,text="               CNCA:               ",font=fontHeading, foreground = "blue"))
tkgrid(tklabel(winfor,text="     Canonical Non-Symmetrical     ",font=fontHeading))
tkgrid(tklabel(winfor,text="Correspondence Analysis",font=fontHeading))
tkgrid(tklabel(winfor,text="                                   ",font=fontHeading))
tkgrid(OK.butinf)
tkfocus(winfor)

}
