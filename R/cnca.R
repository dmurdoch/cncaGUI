cnca <-
function()
{

#############################################################################
#########	libraries
#############################################################################

	library(RODBC)
	require(tcltk)
	library(tkrplot)
	library(rgl)
	tclRequire("BWidget")

#############################################################################
### Informative window
#############################################################################
symbols <- c("*",".", "o","O","0","+","-","|","%","#")
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
			
######################################################################
##We open the Excel file which contains the information of q species
## measured on n sites and of p environmental variables
##measured on the same sites
######################################################################
		
			tkdestroy(wnames)
			file <- tclvalue(tkgetOpenFile())
			if (!length(file)) return()
			channel <- odbcConnectExcel(file)
			hojastabla<-sqlTables(channel)$TABLE_NAME
	
			whoja<-tktoplevel()
			tkwm.title(whoja,gettext("Select one table",domain="R-cncaGUI"))
			tl<-tklistbox(whoja,height=4,selectmode="single",background="white")
			tkgrid(tklabel(whoja,text= paste(gettext("Select one table for (",domain="R-cncaGUI"), Namespe, ")")))
			tkgrid(tl)
	
			for (k in (1:(dim(sqlTables(channel))[1])))
			{
    				tkinsert(tl,"end",hojastabla[k])
			}

			tkselection.set(tl,0)  #Indexing starts at zero.

			OnOK <- function()
			{
				hojaChoice <<- hojastabla[as.numeric(tkcurselection(tl))+1]
				fespecies<<-sqlFetch(channel,hojaChoice)
				odbcClose(channel)
    				tkdestroy(whoja)
    			}
		
			OK.but <-tkbutton(whoja,text="   OK   ",command=OnOK)
			tkgrid(OK.but)
			tkfocus(whoja)
			tkwait.window(whoja)



			file <- tclvalue(tkgetOpenFile())
			if (!length(file)) return()
			channel <- odbcConnectExcel(file)
			hojastabla<-sqlTables(channel)$TABLE_NAME
	
			whoja<-tktoplevel()
			tkwm.title(whoja,gettext("Select one table",domain="R-cncaGUI"))
			tl<-tklistbox(whoja,height=4,selectmode="single",background="white")
	    tkgrid(tklabel(whoja,text=paste(gettext("Select one table for (",domain="R-cncaGUI"), Namevar, ")")))
			tkgrid(tl)
	
			for (k in 1:(dim(sqlTables(channel))[1]))
			{
    				tkinsert(tl,"end",hojastabla[k])
			}

			tkselection.set(tl,0)  #Indexing starts at zero.

			OnOK <- function()
			{
				hojaChoice <<- hojastabla[as.numeric(tkcurselection(tl))+1]
				fvambientales<<-sqlFetch(channel,hojaChoice)
				odbcClose(channel)
    				tkdestroy(whoja)
    			}
		
			OK.but <-tkbutton(whoja,text="   OK   ",command=OnOK)
			tkgrid(OK.but)
			tkfocus(whoja)
			tkwait.window(whoja)





	
	names(fespecies) <- make.names(names(fespecies))
	names(fvambientales) <- make.names(names(fvambientales))

##We save the dimensions
	dimespec<<-dim(fespecies)
	dimvambien<<-dim(fvambientales)

##We create the data matrices
	especies<<-array(dim=c(dimespec[1],(dimespec[2]-1)))
	for(i in 2:dimespec[2])
	{
		for(j in 1:dimespec[1]){
			especies[j,i-1]<<-fespecies[j,i]
		}
	}

	vambientales<<-array(dim=c(dimvambien[1],(dimvambien[2]-1)))
	for(i in 2:dimvambien[2])
	{
		for(j in 1:dimvambien[1]){
			vambientales[j,i-1]<<-fvambientales[j,i]
		}
	}





##We sum all elements of species
		sumatotal<<-sum(especies)

		Fe<<-especies/sumatotal

		fn<<-array(dim=c(dimespec[1],1))

##We calculate the marginal totals and we save it in fn and fq
		Idq<<-as.matrix(rep(1,times=dimespec[2]-1))
		fn<<-Fe%*%Idq	

		Idn<<-as.matrix(rep(1,times=dimespec[1]))
		fq<<-t(Fe)%*%Idn	


#Matrix whose diagonal is the marginals fn 	
		Dn<<-diag(as.vector(t(fn)))
		



###########################################################################################
##	We create the vectors of the labels
###########################################################################################

	textlugares<<-t(fespecies[1])
	textespecies<<-names(fespecies[2:dimespec[2]])
	textvariables<<-names(fvambientales[2:dimvambien[2]])


###########################################################################################
##	We create the vectors of the colors
###########################################################################################

	collugares<<-rep("green",dimespec[1])
	colespecies<<-rep("blue",dimespec[2]-1)
	colvariables<<-rep("red",dimvambien[2]-1)



###########################################################################################
##	We create the vectors of the symbols
###########################################################################################

	simlugares<<-rep("*",dimespec[1])
	simespecies<<-rep("+",dimespec[2]-1)
	simvariables<<-rep(" ",dimvambien[2]-1)






#####Window to change labels and colors#######


	tt<-tktoplevel()
	tkwm.title(tt,gettext("Options",domain="R-cncaGUI"))



#####Dropdown menu #############################


#### List of transformations
	framet<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

	frametext<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")


	frameok<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

	framecol<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

	framename<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

	frames<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

	framegraphic<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")


	scrtrans <- tkscrollbar(framet, repeatinterval=5,
				   command=function(...)tkyview(tltrans,...))
	tltrans<-tklistbox(framet,height=6,width=42,selectmode="single",yscrollcommand=function(...)tkset(scrtrans,...),background="white")
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

		OK.buttrans <-tkbutton(frameok,text="    OK    ",command=OnOKtrans)
		
		
		tkpack(OK.buttrans,expand = "TRUE", side="right", fill = "both")
		

		




##### Checkbox to show the axes or not  #######

	cb <- tkcheckbutton(framecol)
	cbValue <- tclVar("0")
	tkconfigure(cb,variable=cbValue)

##### Checkbox to show the sites or not  #######

	cbl <- tkcheckbutton(framename)
	cblug <- tclVar("0")
	tkconfigure(cbl,variable=cblug)
	


##### Checkbox to show the labels of the sites or not  #######

	cbll <- tkcheckbutton(frames)
	cblablug <- tclVar("0")
	tkconfigure(cbll,variable=cblablug)
	
	
##### List of species###########################



	indicee<-NULL
	Namee<-NULL
	NameVale<-NULL


	scre <- tkscrollbar(framet, repeatinterval=5,
				   command=function(...)tkyview(tle,...))

	tle<-tklistbox(framet,height=4,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scre,...),background="white")
	
	tkpack(tklabel(frametext,text=Namespe),side="left",expand = "TRUE",fill="both")

	for (i in 1:(dimespec[2]-1))
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
    
	}
	
	OK.bute <-tkbutton(frameok,text="    OK    ",command=OnOKe)
	tkpack(tle,scre,expand = "TRUE", side="left", fill = "both")
	tkpack.configure(scre,side="left")

	tkpack(OK.bute,expand = "TRUE", side="left", fill = "both")
	tkfocus(tt)



#######Color#######################################
	indicee<-as.numeric(tkcurselection(tle))+1
	colore <<- colespecies[indicee[1]]
	canvase <- tkcanvas(framecol,width="57",height="20",bg=colore)

	ChangeColore <- function()
	{
 
		colore <<- tclvalue(tcl("tk_chooseColor",initialcolor=colespecies[indicee[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
		 if (nchar(colore)>0)
    			{
				tkconfigure(canvase,bg=colore)
		 		colespecies[indicee]<<-colore
			}
	}

	ChangeColor.buttone<- tkbutton(framecol,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColore,width=2)
	tkpack(canvase,ChangeColor.buttone,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
	Namee <- textespecies[indicee[1]]
	entry.Namee <-tkentry(framename,width="10",textvariable=Namee)

	OnOKle <- function()
	{
		NameVale <<- tclvalue(Namee)
		textespecies[indicee[1]]<<-NameVale

#####Values of listbox###############################
	for (i in 1:(dimespec[2]-1))
	{
		tkdelete(tle,0)
	}

	for (i in (1:(dimespec[2]-1)))
	{
   		tkinsert(tle,"end",textespecies[i])
	}


	}

	OK.butle <-tkbutton(framename,text=gettext(" Change label",domain="R-cncaGUI"),command=OnOKle,width=2)
	tkbind(entry.Namee, "<Return>",OnOKle)
	tkpack(entry.Namee,OK.butle,expand = "TRUE", side="left", fill = "both")
	

######Symbols###################################

comboBoxe <- tkwidget(frames,"ComboBox",editable=FALSE,values=symbols,width=10)

chang.syme <- function()
{
    simChoicee <<- symbols[as.numeric(tclvalue(tcl(comboBoxe,"getvalue")))+1]
	simespecies[indicee]<<-simChoicee
 }
Change.symbole <-tkbutton(frames,text=gettext("   Change symbol   ",domain="R-cncaGUI"),command=chang.syme,width=6)
tkpack(comboBoxe,Change.symbole,side="left",expand="TRUE", fill="both")








##### List of environmental v. ###########################


	indicev<-NULL
	Namev<-NULL
	NameValv<-NULL


	scrv <- tkscrollbar(framet, repeatinterval=5,
				   command=function(...)tkyview(tlv,...))

	tlv<-tklistbox(framet,height=4,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scrv,...),background="white")
	
	tkpack(tklabel(frametext,text=Namevar),side="left",expand = "TRUE",fill="both")

	for (i in (1:(dimvambien[2]-1)))
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
    
	}
	
	OK.butv <-tkbutton(frameok,text="    OK    ",command=OnOKv)

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

##########################################################################################
###	Singular Value Decomposition of Pbest
##########################################################################################

		descpbest<<-La.svd(Pbest,nu=3,nv=3)

		R<<-descpbest$u

	
##########################################################################################
###	Matrix Lp
##########################################################################################

		Fb<<-Fe-(fn%*%t(fq))
	
		L<<-t(Fb)%*%vambientalesst

		Lp<<-((descovar$u%*%(solve(diag(descovar$d)^(1/2)))%*%descovar$v))%*%t(L)

	
##########################################################################################
###	Singular Value Decomposition of Lp
##########################################################################################

		desclp<<-svd(Lp,nu=3,nv=3)

		A<<-desclp$u

		V<<-diag(desclp$d[1:3]) 
		
		Tn<<-desclp$v
	

###########################################################################################
##	Principal coordinates of sites
###########################################################################################

		colugares<<-R%*%(V)

###########################################################################################
##	Standard coordinates of species
###########################################################################################

		coespecies<<-Tn


###########################################################################################
##	Principal coordinates of environmental variables
###########################################################################################

		covambien<<-((descovar$u%*%(diag(descovar$d)^(1/2))%*%descovar$v))%*%A%*%(V)


##########################################################################
#### Contributions and qualities of representation
##########################################################################
		ejes<<-c(gettext("Axis 1",domain="R-cncaGUI"),gettext("Axis 2",domain="R-cncaGUI"),gettext("Axis 3",domain="R-cncaGUI"))

####Contributions of the sites
		colugarescuad<<-colugares^2
		fixi<<-colugarescuad

		fixi[,1]<<-fn*colugarescuad[,1]
		fixi[,2]<<-fn*colugarescuad[,2]
    fixi[,3]<<-fn*colugarescuad[,3]

		Calphai<<-fixi

		Calphai[,1]<<-(fixi[,1]*1000)/descpbest$d[1]
		Calphai[,2]<<-(fixi[,2]*1000)/descpbest$d[2]
		Calphai[,3]<<-(fixi[,3]*1000)/descpbest$d[3]
		rownames(Calphai)<<-textlugares
		colnames(Calphai)<<-ejes

####Contributions of the species

		coespeciescuad<<-(Tn%*%V)^2


		Calphak<<-coespeciescuad

		Calphak[,1]<<-(coespeciescuad[,1]*1000)/descpbest$d[1]
		Calphak[,2]<<-(coespeciescuad[,2]*1000)/descpbest$d[2]
		Calphak[,3]<<-(coespeciescuad[,3]*1000)/descpbest$d[3]
		rownames(Calphak)<<-textespecies
		colnames(Calphak)<<-ejes


####qualities of representation respect to the projected space

		variabilidadps<<-sum(descpbest$d[1:3])

		calidadpst<<-descpbest$d[1:3]/variabilidadps
		calidadps<<-t(calidadpst)*1000
		colnames(calidadps)<<-ejes


####qualities of representation respect to the original space

		variabilidados<<-sum(descpbest$d)

		calidadost<<-descpbest$d[1:3]/variabilidados
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

		qalphaips[,1]<<-(colugarescuad[,1]*1000)/dpri
		qalphaips[,2]<<-(colugarescuad[,2]*1000)/dpri
		qalphaips[,3]<<-(colugarescuad[,3]*1000)/dpri
		rownames(qalphaips)<<-textlugares
		colnames(qalphaips)<<-ejes


		dalphaios<<-colugarescuad

		dalphaios[,1]<<-(colugarescuad[,1]*1000)/doi
		dalphaios[,2]<<-(colugarescuad[,2]*1000)/doi
		dalphaios[,3]<<-(colugarescuad[,3]*1000)/doi
		rownames(dalphaios)<<-textlugares
		colnames(dalphaios)<<-ejes



##### Relative to species

		qalphakps<<-coespeciescuad

		qalphakps[,1]<<-(coespeciescuad[,1]*1000)/dprk
		qalphakps[,2]<<-(coespeciescuad[,2]*1000)/dprk
		qalphakps[,3]<<-(coespeciescuad[,3]*1000)/dprk
		rownames(qalphakps)<<-textespecies
		colnames(qalphakps)<<-ejes


		qalphakos<<-coespeciescuad


		qalphakos[,1]<<-(coespeciescuad[,1]*1000)/dok
		qalphakos[,2]<<-(coespeciescuad[,2]*1000)/dok
		qalphakos[,3]<<-(coespeciescuad[,3]*1000)/dok
		rownames(qalphakos)<<-textespecies
		colnames(qalphakos)<<-ejes




		coindividuosnam<<-as.data.frame(coespecies)
		rownames(coindividuosnam)<<-textespecies
		colnames(coindividuosnam)<<-ejes

		covariablesnam<<-as.data.frame(covambien)
		rownames(covariablesnam)<<-textvariables
		colnames(covariablesnam)<<-ejes

		colugaresnam<<-as.data.frame(colugares)
		rownames(colugaresnam)<<-textlugares
		colnames(colugaresnam)<<-ejes


			cat(gettext("File saved in:    ",domain="R-cncaGUI"),file=gettext("Results.xls",domain="R-cncaGUI"))
			cat(getwd(),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("CONTRIBUTIONS:\n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(gettext("Contributions to",domain="R-cncaGUI"), Namesit,":\n"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(Calphai,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")

			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(gettext("Contributions to",domain="R-cncaGUI"), Namespe,":\n"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(Calphak,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("QUALITIES OF REPRESENTATION\n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")

			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("Proportion of inertia explained for each axis (projected space):\n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(calidadps,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("Proportion of inertia explained for each axis (original space):\n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(calidados,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("Qualities of representation relative to elements:\n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namesit,gettext("respect to projected space:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(qalphaips,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namesit,gettext("respect to original space:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(dalphaios,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")



			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namespe,gettext("respect to projected space:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(qalphakps,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namespe,gettext("respect to original space:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(qalphakos,file="temp.xls", sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")



			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namespe,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(coindividuosnam,file="temp.xls",sep="\t",dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")




			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namevar,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(covariablesnam, file="temp.xls", sep="\t", dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(paste(Namesit,gettext("coordinates:\n",domain="R-cncaGUI")),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(colugaresnam, file="temp.xls", sep="\t", dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")


			cat("\n",file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			cat(gettext("Eigen values: \n",domain="R-cncaGUI"),file="temp.xls")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")
			write.table(descpbest$d, file="temp.xls", sep="\t", dec=",")
			file.append(gettext("Results.xls",domain="R-cncaGUI"),"temp.xls")




			file.show(gettext("Results.xls",domain="R-cncaGUI"))
			file.remove("temp.xls")

###########################################################################################
##	Rescale
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
		datosr<<-rbind(coespecies,covambien)
		centro<<-c(0,0)
		




################ Show axes or not
	cbVal <<- as.character(tclvalue(cbValue))

	cblugares <<- as.character(tclvalue(cblug))

	cblablugares<<-as.character(tclvalue(cblablug))

	simbolos<<-c(simespecies, simvariables, simlugares)

################ Show sites or not
	if (cblugares=="1"){

		xCoords<<-datos[,1]
		yCoords<<-datos[,2]
		zCoords<<-datos[,3]
	
		if (cblablugares=="1"){
			labelsVec <<- c(textespecies, textvariables,textlugares)
		}else{
			labelsVec <<- c(textespecies, textvariables)

		}

	
		colores<<-c(colespecies,colvariables,collugares)


	}else{
		xCoords<<-datosr[,1]
		yCoords<<-datosr[,2]
		zCoords<<-datosr[,3]
		labelsVec <<- c(textespecies, textvariables)
		colores<<-c(colespecies,colvariables)
		
	}

	indexLabeled<-c(1:length(xCoords))
	indexLabeledaux<-c()
	labeledPoints <- list()

	
	wgr <- tktoplevel()
	tkwm.title(wgr,gettext("Graph",domain="R-cncaGUI"))
	parPlotSize <- c()
	usrCoords <- c()

	plotFunction <- function()
	{
  		params <- par(bg="white")
 		if (cblugares=="1"){
		 	plot(datos[,1],datos[,2],main=gettext("Graph",domain="R-cncaGUI"),type= "n", xlab=" ",ylab=" ")
	

		}else{
		 	plot(datosr[,1],datosr[,2],main=gettext("Graph",domain="R-cncaGUI"),type= "n", xlab=" ",ylab=" ")

		}
	
 		
		points(coespecies[,1],coespecies[,2],pch=simespecies, col=colespecies)

		arrows(centro[1],centro[2],covambien[,1],covambien[,2],col=colvariables,lty="dotted", length=0.05)

	

		points(centro[1],centro[2],pch=18,col="black")

		if (cbVal=="1"){
	
			abline(h=centro[2],v=centro[1],lty="dotted")
		}


		if (cblugares=="1"){
			points(colugares[,1],colugares[,2],pch=simlugares, col=collugares)
		
			if (cblablugares=="1"){
			}

		}

  		if (length(indexLabeled)>0)
    		for (i in (1:length(indexLabeled)))
    		{
      		indexClosest <- indexLabeled[i]
      		text(xCoords[indexClosest],yCoords[indexClosest],
           		labels=labelsVec[indexClosest], col= colores[indexClosest])
			    		}
  		parPlotSize <<- par("plt")
  		usrCoords   <<- par("usr")
  		par(params)
	}

	img <- tkrplot(wgr,fun=plotFunction,hscale=1.5,vscale=1.5)
	tkpack(img, expand="TRUE", fill="both")


	labelClosestPoint <- function(xClick,yClick,imgXcoords,imgYcoords)
	{
  		squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  		indexClosest <<- which.min(squared.Distance)
 		if (tclvalue(mbval)=="yes"){  
			indexLabeled <<- c(indexLabeled,indexClosest)


###############################
			xCoords[indexClosest]<<-xPlotCoord
			yCoords[indexClosest]<<-yPlotCoord
###############################

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
  
	tkrreplot(img)
	}


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



		colori <- colores[indexClosest]
		canvasi <- tkcanvas(framemm1,width="120",height="20",bg=colori)

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

		ChangeColor.buttoni<- tkbutton(framemm1,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColori)
		tkpack(canvasi,ChangeColor.buttoni,expand = "TRUE", side="left", fill = "both")
		
		Namei <<- labelsVec[indexClosest]

		tclvalue(Namei) <<- labelsVec[indexClosest]
		entry.Namei <<-tkentry(framemm2,width="10",textvariable=Namei)
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
			
			for (i in 1:(dimespec[2]-1))
			{
				tkdelete(tle,0)
			}

			for (i in (1:(dimespec[2]-1)))
			{
 		  		tkinsert(tle,"end",textespecies[i])
			}




			for (i in 1:dimvambien[2]-1)
			{
				tkdelete(tlv,0)
			}

			for (i in (1:(dimvambien[2]-1)))
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

	OK.butli <-tkbutton(framemm2,text=gettext(" Change label",domain="R-cncaGUI"),command=OnOKli,width=2)
	tkbind(entry.Namei, "<Return>",OnOKli)
	tkpack(entry.Namei,OK.butli,expand = "TRUE", side="left", fill = "both")
	


	comboBox <- tkwidget(framemm3,"ComboBox",editable=FALSE,values=symbols,width=10, text= simbolos[indexClosest])

	chang.sym <- function()
	{
   	 	simChoice <<-symbols[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
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
	Change.symbol <-tkbutton(framemm3,text=gettext("   Change symbol   ",domain="R-cncaGUI"),command=chang.sym,width=6)
	tkpack(comboBox,Change.symbol,side="left",expand="TRUE", fill="both")


	tkpack(framemm1,framemm2,framemm3,expand = "TRUE", side="top", fill = "both")


	}


	CopyToClip <- function()
	{
  		tkrreplot(img)
	}

	copy.but <- tkbutton(wgr,text=gettext("Export",domain="R-cncaGUI"),command=CopyToClip)
	tkpack(img, expand="TRUE", fill="both")
	#tkpack(copy.but,expand="TRUE", fill="both")

  g3d<-function()
  {
      labelsVec3d <<- c(textespecies, textvariables,textlugares)
      
      bg3d("white")
      if (cbVal=="1"){
          axes3d()
          }
      points3d(xCoords,yCoords,zCoords, color=colores)
      texts3d(xCoords, yCoords, zCoords,labelsVec3d,color=colores)
  	
      
	     for (i in 1:(dim(covambien)[1]))
	     {
	       linea<-rbind(covambien[i,],c(0,0,0))	
	       segments3d(linea[,1],linea[,2], linea[,3],color=colvariables[i])

	     }
  }
  
  but.3d <- tkbutton(wgr,text=gettext("3D",domain="R-cncaGUI"),command=g3d)
	tkpack(but.3d, copy.but,side="left",expand="TRUE", fill="both")


	OnLeftClick <- function(x,y)
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

	  msg <- (gettext("-To change the label press Yes.\n-To remove it press No.\n-If you do not want to do anything press Cancel.",domain="R-cncaGUI"))
	  mbval<<- tkmessageBox(title=gettext("Change of label",domain="R-cncaGUI"),
                       message=msg,type="yesnocancel",icon="question")

 	 labelClosestPoint(xClick,yClick,imgXcoords,imgYcoords)

	

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

	tkbind(img, "<ButtonRelease-1>",OnLeftClick)
	tkconfigure(img,cursor="pencil")
	
	tkbind(img, "<Button-3>",OnRightClick)
	tkconfigure(img,cursor="pencil")

	}
	graphic.button <-tkbutton(framegraphic,text=gettext("    Graph    ",domain="R-cncaGUI"),command=Graphics, width=12)
	tkpack(graphic.button, expand="TRUE", side= "left", fill ="both")


	tkpack(tlv,scrv,expand = "TRUE", side="left", fill = "both")
	tkpack.configure(scrv,side="left")

	tkpack(OK.butv,expand = "TRUE", side="left", fill = "both")
	tkfocus(tt)



#######Color#######################################
	indicev<-as.numeric(tkcurselection(tlv))+1
	colorv <- colvariables[indicev[1]]
	canvasv <- tkcanvas(framecol,width="57",height="20",bg=colorv)

	ChangeColorv <- function()
	{
 
		colorv <<- tclvalue(tcl("tk_chooseColor",initialcolor=colvariables[indicev[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
		 if (nchar(colorv)>0)
    			{
				tkconfigure(canvasv,bg=colorv)
		 		colvariables[indicev]<<-colorv
			}
	}

	ChangeColor.buttonv<- tkbutton(framecol,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColorv,width=2)
	tkpack(canvasv,ChangeColor.buttonv,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
	Namev <- textvariables[indicev[1]]
	entry.Namev <-tkentry(framename,width="10",textvariable=Namev)

	OnOKlv <- function()
	{
		NameValv <<- tclvalue(Namev)
		textvariables[indicev]<<-NameValv

#####Values of listbox###############################
	for (i in 1:dimvambien[2]-1)
	{
		tkdelete(tlv,0)
	}

	for (i in (1:(dimvambien[2]-1)))
	{
   		tkinsert(tlv,"end",textvariables[i])
	}


	}

	OK.butlv <-tkbutton(framename,text=gettext(" Change label",domain="R-cncaGUI"),command=OnOKlv,width=2)
	tkbind(entry.Namev, "<Return>",OnOKlv)
	tkpack(entry.Namev,OK.butlv,expand = "TRUE", side="left", fill = "both")


######Symbols###################################

tkpack(tklabel(frames,text="      ",width=27),expand = "TRUE", side="left", fill = "both")

##### List of sites ###########################

	indicel<-NULL
	Namel<-NULL
	NameVall<-NULL


	scrl <- tkscrollbar(framet, repeatinterval=5,
				   command=function(...)tkyview(tll,...))

	tll<-tklistbox(framet,height=4,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scrl,...),background="white")
	
	tkpack(tklabel(frametext,text=Namesit),side="left",expand = "TRUE",fill="both")

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


	}
	
	OK.butl <-tkbutton(frameok,text="    OK    ",command=OnOKl)
	tkpack(tll,scrl,expand = "TRUE", side="left", fill = "both")
	tkpack.configure(scrl,side="left")

	tkpack(OK.butl,expand = "TRUE", side="left", fill = "both")
	tkfocus(tt)



#######Color#######################################
	indicel<-as.numeric(tkcurselection(tll))+1
	colorl <- collugares[indicel[1]]
	canvasl <- tkcanvas(framecol,width="57",height="20",bg=colorl)

	ChangeColorl <- function()
	{
 
		colorl <<- tclvalue(tcl("tk_chooseColor",initialcolor=collugares[indicel[1]],title=gettext("Choose a color",domain="R-cncaGUI")))
 
		 if (nchar(colorl)>0)
    			{
				tkconfigure(canvasl,bg=colorl)
		 		collugares[indicel]<<-colorl
			}
	}

	ChangeColor.buttonl<- tkbutton(framecol,text=gettext("Change Color",domain="R-cncaGUI"),command=ChangeColorl,width=2)
	tkpack(canvasl,ChangeColor.buttonl,expand = "TRUE", side="left", fill = "both")
 

######Labels  ###################################
	Namel <- textlugares[indicel[1]]
	entry.Namel <-tkentry(framename,width="10",textvariable=Namel)

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

	OK.butll <-tkbutton(framename,text=gettext(" Change label",domain="R-cncaGUI"),command=OnOKll,width=2)
	tkbind(entry.Namel, "<Return>",OnOKll)
	tkpack(entry.Namel,OK.butll,expand = "TRUE", side="left", fill = "both")
	
######Symbols###################################

comboBoxl <- tkwidget(frames,"ComboBox",editable=FALSE,values=symbols,width=10)

chang.syml <- function()
{
    simChoicel <<-symbols[as.numeric(tclvalue(tcl(comboBoxl,"getvalue")))+1]
	simlugares[indicel]<<-simChoicel

 }
Change.symboll <-tkbutton(frames,text=gettext("   Change symbol   ",domain="R-cncaGUI"),command=chang.syml,width=6)
tkpack(comboBoxl,Change.symboll,side="left",expand="TRUE", fill="both")





                                                                      
	tkpack(tklabel(frametext,text=gettext("Transformations",domain="R-cncaGUI")),side="right",expand = "TRUE",fill="both")
	tkpack(tltrans,scrtrans,expand = "TRUE", side="left", fill = "both")
	tkpack.configure(scrtrans,side="left")
	tkpack(tklabel(framecol,text=gettext("Show axes",domain="R-cncaGUI")),cb,expand = "TRUE", side="left",expand="TRUE", fill = "both")
	tkpack(tklabel(framename,text=paste(gettext("Show",domain="R-cncaGUI"), Namesit)),cbl,expand = "TRUE", side="left", fill = "both")
	tkpack(tklabel(frames,text=paste(gettext("Show labels for",domain="R-cncaGUI"), Namesit)),cbll,expand = "TRUE", side="left", fill = "both")


	tkpack(frametext,framet,frameok,framecol,framename,frames,framegraphic,expand = "TRUE",fill="both")

}

 OK.butnames<-tkbutton(wnames,text="   OK   ", command=OnOKnames)
	tkbind(OK.butnames, "<Return>",OnOKnames)
	
	sitesname<-tclVar(gettext("Sites",domain="R-cncaGUI"))
	speciesname<-tclVar(gettext("Species",domain="R-cncaGUI"))
	variablesname<-tclVar(gettext("Environmental v",domain="R-cncaGUI"))

	entry.Namesit <-tkentry(wnames,width="50",textvariable=sitesname)
	tkbind(entry.Namesit, "<Return>",OnOKnames)

	entry.Namespec <-tkentry(wnames,width="50",textvariable=speciesname)
	tkbind(entry.Namespec, "<Return>",OnOKnames)

	entry.Namev <-tkentry(wnames,width="50",textvariable=variablesname)
	tkbind(entry.Namev, "<Return>",OnOKnames)

	tkgrid(tklabel(wnames,text=gettext("Names for sites:",domain="R-cncaGUI")))
	tkgrid(entry.Namesit)
	tkgrid(tklabel(wnames,text=gettext("Names for species:",domain="R-cncaGUI")))
	tkgrid(entry.Namespec)
	tkgrid(tklabel(wnames,text=gettext("Names for environmental v:",domain="R-cncaGUI")))
	tkgrid(entry.Namev)
	
	tkgrid(OK.butnames)
	tkfocus(wnames)
	
	}


	
OK.butinf <-tkbutton(winfor,text="   OK   ",command=OnOKinf)
tkbind(OK.butinf, "<Return>",OnOKinf)

fontHeading <- tkfont.create(family="times",size=24,weight="bold",slant="italic")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)
tkgrid(tklabel(winfor,text="               CNCA               ",font=fontHeading))
tkgrid(OK.butinf)
tkfocus(winfor)

}

