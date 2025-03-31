/***********************************************************************Lasso avec Choose = SBC / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=SBC stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table1 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table1; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = SBC and Stop = SBC)";
run;



/***********************************************************************Lasso avec Choose = AIC / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=AIC stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table2 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table2; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = AIC and stop = SBC)";
run;


/***********************************************************************Lasso avec Choose = BIC / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=BIC stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table3 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table3; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = BIC and stop = SBC)";
run;




/***********************************************************************Lasso avec Choose = Cp / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=Cp stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table4 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table4; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = Cp and stop = SBC)";
run;




/***********************************************************************Lasso avec Choose = CV / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=CV stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table5 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table5; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = CV and stop = SBC)";
run;




/***********************************************************************Lasso avec Choose = ADJRSQ / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=ADJRSQ stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table6 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table6; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = ADJRSQ and stop = SBC)";
run;



/***********************************************************************Lasso avec Choose = AICc / stop = SBC ********************************************************************/


proc iml ; 
mu1 = j(7, 1, 0); /* On crée le vecteur mu1 qui correspond aux variables corrélées, c'est a dire 7 */ 

vcov1 = toeplitz({1, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55}); 
/* On créer la matrice de variance covariance en injectant de la colinéarité seulement sur les 7 premières variables afin d'avoir de la corrélation intern */

mu2 = j(43, 1, 0); /* Le vecteur mu2 qui correspond aux variables non corrélées, donc 43 */

vcov2 = I(43); /* La matrice de variance covariance d'odre 43 qui correspond aux variables non corrélées */

TT=100 ; /* On a 100 observations */ 
call randseed(4321); 

overfitting = 0; 
underfitting = 0; 
perfectfitting = 0; 
WrongModel = 0; 
X1freq = 0 ; X2freq = 0 ; X3freq = 0 ; X4freq = 0 ; X5freq = 0 ; X6freq = 0 ; X7freq = 0 ; X8freq = 0 ; 
X9freq = 0 ; X10freq = 0 ; X11freq = 0 ; X12freq = 0 ; X13freq = 0 ; X14freq = 0 ; X15freq = 0 ; 
X16freq = 0 ; X17freq = 0 ; X18freq = 0 ; X19freq = 0 ; X20freq = 0 ; X21freq = 0 ; X22freq = 0 ; 
X23freq = 0 ; X24freq = 0 ; X25freq = 0 ; X26freq = 0 ; X27freq = 0 ; X28freq = 0 ; X29freq = 0 ; 
X30freq = 0 ; X31freq = 0 ; X32freq = 0 ; X33freq = 0 ; X34freq = 0 ; X35freq = 0 ; X36freq = 0 ; 
X37freq = 0 ; X38freq = 0 ; X39freq = 0 ; X40freq = 0 ; X41freq = 0 ; X42freq = 0 ; X43freq = 0 ; 
X44freq = 0 ; X45freq = 0 ; X46freq = 0 ; X47freq = 0 ; X48freq = 0 ; X49freq = 0 ; X50freq = 0 ;
intfreq=0; 

Do t=1 to 1000 ; 

	Xcorr = randnormal(TT, Mu1, vcov1); 

	Xind = randnormal(TT, Mu2, vcov2); 

	eps = normal(j(TT,1,1))*0.1; 

	beta = {-0.7, 0.8, 0.4, 0.67, 0.77, -0.25, 0.44}; 

	y = Xcorr[,1:7]*beta+eps; 
 
	table = y|| Xcorr || Xind; 
	name = 'y' // ('X1' : 'X50')`; 

	/* Création de la table */ 
	create Lasso from table[colname = name]; 
	append from table; 
	close Lasso; 

submit; 
	proc glmselect data=Lasso outdesign=Lasso1 noprint;
    model Y = X1 - X50 / selection=Lasso (Choose=AICc stop=SBC LSCOEFFS);
	run;
	proc transpose data=Lasso1 out=Lasso1;
	run;
	endsubmit; 

	use Lasso1; 
	read all; 
	close Lasso1; 

	Vrai="Intercept"//("X1":"X7")`;
	selection=_label_[1:nrow(_LABEL_)-1];
	*print vrai; 
	*print selection; 


	/* Nombre de selection de chaque variable */

	do i=1 to nrow(selection); 
    if "X1" = selection[i] then X1freq = X1freq +1; else X1freq = X1freq; 
    if "X2" = selection[i] then X2freq = X2freq +1; else X2freq = X2freq; 
    if "X3" = selection[i] then X3freq = X3freq +1; else X3freq = X3freq; 
    if "X4" = selection[i] then X4freq = X4freq +1; else X4freq = X4freq; 
    if "X5" = selection[i] then X5freq = X5freq +1; else X5freq = X5freq; 
    if "X6" = selection[i] then X6freq = X6freq +1; else X6freq = X6freq; 
    if "X7" = selection[i] then X7freq = X7freq +1; else X7freq = X7freq; 
    if "X8" = selection[i] then X8freq = X8freq +1; else X8freq = X8freq; 
    if "X9" = selection[i] then X9freq = X9freq +1; else X9freq = X9freq; 
    if "X10" = selection[i] then X10freq = X10freq +1; else X10freq = X10freq; 
    if "X11" = selection[i] then X11freq = X11freq +1; else X11freq = X11freq; 
    if "X12" = selection[i] then X12freq = X12freq +1; else X12freq = X12freq; 
    if "X13" = selection[i] then X13freq = X13freq +1; else X13freq = X13freq; 
    if "X14" = selection[i] then X14freq = X14freq +1; else X14freq = X14freq; 
    if "X15" = selection[i] then X15freq = X15freq +1; else X15freq = X15freq; 
    if "X16" = selection[i] then X16freq = X16freq +1; else X16freq = X16freq; 
    if "X17" = selection[i] then X17freq = X17freq +1; else X17freq = X17freq; 
    if "X18" = selection[i] then X18freq = X18freq +1; else X18freq = X18freq; 
    if "X19" = selection[i] then X19freq = X19freq +1; else X19freq = X19freq; 
    if "X20" = selection[i] then X20freq = X20freq +1; else X20freq = X20freq; 
    if "X21" = selection[i] then X21freq = X21freq +1; else X21freq = X21freq; 
    if "X22" = selection[i] then X22freq = X22freq +1; else X22freq = X22freq; 
    if "X23" = selection[i] then X23freq = X23freq +1; else X23freq = X23freq; 
    if "X24" = selection[i] then X24freq = X24freq +1; else X24freq = X24freq; 
    if "X25" = selection[i] then X25freq = X25freq +1; else X25freq = X25freq; 
    if "X26" = selection[i] then X26freq = X26freq +1; else X26freq = X26freq; 
    if "X27" = selection[i] then X27freq = X27freq +1; else X27freq = X27freq; 
    if "X28" = selection[i] then X28freq = X28freq +1; else X28freq = X28freq; 
    if "X29" = selection[i] then X29freq = X29freq +1; else X29freq = X29freq; 
    if "X30" = selection[i] then X30freq = X30freq +1; else X30freq = X30freq; 
    if "X31" = selection[i] then X31freq = X31freq +1; else X31freq = X31freq; 
    if "X32" = selection[i] then X32freq = X32freq +1; else X32freq = X32freq; 
    if "X33" = selection[i] then X33freq = X33freq +1; else X33freq = X33freq; 
    if "X34" = selection[i] then X34freq = X34freq +1; else X34freq = X34freq; 
    if "X35" = selection[i] then X35freq = X35freq +1; else X35freq = X35freq; 
    if "X36" = selection[i] then X36freq = X36freq +1; else X36freq = X36freq; 
    if "X37" = selection[i] then X37freq = X37freq +1; else X37freq = X37freq; 
    if "X38" = selection[i] then X38freq = X38freq +1; else X38freq = X38freq; 
    if "X39" = selection[i] then X39freq = X39freq +1; else X39freq = X39freq; 
    if "X40" = selection[i] then X40freq = X40freq +1; else X40freq = X40freq; 
    if "X41" = selection[i] then X41freq = X41freq +1; else X41freq = X41freq; 
    if "X42" = selection[i] then X42freq = X42freq +1; else X42freq = X42freq; 
    if "X43" = selection[i] then X43freq = X43freq +1; else X43freq = X43freq; 
    if "X44" = selection[i] then X44freq = X44freq +1; else X44freq = X44freq; 
    if "X45" = selection[i] then X45freq = X45freq +1; else X45freq = X45freq; 
    if "X46" = selection[i] then X46freq = X46freq +1; else X46freq = X46freq; 
    if "X47" = selection[i] then X47freq = X47freq +1; else X47freq = X47freq; 
    if "X48" = selection[i] then X48freq = X48freq +1; else X48freq = X48freq; 
    if "X49" = selection[i] then X49freq = X49freq +1; else X49freq = X49freq; 
    if "X50" = selection[i] then X50freq = X50freq +1; else X50freq = X50freq; 
end;

	ensemble = xsect(vrai, selection);  

	/* Perfectfit */ 
	if ncol(ensemble)=8 & nrow(selection)=8 then perfectfitting = perfectfitting +1; 
	else perfectfitting = perfectfitting ;

	/* Over*/ 
	if ncol(ensemble)=8 & nrow(selection)>8 then overfitting = overfitting +1 ; 
	else overfitting = overfitting ;

	/*Under*/ 
	if ncol(ensemble)>1 & ncol(ensemble)<8 & nrow(selection) = ncol(ensemble) then underfitting = underfitting +1 ; 
	else underfitting = underfitting ;

	/* Wrong*/ 
	*if ncol(ensemble)=1 then wrongmodel=wrongmodel +1 ; 
	end; 

print(overfitting); 
print(underfitting); 
print(perfectfitting); 
*print(wrongmodel); 



fX1 = X1freq / 1000;
fX2 = X2freq / 1000;
fX3 = X3freq / 1000;
fX4 = X4freq / 1000;
fX5 = X5freq / 1000;
fX6 = X6freq / 1000;
fX7 = X7freq / 1000;
fX8 = X8freq / 1000;
fX9 = X9freq / 1000;
fX10 = X10freq / 1000;
fX11 = X11freq / 1000;
fX12 = X12freq / 1000;
fX13 = X13freq / 1000;
fX14 = X14freq / 1000;
fX15 = X15freq / 1000;
fX16 = X16freq / 1000;
fX17 = X17freq / 1000;
fX18 = X18freq / 1000;
fX19 = X19freq / 1000;
fX20 = X20freq / 1000;
fX21 = X21freq / 1000;
fX22 = X22freq / 1000;
fX23 = X23freq / 1000;
fX24 = X24freq / 1000;
fX25 = X25freq / 1000;
fX26 = X26freq / 1000;
fX27 = X27freq / 1000;
fX28 = X28freq / 1000;
fX29 = X29freq / 1000;
fX30 = X30freq / 1000;
fX31 = X31freq / 1000;
fX32 = X32freq / 1000;
fX33 = X33freq / 1000;
fX34 = X34freq / 1000;
fX35 = X35freq / 1000;
fX36 = X36freq / 1000;
fX37 = X37freq / 1000;
fX38 = X38freq / 1000;
fX39 = X39freq / 1000;
fX40 = X40freq / 1000;
fX41 = X41freq / 1000;
fX42 = X42freq / 1000;
fX43 = X43freq / 1000;
fX44 = X44freq / 1000;
fX45 = X45freq / 1000;
fX46 = X46freq / 1000;
fX47 = X47freq / 1000;
fX48 = X48freq / 1000;
fX49 = X49freq / 1000;
fX50 = X50freq / 1000;




	
probaOver = (overfitting/1000); 
probaUnder = (underfitting/1000); 
probaPerfect = (perfectfitting/1000); 
probaWrong = 1 - (probaOver + probaUnder + probaPerfect); 


print(probaOver); 
print(probaunder); 
print(probaperfect); 
print(probawrong); 


fin = {"probaover", "probaunder", "probaperfect", "probawrong"}; 
fin_concat = probaover || probaunder || probaperfect || probawrong ; 

create Lasso_Table7 from fin_concat[colname=fin] ;
append from fin_concat; 
close Lasso_Table7; 


/* Créer un tableau concaténé des fréquences */
fr = {"fX1", "fX2", "fX3", "fX4", "fX5", "fX6", "fX7", "fX8", "fX9", "fX10",
      "fX11", "fX12", "fX13", "fX14", "fX15", "fX16", "fX17", "fX18", "fX19", "fX20",
      "fX21", "fX22", "fX23", "fX24", "fX25", "fX26", "fX27", "fX28", "fX29", "fX30",
      "fX31", "fX32", "fX33", "fX34", "fX35", "fX36", "fX37", "fX38", "fX39", "fX40",
      "fX41", "fX42", "fX43", "fX44", "fX45", "fX46", "fX47", "fX48", "fX49", "fX50"}; 

fr_concat = fX1 || fX2 || fX3 || fX4 || fX5 || fX6 || fX7 || fX8 || fX9 || fX10 ||
            fX11 || fX12 || fX13 || fX14 || fX15 || fX16 || fX17 || fX18 || fX19 || fX20 ||
            fX21 || fX22 || fX23 || fX24 || fX25 || fX26 || fX27 || fX28 || fX29 || fX30 ||
            fX31 || fX32 || fX33 || fX34 || fX35 || fX36 || fX37 || fX38 || fX39 || fX40 ||
            fX41 || fX42 || fX43 || fX44 || fX45 || fX46 || fX47 || fX48 || fX49 || fX50;

create Lasso_freq1 from fr_concat[colname=fr]; 
append from fr_concat; 
close Lasso_freq1;

/* Transposer les données */
proc transpose data=Lasso_freq1 out=Lasso_freq2(rename=(_NAME_=Variable COL1=Frequence));
run;

data Lasso_freq2;
    set Lasso_freq2;
    /* Créer une colonne avec une séquence de 1 à  50 */
    ID = _N_; /* _N_ génère un compteur qui s'incrémente de 1 pour chaque ligne */
run;

/* Trier les données par Variable_Num */
proc sort data=Lasso_freq2;
    by ID;
run;

proc sgplot data=Lasso_freq2;
    vbar Variable / response=Frequence datalabel;
    xaxis label="Selected variables" discreteorder=data;
    yaxis label="Frequency";
    title "Frequency of variable selection (Choose = AICc and stop = SBC)";
run;







/**********************************************GRAPHIQUE**************************************************/

 

data nouveau;
    set Lasso_Table1;
    Source = "Lasso_Table1";
    output;
 
    set Lasso_Table2;
    Source = "Lasso_Table2";
    output;
 
    set Lasso_Table3;
    Source = "Lasso_Table3";
    output;
 
    set Lasso_Table4;
    Source = "Lasso_Table4";
    output;
 
    set Lasso_Table5;
    Source = "Lasso_Table5";
    output;
 
    set Lasso_Table6;
    Source = "Lasso_Table6";
    output;
 
    set Lasso_Table7;
    Source = "Lasso_Table7";
    output;
 
    *set Lasso_Table8;
    *Source = "Lasso_Table8";
    *output;
run;
 

data nouveau;
    set nouveau;
    length Criterion $10;
    criterion_num = mod(_n_-1, 8) + 1; 
    
    select(criterion_num);
        when(1) Criterion = "SBC";
        when(2) Criterion = "AIC";
        when(3) Criterion = "BIC";
        when(4) Criterion = "CP";
        when(5) Criterion = "CV";
        when(6) Criterion = "ADJRSQ";
        when(7) Criterion = "AICC";
        *when(8) Criterion = "PRESS";
        otherwise Criterion = " ";
    end;
    drop criterion_num;
run;
 

data nouveau_long;
    set nouveau;
    length Metrics $12;
  
   
    Metrics = "ProbaOver";    Probability = ProbaOver;     output;
    Metrics = "ProbaUnder";   Probability = ProbaUnder;    output;
    Metrics = "ProbaPerfect"; Probability = ProbaPerfect;  output;
    Metrics = "ProbaWrong";   Probability = ProbaWrong;    output;
    
    keep Criterion Metrics Probability;
run;
 
 
proc template;
   define style mycolors;
      parent=styles.default;
      style GraphData1 / color=red; /* Overfitting */
      style GraphData2 / color=blue; /* Wrong Model */
      style GraphData3 / color=green; /* Underfitting */
      style GraphData4 / color=black; /* Perfect Fit */
   end;
run;
 
ods graphics / reset imagename="ModelFitProba" imagename="ProbabilityBarChart";
ods listing style=mycolors;
 
proc sgplot data=nouveau_long;
    vbar Criterion / response=Probability group=Metrics groupdisplay=cluster 
                    datalabel 
                    attrid=mycolors; 
    xaxis label="Selection Criterion" discreteorder=data;
    yaxis label="Probability";
    title "Intern Correlation Lasso (Stop = SBC)";
run;
 
ods listing style=default;
