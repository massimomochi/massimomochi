#
R_code_vegetation_indices.r

library(raster) 

setwd("C:/lab/")

defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

# B1=NIR, B2=red, B3=green

par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="lin")

# in questo modo facciamo un analisi multitemporale
# vediamo che siamo nella stessa zona ma nella prima immagine il fiume si presenta più acceso perché probabilmente aveva più sali disciolti che assorbe meno l'infrarosso
# se il fiume fosse nero significa che è acqua pura perché assorbe tutto l'infrarosso
# tutta la parte rossa è vegetazione di foresta pluviale, la parte chiara è suolo agricolo

# utilizziamo il DVI che è la differenza tra riflettanza dell'infrarosso vicino e la riflettanza del rosso
# il pixel di vegetazione sana ha il massimo di riflettanza nel NIR e il minimo di riflettanza nel RED perché viene assorbita (solitamente è vicino a 0)
# possiamo normalizzarlo generando NDVI e si fa NIR-RED/NIR+RED

# prima di tutto richiamiamo i pacchetti e settiamo la working directory
# carichiamo le immagini con "brick"
# successivamente plottiamo le immagini con il plotRGB

dvi1<- defor1$defor1.1 - defor1$defor1.2 #vediamo negli attributi i nomi delle bande NIR e RED e li leghiamo con il $ al nome dell'immagine, in questo modo stiamo facendo la differenza tra le due bande dell'immagine
plot(dvi1) # visualizziamo il prodotto grezzo con i colori di R
# le parti deforestate vengono visualizzate bene il rossastro e le parti vegetate in verde
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # cambiamo il colore all'immagine
plot(dvi1, col=cl, main="DVI at time 1") # tutto ciò che è rosso è vegetazione, aggiungiamo anche il titolo
# essendo al bordo nel lato superiore si genera un artefatto che non esiste realmente

dvi2<- defor2$defor2.1 - defor2$defor2.2 
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) 
plot(dvi2, col=cl, main="DVI at time 2") # la parte gialla è suolo nudo e il rosso è la vegetazione
# faremo un calcolo per ricavare la percentuale di foresta persa nel tempo

par(mfrow=c(2,1))
plot(dvi1, col=cl, main="DVI at time 1")
plot(dvi2, col=cl, main="DVI at time 2")
# le mettiamo in un'unica finetsra per confrontarle e successivamente facciamo il calcolo

difdvi<- dvi1-dvi2 # facciamo la differenza tra le due mappe e compare un messaggio ( Raster objects have different extents. Result for their intersection is returned)
# ci dice che l'estenzione delle due mappe non è la stessa, molto probabilmente ci sono alcuni pixel in piu in una rispetto all'altra

cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(difdvi,col=cld)
# dove ho valori di differenza più marcata la mappa è rossa, dove la differenza è bassa abbiamo il bianco e il celeste
# ci dice dove c'è stata della sofferenza vegetativa nell nostra area

#NDVI: normalizza il DVI, fa la standardizzazione del DVI sulla somma tra NIR e RED, in modo da ottenere numeri bassi e si possono confrontare immagini con risoluzione radiometrica differente
#il range dell'NDVI è [-1 , 1]

#NDVI
#(NIR-RED)/(NIR+RED)
ndvi1<-(defor1$defor1.1-defor1$defor1.2)/(defor1$defor1.1+defor1$defor1.2)
plot(ndvi1,col=cl) #vediamo che il range della legenda va da -1 a 1
# si può scrivere anche:
#ndvi1<-dvi1/(defor1$defor1.1+defor1$defor1.2)
# in quanto il numeratore era già associato ad una variabile
# in RStoolbox esistono tantissimi indici da poter calcolare con una funzione "spectralIndices", con un solo comando posso calcolare una moltitudine di indici

ndvi2<-dvi2/(defor2$defor2.1+defor2$defor2.2)
plot(ndvi2,col=cl) # anche in questo caso viene visualizzata molto bene la differenza di vegetazione

difndvi<-ndvi1-ndvi2
plot(difndvi,col=cld)

# RStoolbox: spectralIndices
library(RStoolbox) # dobbiamo richiamare questo pacchetto
si1<-spectralIndices(defor1,green=3,red=2,nir=1) #dobbiamo dichiarare le bande che abbiamo 
plot(si1, col=cl)
# ci sono i DVI e l'NDVI e altri molteplici indici
#NDWI lavora sull'acqua, quindi non solo sulla vegetazione

si2<-spectralIndices(defor2,green=3,red=2,nir=1) #dobbiamo dichiarare le bande che abbiamo 
plot(si2, col=cl) #facciamo la stessa cosa per la seconda immagine

#############################################################################################

#worldwide NDVI

#il pacchetto "rasterdiv" contiene un dataset di copernicus

install.packages("rasterdiv") # significa raster diversity
library(rasterdiv) # carichiamo il pacchetto rasterdiv
library(raster)
library(RStoolbox)
library(rasterVis)

#possiamo utilizzare il dataset "copNDVI" contenuto nel pacchetto

plot(copNDVI)
# vogliamo poi togliere tutta la parte che riguarda l'acqua tramite la funzione "cbinc" che cambia dei valori
# i pixel 253,254,255 (che riguardano l'acqua) possono essere trasformati in non valori NA
copNDVI<-raster::reclassify(copNDVI,cbind(252,255,NA),right=TRUE) #usiamo la funzione reclassify che si lega al pacchetto con i ::
# riclassifichiamo l'immagine originale e diciamo che i valori scritti (range) devono diventare NA
plot(copNDVI)

#facciamo il levelplot con il pacchetto rasterVis
levelplot(copNDVI) # si visualizzano i valori gradati per valore di NDVI nel mondo
#nella zona dell'equatore c'è il valore massimo di massa percé c'è massima luce e le piante che hanno molta sete di luce si accavallano l'una sull'altra per catturare quanta più luce possibile, atteggiamento tipico delle foreste tropicali
# a 23° Nord ci sono i deserti, tutti posti sulla stessa linea, perché li l'evapotraspirazione è elevatissima, si generano moti convettivi che fanno risalire aria umida e discendere aria secchissima
