#Time series analysis
#Greenland increase of temperature
#Data and code from Emanuela Cosma 

setwd("C:/lab/") # Windows

# install.packages("raster") vanno sempre messi prima della setwd
library(raster)

#oggi utilizzaremo dati sulla tempertura e dati sullo strato della groenlandia 

#Primo carichiamo pacchetto raster 
#choose Directory

#STACK -> la funzione stack è usata per trasformare dati disponibili come colonne separate in un data frame o lista avente una singola colonna 
#che può essere utilizzato nello studio dei modelli di varianza o altri modelli lineari

#importiamo immagini funzione per singoli dati , no pacchetto : raster 
#all'interno di pacchetto raster esiste funzione "raster" per creare un'oggetto raster layer
#ciclo movimento iterativo di funzioni -> in informatica 

lst_2000 <- raster("lst_2000.tif")

plot(lst_2000) #plottiamo semplicemente l'immagine importata 

lst_2005 <- raster("lst_2005.tif")

plot(lst_2005)

#utilizziamo per ridurre il peso delle immagini , valori interi e non digitali DN digital number 
#per la temperatura "bit" 


lst_2010 <- raster("lst_2010.tif")

lst_2015 <- raster("lst_2015.tif")

par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

#lapply , è una funzione che applico ad  un'altra  certa funzione (raster)a una lista di file , tutti assieme .

#funzione list.files crea la lista di file a cui R poi applicherà Lapply 

#LIST OF FILE 
#pattern spiega al software - cerca i file tramite nome 

list.file

rlist <- list.files(pattern="lst")
rlist
[1] "lst_2000.tif" "lst_2005.tif" "lst_2010.tif"
[4] "lst_2015.tif"

lapply(rlist,raster) #applichiamo la funzione alla lista scelta e utilizzando la funzione raster 

import <- lapply(rlist,raster) #file uniti tutti assieme con un unico nome - attenzione maiuscole 

#stack blocco di file tutti assieme  - fa un unico file grande e univoco 
#mi servirà per fare plot 

TGr <- stack(import)
plot(TGr)

#potrei usare lapply, invece di usare raster metto plot 

#creiao un file composto dalle T nei vari anni su un unica immagine

plotRGB(TGr, 1, 2, 3, stretch="Lin")


plotRGB(TGr, 1, 2, 3, stretch="Lin")

#coloristRpackage -> ne parlerà

#VENERDI' 9 

library(rasterVis) #Methods for enhanced aumentare la visualizzaione e interazione con i dati raster
library(raster)

levelplot(TGr) #funzione levelpolt visione diversi livelli plot *****

levelplot(TGr$list_2000) #il grafico , mostra l'andatura media per colonna e lo stesso a dx per riga 
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100) #immagine singole e non un immagine satellitare su vari livelli RGB

levelplot(TGr, col.regions=cl)

levelplot(TGr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

levelplot(TGr,col.regions=cl, main="LST variation in time",names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

# Melt

setwd("~/Desktop/lab")

meltlist <- list.files(pattern="melt") 

melt_import <- lapply(meltlist,raster)

melt <- stack(melt_import)

melt

levelplot(melt) #grafico scioglimento ghiacciai 1979- 2007

#analisi multitemporale 

melt_amount <- melt$X2007annual_melt - melt$X1979annual_melt #sottrazione dati per vedere differenza 

clb <- colorRampPalette(c("blue","white","red"))(100)
plot(melt_amount, col=clb)

levelplot(melt_amount, col.regions=clb)

#14 aprile 2021

#installo pacchetto "knitr"

install.packages("knitr")  
