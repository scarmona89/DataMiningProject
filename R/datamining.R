library(ggplot2)
library(MASS)
library(plotrix)
library(rworldmap)
library(raster)
library(sp)

readCSV<-function(input){

  # Download CSV from repository
  # fileUrl = "https://raw.githubusercontent.com/TheMaphius/DataMiningProject/master/resources/XXXXXX.csv"
  # if(!file.exists("./resources")){dir.create("./resources")}
  # download.file(fileUrl, destfile = "./resources/log_tuneado.csv", method = "curl")

  path = paste(getwd(),"/resources/log_tuneado.csv",sep="")
  csv<<-read.csv(file=path, header=FALSE, sep=";", col.names = c("c1","TimeStamp","c3","c4","c5","c6","c7","IP_origen","IP_Publica","c10","IP_interna","c12","c13","Protocolo","c15","Desde","Hacia","c18","c19","c20","c21","c22","c23","c24","Puerto_destino","c26","c27","c28","c29","c30","c31","32","Malicioso","Nivel_de_riesgo","35","c36","c37","Pais_origen","Pais_destino","c40","c41","c42","c43","c44","c45","c46","c47","c48","c49","Sender","Subject","Remitente","c53","c54","c55","c56","c57"), colClasses = c("character", "factor", "factor", "character" )) [,c("TimeStamp","IP_origen","IP_Publica","Protocolo","Desde","Hacia","Puerto_destino","Malicioso","Nivel_de_riesgo","Pais_origen","Pais_destino","Sender","Subject","Remitente")]
  numLines<<-length(csv$TimeStamp)
}


attackperHour<-function(){

  hours<-table((format(strptime(csv$TimeStamp, format='%d/%m/%Y %H:%M'),'%H')))
  percents<-(hours/numLines)*100
  print(sum(percents))
  plot(percents)

}

attackSourceCountry<-function(){

  countries <- table(csv$Pais_origen)

  dataframe <- data.frame(country=names(countries),
                          value=(as.vector(countries)/numLines)*100)

  country2Map <- joinCountryData2Map(dataframe,
                                     joinCode="ISO2",
                                     nameJoinColumn="country")

  mapCountryData(country2Map,
                 nameColumnToPlot="value",
                 mapTitle="Europe",
                 xlim=c(-10, 40), ylim=c(35, 70),
                 colourPalette="red2yellow",
                 addLegend=FALSE,
                 oceanCol="lightblue", missingCountryCol="black")

}
# attackSourceCountry<-function(){
#
#   originCountryAttack<-table(csv$Pais_origen)
#   percents<-(originCountryAttack/numLines)*100
#   print(percents)
#   plot(percents)
# }

attackDestinyCountry<-function(){

  originCountryAttack<-table(csv$Pais_destino)
  percents<-(originCountryAttack/numLines)*100
  print(percents)
  plot(percents)
}

attackforApplication<-function(){
  attack<-table(csv$Protocolo)
  percents<-(attack/numLines)*100
  print(percents)
  plot(percents)
}

levelofRisk<-function(){
  risk<-table(csv$Nivel_de_riesgo)
  percents<-(risk/numLines)*100
  print(percents)
  pie3D(percents, col=rainbow(length(percents)), main="Pie chart Level of Risk")
}

# getIps2Long()<-function(){
#   ipsList<-table(csv$IP_origen)
#   ## Working
# }


test<-function(){
  originCountryAttack<-(table(csv$Pais_origen))

  print(as.vector(originCountryAttack))
}
readCSV()



