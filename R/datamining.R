library(ggplot2)
library(MASS)
library(plotrix)
library(rworldmap)
library(raster)
library(sp)


readCSV<-function(input){

  if(!file.exists(paste( getwd(), "/resources", sep = ""))){
     dir.create("./resources")
  }

  path = paste(getwd(),"/resources/full_log_v2.csv",sep="")
  csv<<-read.csv(file=path, header=FALSE, sep=";", col.names = c("c1","TimeStamp","c3","c4","c5","c6","c7","IP_origen","IP_Publica","c10","IP_interna","c12","c13","Protocolo","c15","Desde","Hacia","c18","c19","c20","c21","c22","c23","c24","Puerto_destino","c26","c27","c28","c29","c30","c31","c32","Malicioso","Nivel_de_riesgo","35","c36","c37","Pais_origen","Pais_destino","c40","c41","c42","c43","c44","c45","c46","c47","c48","c49","Sender","Subject","Remitente","c53","c54","c55","c56","c57","c58","c59","c60","c61","c62"), colClasses = c("character", "factor", "factor", "character" )) [,c("TimeStamp","IP_origen","IP_Publica","Protocolo","Desde","Hacia","Puerto_destino","Malicioso","Nivel_de_riesgo","Pais_origen","Pais_destino","Sender","Subject","Remitente")]
  numLines<<-length(csv$TimeStamp)

}


attackperHour<-function(){

  hours<-table((format(strptime(csv$TimeStamp, format='%d/%m/%Y %H:%M'),'%H')))
  percents<-(hours/numLines)*100
  print(percents)
  # plot(percents)

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
                 addLegend=TRUE,
                 oceanCol="lightblue", missingCountryCol="black")

}

attackDestinyCountry<-function(){

  originCountryAttack<-table(csv$Pais_destino)
  percents<-(originCountryAttack/numLines)*100
  print(percents)
  plot(percents)
}

attackforApplication<-function(){
  attack<-table(csv$Protocolo)
  percents<-(attack/numLines)*100
  pie(percents[as.vector(percents) > 1], main="Ataques por aplicación", col=rainbow(length(percents[as.vector(percents) > 1])), labels = "")
  legend("bottomright", names(percents[as.vector(percents) > 1]), cex=0.5, fill=rainbow(length(percents[as.vector(percents) > 1])))
}

levelofRisk<-function(){
  risk<-table(csv$Nivel_de_riesgo)
  percents<-(risk/numLines)*100
  color<-c("red", "blue", "yellow")
  pie(percents, main="Riesgo de ataques", col=color, labels = "")
  legend("bottomright", names(percents), cex=0.5, fill=color)
}

correlateIps<-function(){

  if(!file.exists(paste(getwd(), "/resources/blacklist.csv", sep=""))){
    download.file("http://www.talosintel.com/feeds/ip-filter.blf", destfile = paste(getwd(), "/resources/blacklist.csv", sep=""))
  }

  path<-paste(getwd(),"/resources/blacklist.csv", sep = "")
  ips<<-read.csv(file=path, header = FALSE, col.names = "IP", colClasses = "character")

  fileredIps<-table(csv$IP_origen[csv$Malicioso=="benign"] %in% unique(ips$IP))
  percents<-(fileredIps/numLines)*100
  pie(percents, col=rainbow(length(percents)), main="IPs falsos positvos", labels = "")
  legend("bottomright", names(percents), cex=0.8, fill=rainbow(length(percents)))

  blacklist <- list()
  i = 1

  for(ip_malicious in unique(csv$IP_origen[csv$Malicioso=="benign"])){
    for(ip_blacklist in ips$IP){
      if(as.character(ip_malicious) ==  as.character(ip_blacklist)){
        blacklist$ip[i] <- ip_malicious
        i <- i + 1
        print(ip_malicious)
      }
    }
  }

  write.csv(x = blacklist, paste(getwd(), "/resources/blacklistFile.csv", sep=""), row.names = FALSE)


}

readCSV()
