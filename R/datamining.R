# FUNCTIONS

read_logs<-function(input){
  
  logs<-read.csv(file=input, nrows=20, header=FALSE, sep=";", col.names = c("c1","TimeStamp","c3","c4","c5","c6","c7","IP_origen","IP_Publica","c10","IP_interna","c12","c13","Protocolo","c15","Desde","Hacia","c18","c19","c20","c21","c22","c23","c24","Puerto_destino","c26","c27","c28","c29","c30","c31","32","Malicioso","Nivel_de_riesgo","35","c36","c37","Pais_origen","Pais_destino","c40","c41","c42","c43","c44","c45","c46","c47","c48","c49","Sender","Subject","Remitente","c53","c54","c55","c56","c57"), colClasses = c("character", "factor", "factor", "character" )) [,c("TimeStamp","IP_origen","IP_Publica","Protocolo","Desde","Hacia","Puerto_destino","Malicioso","Nivel_de_riesgo","Pais_origen","Pais_destino","Sender","Subject","Remitente")]
  print(logs)
  
  #print ("---------RESULTS-----------")
  #for (i in 1:10){
    #print (logs[[i,2]])
  #}
  
}
