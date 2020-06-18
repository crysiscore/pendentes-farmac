
library(RPostgreSQL)
library(stringi)
library(stringdist)
library(dplyr)

setwd('~/Git/pendentes_farmac/')
source('sql_querys.R')

## con farmac_sync
local.postgres.user ='farmac'                         # ******** modificar
local.postgres.password='iD@rt2020'                   # ******** modificar
local.postgres.db.name='pharm'                        # ******** modificar
local.postgres.host='197.249.6.156'                   # ******** modificar
local.postgres.port=5455                              # ******** modificar
# ******** modificar
con_farmac_sync  <-  getLocalServerCon()

## con us
local.postgres.user ='postgres'                         # ******** modificar
local.postgres.password='postgres'                      # ******** modificar
local.postgres.db.name='pharm'                          # ******** modificar
local.postgres.host='192.168.0.117'                        # ******** modificar
local.postgres.port=5432    

# ******** modificar
con_local_us  <-  getLocalServerCon()

rs_name <- dbGetQuery(con_local_us, " select clinicname from clinic where id =2;")
us_name <- rs_name$clinicname

#refferedPatients <- dbGetQuery(con_farmac_sync, " select * from sync_temp_patients;")
missing_patients <- dbGetQuery(con_local_us,sql_patients_referidos_not_in_sync)
missing_patients_id <- missing_patients[,c('id','value','clinic','clinicname')]
patients_us <- dbGetQuery(con_local_us, " select * from patient ;")


data_missing_patients <- inner_join(missing_patients_id , patients_us ,by = c("id"), keep=FALSE ) %>%
                         select(id,cellphone,dateofbirth,clinic.x,clinicname,firstnames,lastname,patientid,province,sex,uuid,value)
data_missing_patients$usname <-us_name
names(data_missing_patients)[which(names(data_missing_patients)=="clinic.x")] <- "clinic"
data_missing_patients <- unique(data_missing_patients)

#pendentes_farmac_albazine <- read.csv('~/Downloads/data_farmac_magoanine.xlsx',stringsAsFactors = FALSE)
pendentes_farmac_albazine <- readxl::read_xlsx('~/Downloads/data_farmac_magoanine.xlsx',col_names = TRUE)

pendentes_farmac_albazine$seq <- substr(pendentes_farmac_albazine$NID,
                                        stri_locate_last(str = pendentes_farmac_albazine$NID,regex = '/') +1 ,
                                        nchar(pendentes_farmac_albazine$NID))
pendentes_farmac_albazine$obs <- ""
pendentes_farmac_albazine$nid_albazine <- ""
pendentes_farmac_albazine$nid_farmac <- ""
pendentes_farmac_albazine$nome_albazine <- ""
pendentes_farmac_albazine$nome_farmac <- ""
pendentes_farmac_albazine$situacao <- ""

for (v in 1:nrow(pendentes_farmac_albazine)) {
  seq_1 <- pendentes_farmac_albazine$NID[v]
  
  
    index <- which(grepl(pattern = seq_1,x = refferedPatients$patientid)==TRUE)
    if(length(index)==1){
    if(getStringDistance(string1 =pendentes_farmac_albazine$Nome[v], string2 = refferedPatients$firstnames[index[1]] ) < 0.7 ){
      pendentes_farmac_albazine$obs[v] <- "found"
      pendentes_farmac_albazine$nid_farmac[v] <- refferedPatients$patientid[index[1]]
      pendentes_farmac_albazine$nome_farmac[v] <- gsub(pattern = "NA",replacement = ' ',
                                                       x = paste0(refferedPatients$firstnames[index[1]], " ", refferedPatients$lastname[index[1]]))
      pendentes_farmac_albazine$situacao[v] <- "existe na farmac"
}
  } else if(length(index)==2){
    
    pendentes_farmac_albazine$obs[v] <- "found_2x"
    pendentes_farmac_albazine$nid_farmac[v] <- paste0( refferedPatients$patientid[index[1]], ' - ', refferedPatients$patientid[index[2]])
    pendentes_farmac_albazine$situacao[v] <- "existe na farmac duplicado"
    
  } else {
    
    index <- which(grepl(pattern = seq_1,x = missing_patients$patientid)==TRUE)
    
    if(length(index)==1){
      
      pendentes_farmac_albazine$obs[v] <- "found_albazine"
      pendentes_farmac_albazine$nid_albazine[v] <- missing_patients$patientid[index[1]]
      pendentes_farmac_albazine$nome_albazine[v] <- gsub(pattern = "NA",replacement = ' ',
                                                       x = paste0(missing_patients$firstnames[index[1]], " ", missing_patients$lastname[index[1]]))
      
    } else if(length(index)==2){
      
      pendentes_farmac_albazine$obs[v] <- "found_2x_albazine"
      if(missing_patients$firstnames[index[1]]==missing_patients$firstnames[index[2]]) { # same patient
        if(missing_patients$clinic[index[1]]!=missing_patients$clinic[index[2]]){
          pendentes_farmac_albazine$nid_albazine[v] <- missing_patients$patientid[index[1]]
          pendentes_farmac_albazine$nome_albazine[v] <- gsub(pattern = "NA",replacement = ' ',
                                                             x = paste0(missing_patients$firstnames[index[1]], " ", missing_patients$lastname[index[1]]))
          
        }
      } else {
        
        pendentes_farmac_albazine$obs[v] <- "duplicado_pat"
      }
      # pendentes_farmac_albazine$nid_referido[v] <- paste0( refferedPatients$patientid[index[1]], ' - ', refferedPatients$patientid[index[2]])
      
    } else {}
    # seq <- pendentes_farmac_albazine$seq[v]
    # index <- which(grepl(pattern = seq,x = refferedPatients$patientid)==TRUE)
    # if(length(index)==1){
    #   
    #   pendentes_farmac_albazine$obs[v] <- "found"
    #   pendentes_farmac_albazine$nid_referido[v] <- refferedPatients$patientid[index[1]]
    #   
    # }else if(length(index)==2){
    #   
    #   pendentes_farmac_albazine$obs[v] <- "found_2x"
    #   pendentes_farmac_albazine$nid_referido[v] <- paste0( refferedPatients$patientid[index[1]], '-', refferedPatients$patientid[index[2]])
    #   
    # }
  }
  
}


# Insere na tabela sync_temp_patients os nids em falta
for( v in 1:nrow(data_missing_patients)){

  if(is.na( data_missing_patients$value[v])){
    
    if(is.na( data_missing_patients$dateofbirth[v])){
      base_query <- paste0("(",data_missing_patients$id[v],',', TRUE,", '", data_missing_patients$cellphone[v],"' , '", "1988-01-01" , "' ,",  data_missing_patients$clinic[v],", ","'",data_missing_patients$clinicname[v],"' ,", 2," , '",
                           data_missing_patients$usname[v],"' , '",data_missing_patients$firstnames[v],"' , '' , '",data_missing_patients$lastname[v],"' , 'T', '" , data_missing_patients$patientid[v],"' , '", data_missing_patients$province[v],  "' , '",
                           data_missing_patients$sex[v],"' , '' , '', '', '', '','',","'Matched' , '",data_missing_patients$uuid[v], "' , '", "' ) ,")
      
      
    } else {
    base_query <- paste0("(",data_missing_patients$id[v],',', TRUE,", '", data_missing_patients$cellphone[v],"' , '",  data_missing_patients$dateofbirth[v],"' ,",  data_missing_patients$clinic[v],", ","'",data_missing_patients$clinicname[v],"' ,", 2," , '",
                         data_missing_patients$usname[v],"' , '",data_missing_patients$firstnames[v],"' , '' , '",data_missing_patients$lastname[v],"' , 'T', '" , data_missing_patients$patientid[v],"' , '", data_missing_patients$province[v],  "' , '",
                         data_missing_patients$sex[v],"' , '' , '', '', '', '','',","'Matched' , '",data_missing_patients$uuid[v], "' , '", "' ) ,")
    }
    
  } 
  else {
    if(is.na( data_missing_patients$dateofbirth[v])){
      
      base_query <- paste0("(",data_missing_patients$id[v],',', TRUE,", '", data_missing_patients$cellphone[v],"' , '","1988-01-01" ,"' ,",  data_missing_patients$clinic[v],", ","'",data_missing_patients$clinicname[v],"' ,", 2," , '",
                           data_missing_patients$usname[v],"' , '",data_missing_patients$firstnames[v],"' , '' , '",data_missing_patients$lastname[v],"' , 'T', '" , data_missing_patients$patientid[v],"' , '", data_missing_patients$province[v],  "' , '",
                           data_missing_patients$sex[v],"' , '' , '', '', '', '','',","'Matched' , '",data_missing_patients$uuid[v], "' , '",data_missing_patients$value[v], "' ) ,")
      
      
    }
    else {
      
      base_query <- paste0("(",data_missing_patients$id[v],',', TRUE,", '", data_missing_patients$cellphone[v],"' , '",  data_missing_patients$dateofbirth[v],"' ,",  data_missing_patients$clinic[v],", ","'",data_missing_patients$clinicname[v],"' ,", 2," , '",
                           data_missing_patients$usname[v],"' , '",data_missing_patients$firstnames[v],"' , '' , '",data_missing_patients$lastname[v],"' , 'T', '" , data_missing_patients$patientid[v],"' , '", data_missing_patients$province[v],  "' , '",
                           data_missing_patients$sex[v],"' , '' , '', '', '', '','',","'Matched' , '",data_missing_patients$uuid[v], "' , '",data_missing_patients$value[v], "' ) ,")
      
    }

  }


  #print(base_query)
  write(base_query,file="sql_farmac_missing_patients.txt", append  = TRUE)
  #write(base_query,file="update_querys.txt",  append  = TRUE)
}

# INSERT INTO public.sync_temp_patients(
#   id, accountstatus, cellphone, dateofbirth, clinic, clinicname, 
#   mainclinic, mainclinicname, firstnames, homephone, lastname, 
#   modified, patientid, province, sex, workphone, address1, address2, 
#   address3, nextofkinname, nextofkinphone, race, uuid, datainiciotarv)
# values 
# (487304,TRUE, '826898735' , '1975-01-01' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'madelana ' , '' , 'carlos' , 'T', '0111041101/2014/01439' , 'MAPUTO' , 'F' , '' , '', '', '', '','','Matched' , 'EC9708DB-D5EF-42A9-B91A-A518E0873236' , '20 Aug 2014' ) ,
# (494480,TRUE, '' , '1995-01-01' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'TELMA CABRAL' , '' , 'DIMANDE' , 'T', '0111041101/2012/01533' , 'Maputo' , 'F' , '' , '', '', '', '','','Matched' , 'F97EEA63-B1CB-4AE7-9D61-636EF9A655A0' , '22 Dec 2014' ) ,
# (541172,TRUE, '' , '1997-01-01' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'Alberto Antonio' , '' , 'Marrengue' , 'T', '0111041101/2017/01287' , 'MAPUTO' , 'M' , '' , '', '', '', '','','Matched' , 'faa55e97-1058-4c27-81ee-9a3e0fc62fe4' , '23 Nov 2017' ) ,
# (490670,TRUE, '' , '1983-01-01' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'maria ' , '' , 'marsalo' , 'T', '0111041101/2013/01190' , 'Maputo' , 'F' , '' , '', '', '', '','','Matched' , 'B9BA82CA-3C00-4459-8983-D2F3BE2012DC' , '31 Jul 2017' ) ,
# (504113,TRUE, '840678013' , '1992-01-01' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'Lucinda Reginaldo' , '' , 'Marcos' , 'T', '0111041101/2017/00046' , 'Maputo' , 'F' , '' , '', '', '', '','','Matched' , 'c82dcddc-8675-49bc-b8bb-4713f4435424' , '31 Jul 2013' ) ,
# (482165,TRUE, '' , '1978-06-20' ,1241478, 'Farmacia Magoanine' ,2 ,'CS ALBASINE' , 'mari0 ' , '' , 'armamdo' , 'T', '01110401101/2012/02033' , 'MAPUTO' , 'M' , '' , '', '', '', '','','Matched' , '5B8A7895-C19F-4770-9C10-60D10EB2148B' , '09 Jan 2013' ) ;

