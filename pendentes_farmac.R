##  **************** Configuracao de Parametros para conexao com servidor PostgreSQL
##  **  farmac (mail.ccsaude.org.mz) 
##  **  local (localhost)
library(RPostgreSQL)
library(stringi)
local.postgres.user ='postgres'                         # ******** modificar
local.postgres.password='postgres'                      # ******** modificar
local.postgres.db.name='pharm'                          # ******** modificar
local.postgres.host='localhost'                        # ******** modificar
local.postgres.port=5432                                # ******** modificar

#' getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL Local
#' 
#' @param postgres.user username do postgres
#' @param postgres.password passwd
#' @param postgres.db.name nome da db no postgres
#' @param postgres.host localhost
#' @return FALSE/Con -  (con) retorna um conexao valida  (FALSE) - erro de conexao   
#' @examples 
#' con_local<- getLocalServerCon()
#' 
getLocalServerCon <- function(){
  
  
  status <- tryCatch({
    
    
    # imprimme uma msg na consola
    message(paste0( "Postgres - conectando-se ao servidor Local : ",
                    local.postgres.host, ' - db:',local.postgres.db.name, "...") )
    
    
    
    # Objecto de connexao com a bd openmrs postgreSQL
    con_postgres <-  dbConnect(PostgreSQL(),user = local.postgres.user,
                               password = local.postgres.password, 
                               dbname = local.postgres.db.name,
                               host = local.postgres.host,
                               port = local.postgres.port )
    
    return(con_postgres)
    
  },
  error = function(cond) {
    
    ## Coisas a fazer se occorre um erro 
    
    # imprimir msg na consola
    
    message(paste0( "PosgreSQL - Nao foi possivel connectar-se ao host: ",
                    local.postgres.host, '  db:',local.postgres.db.name,
                    "...",'user:',local.postgres.user,
                    ' passwd: ', local.postgres.password)) 
    # guardar o log 
    # if(is.farmac){
    #   saveLogError(us.name = farmac_name,
    #                event.date = as.character(Sys.time()),
    #                action = 'getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL Local ',
    #                error = as.character(cond$message) )  
    #   
    # } else {
    #   
    #   saveLogError(us.name = main_clinic_name ,
    #                event.date = as.character(Sys.time()),
    #                action = 'getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL Local',
    #                error = as.character(cond$message) )  
    #}
    
    
    
    return(FALSE)
  },
  finally = {
    # NOTE:
    # Here goes everything that should be executed at the end,
    # Do nothing
  })
  
  status
}

con_farmac_sync  <-  getLocalServerCon()
con_local_albazine  <-  getLocalServerCon()


#pendentes_farmac_albazine <- read.csv('~/Downloads/data_farmac_magoanine.xlsx',stringsAsFactors = FALSE)
pendentes_farmac_albazine <- readxl::read_xlsx('~/Downloads/data_farmac_magoanine.xlsx',col_names = TRUE)
pendentes_farmac_albazine$seq <- substr(pendentes_farmac_albazine$NID,
                                        stri_locate_last(str = pendentes_farmac_albazine$NID,regex = '/') +1 ,
                                        nchar(pendentes_farmac_albazine$NID))
pendentes_farmac_albazine$obs <- ""
refferedPatients <- dbGetQuery(con_farmac_sync, " select * from sync_temp_patients;")
Patients <- dbGetQuery(con_local_albazine, " select * from patient;")

for (v in 1:dim(pendentes_farmac_albazine)) {
  
  nid <- pendentes_farmac_albazine$NID[v]
  
  index <- grepl(pattern = nid, x = refferedPatients$patientid,ignore.case = TRUE)
  
  if(length(index)==1){
    pendentes_farmac_albazine$obs[v] <- refferedPatients$patientid[v]
    
  }
  
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


k=4845 # madalena carlos
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '20 Aug 2014' ) ;")

k=23990 # Telma cabral
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '22 Dec 2014' ) ;")

k=25551 #Alberto Antonio
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '23 Nov 2017' ) ;")

k=23621 #Maria Marsalo
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '31 Jul 2017' ) ;")

k=25037 #Lucinda reginaldo
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '31 Jul 2013' ) ;")

k=22775 
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '09 Jan 2013' ) ;")



k=23932  #Xavier  Namburete	
paste0("(",Patients$id[k],',', TRUE,", '", Patients$cellphone[k],"' , '",  Patients$dateofbirth[k],"' ,",  Patients$clinic[k],", ","'Farmacia Magoanine' ,", 2," ,",
       "'CS ALBASINE' , '",Patients$firstnames[k],"' , '' , '",Patients$lastname[k],"' , 'T', '" , Patients$patientid[k],"' , '", Patients$province[k],  "' , '",
       Patients$sex[k],"' , '' , '', '', '', '','',","'Matched' , '",Patients$uuid[k], "' , '29 Oct 2013' ) ;")