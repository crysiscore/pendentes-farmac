
library(RPostgreSQL)
library(stringi)
library(dplyr) 

source("generic_functions.R")

#con_postgres_old <-  get.postgres.conection("pharm")
con_postgres_new <-   get.postgres.conection("central")


# busca  e une pacientes/dispensas da bd antiga (farmac_sync/iDART central server )
patients <- dbGetQuery(con_postgres_old, " select * from sync_temp_patients;")
dispenses <- dbGetQuery(con_postgres_old, " select * from sync_temp_dispense;")

patients_novo <- dbGetQuery(con_postgres_new, " select * from sync_temp_patients;")
dispenses_novo <- dbGetQuery(con_postgres_new, " select * from sync_temp_dispense;")

all_dispenses <- plyr::rbind.fill(dispenses,dispenses_novo)
all_patients  <- plyr::rbind.fill(patients,patients_novo)

# formatacao de datas
all_dispenses$dispensedate <- as.Date(as.POSIXct(all_dispenses$dispensedate, 'GMT'))
all_dispenses$dateexpectedstring <- as.Date(all_dispenses$dateexpectedstring, "%d %b %Y")
temp <- all_dispenses %>% select(patientid,patientfirstname,patientlastname, regimeid, dispensedate, dispensatrimestral, tipodt, dispensasemestral)
temp <- all_dispenses %>% select(patientid,patientfirstname,patientlastname,  dispensedate, dispensatrimestral, tipodt, dispensasemestral)
# misc vectores
vec_nids <- unique(all_dispenses$patientid)
vec_total_patients <- unique(all_patients$patientid)
us <-  unique(all_dispenses$mainclinicname)
total <- c(0,0,0,0,0,0,0,0,0)
dispensa_mensal <- c(0,0,0,0,0,0,0,0,0)
dispensa_trimestral  <- c(0,0,0,0,0,0,0,0,0)
dispensa_covid_19 <- c(0,0,0,0,0,0,0,0,0)


# empty temporary df
df_last_patient_dispense <-  all_dispenses[0,]
total_dispensa_mensal <- 0
total_dispensa_trimestral <- 0
total_dispensa_covid_19 <- 0

# Final df 
df_modelos_farmac <-  data.frame(us,total,dispensa_mensal,dispensa_trimestral,dispensa_covid_19)
df_modelos_farmac <-  filter(df_modelos_farmac, !is.na(us))
  
for ( i in 1:length(vec_nids )) {
  
  patient_id <- vec_nids[i]
  
  temp_dispenses <- all_dispenses %>%  filter(patientid==patient_id) %>%  arrange(dateexpectedstring)
  temp_vec_dateexpectedstring <- unique(temp_dispenses$dateexpectedstring)
  ultimo_date_expectedstring <- max(temp_vec_dateexpectedstring)
  last_dispense <- temp_dispenses %>%  filter(dateexpectedstring==ultimo_date_expectedstring) %>%  arrange(dispensedate)
  df_last_patient_dispense  <- plyr::rbind.fill(df_last_patient_dispense,last_dispense)
  
  ## 3 rows probably dt
  
  if(nrow(last_dispense)==1){
    
    diff <- as.numeric(difftime(last_dispense$dateexpectedstring, last_dispense$dispensedate, units = "days") )
    if(last_dispense$dispensatrimestral[1]==1){
    if(diff > 46){
      
      total_dispensa_trimestral <- total_dispensa_trimestral + 1
      count_dispense_us(last_dispense,"dispensa_trimestral")
    } else {
      if(diff <= 30){
        total_dispensa_mensal <- total_dispensa_mensal  +  1
        count_dispense_us(last_dispense,"dispensa_mensal")
      } else {
        total_dispensa_mensal <- total_dispensa_mensal  +  1
        count_dispense_us(last_dispense,"dispensa_mensal")
        }
    }
      } else {
      if(diff > 46){
        total_dispensa_covid_19 <- total_dispensa_covid_19 + 1
        count_dispense_us(last_dispense,"dispensa_covid")
      } else {
        total_dispensa_mensal <- total_dispensa_mensal  +  1
        count_dispense_us(last_dispense,"dispensa_mensal")
      }
   }
  }
  else if(nrow(last_dispense)==2){

    if(last_dispense$dispensedate[1]== last_dispense$dispensedate[2]){
      diff <- as.numeric(difftime(last_dispense$dateexpectedstring[1], last_dispense$dispensedate[1], units = "days") )
      
      if(last_dispense$dispensatrimestral[1]==1){
           if(diff <=30){
             
             total_dispensa_mensal <- total_dispensa_mensal +1
             count_dispense_us(last_dispense,"dispensa_mensal")
           }
        else { total_dispensa_trimestral <- total_dispensa_trimestral +1
        count_dispense_us(last_dispense,"dispensa_trimestral")
        }
        
       
      }
      else {
        diff <- as.numeric(difftime(last_dispense$dateexpectedstring[2], last_dispense$dispensedate[2], units = "days") )
        if(diff > 46){
          total_dispensa_covid_19 <- total_dispensa_covid_19 + 1
          count_dispense_us(last_dispense,"dispensa_covid")
        } else {
          total_dispensa_mensal <- total_dispensa_mensal  +  1
          count_dispense_us(last_dispense,"dispensa_mensal")
        }
        
      }
    }
    else {
      
      diff <- as.numeric(difftime(last_dispense$dateexpectedstring[2], last_dispense$dispensedate[2], units = "days") )
      if(diff > 46){
        total_dispensa_covid_19 <- total_dispensa_covid_19 + 1
        count_dispense_us(last_dispense,"dispensa_covid")
      } else {
        total_dispensa_mensal <- total_dispensa_mensal  +  1
        count_dispense_us(last_dispense,"dispensa_mensal")
      }
    }
    
    
  }
  else if(nrow(last_dispense)==3 ){

    if(last_dispense$dispensedate[1]== last_dispense$dispensedate[3]){

      diff <- as.numeric(difftime(last_dispense$dateexpectedstring[3], last_dispense$dispensedate[3], units = "days") )
      if(last_dispense$dispensatrimestral[3]==1){
        if(diff > 46){
          total_dispensa_trimestral <- total_dispensa_trimestral +1
          count_dispense_us(last_dispense,"dispensa_trimestral")
        } else {
          total_dispensa_mensal <- total_dispensa_mensal  +  1
          count_dispense_us(last_dispense,"dispensa_mensal")
        }
        
        
        
      }
      else {
        if(diff > 46){
          total_dispensa_covid_19<- total_dispensa_covid_19 +1
          count_dispense_us(last_dispense,"dispensa_covid")
          
        } else {
          total_dispensa_mensal <- total_dispensa_mensal  +  1
          count_dispense_us(last_dispense,"dispensa_mensal")
        }
        
      }
      
    }
    else {
      
      total_dispensa_trimestral <- total_dispensa_trimestral +1
      count_dispense_us(last_dispense,"dispensa_trimestral")
    }

    
  }  ##   mensal
  else {
    
    min_dispense_date <- min(last_dispense$dispensedate)
    diff <- as.numeric(difftime(ultimo_date_expectedstring, min_dispense_date, units = "days") )
    if(diff > 46  ){
      if(last_dispense$dispensatrimestral[1]==1){
          total_dispensa_trimestral <- total_dispensa_trimestral + 1
          count_dispense_us(last_dispense,"dispensa_trimestral")
        } else {
          total_dispensa_covid_19 <- total_dispensa_covid_19 + 1
          count_dispense_us(last_dispense,"dispensa_covid")
        }
      
    } else {
      
      total_dispensa_mensal <- total_dispensa_mensal  +  1     
      count_dispense_us(last_dispense,"dispensa_mensal")
    } 

}
}

print(paste0("total_dispensa_mensal: " ,total_dispensa_mensal))
print(paste0("total_dispensa_trimestral: ", total_dispensa_trimestral))
print(paste0("total_dispensa_covid_19: ", total_dispensa_covid_19))

df_modelos_farmac <- select(df_modelos_farmac,  c("us","dispensa_mensal", "dispensa_trimestral","dispensa_covid_19" ) )

df_modelos_farmac$total_pacientes <- 0
df_modelos_farmac$pacientes_sem_dispensa <- 0

vec_all_patiens <- table(all_patients$mainclinicname)
#patients
for (i in 1:length(vec_all_patiens)) {
   us <- dimnames(vec_all_patiens)[[1]][i]
   total <- vec_all_patiens[[i]]
   index <- which(df_modelos_farmac$us == us)
   df_modelos_farmac$total_pacientes[index] <- total
  
}
writexl::write_xlsx(x = df_modelos_farmac,path = "dd_analysis.xlsx")
table(all_patients$mainclinicname)
