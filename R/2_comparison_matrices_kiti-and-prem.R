library(data.table)

##########################################################################
#' participants is a data.table with the following columns
#' part_id for participant id (integer)
#' part_age with the estimated age of the participant (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#' 
#setwd("C:/Users/matt_/Google Drive/Research/COVID/Pop council")
participants_import_kiti<-read.csv("Data/kiti_data_wide_csv.csv")
#participants_S1<-data.frame(participants_import)
participants_kiti<-data.table(participants_import_kiti)

participants_kiti$part_age<-participants_kiti$age_class_part_2

mean(participants_kiti$part_id)
mean(participants_kiti$part_age)
participants_kiti$weekday="Tuesday"


#participants<-readRDS("participants_50plus.rds")
##########################################################################
#' contacts is a data.table with the following columns
#' part_id for participant id (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#' age_est with the estimated age of the contact
##' 

contacts_import_kiti<- read.csv("Data/kiti_data_long.csv")
#contacts_S1<-data.frame(contacts_import)
contacts_kiti<-data.table(contacts_import_kiti)
#
#

contacts_kiti$cnt_age_exact<-contacts_kiti$cnt_age

contacts_kiti$age_est=contacts_kiti$cnt_age
contacts_kiti$weekday="Tuesday"
mean(contacts_kiti$age_est)

#contacts_S1<-readRDS("contacts_50plus.rds")



##########################################################################
#' population_data is a data.table with population estimates
#' it is programmed to be a data.table with one row for each agegroup, and the following columns:
#' age (integer)
#' total (integer with total number of people)
population_data_import_kenya<- read.csv("Data/population_data_kenya_wpp.csv")
population_data_import_kilifi<- read.csv("Data/kilifi_age_dist.csv")
population_data_import_informal<- read.csv("Data/inform_age_dist.csv")

population_data_kenya<-data.table(population_data_import_kenya)
population_data_kilifi<-data.table(population_data_import_kilifi)
population_data_informal<-data.table(population_data_import_informal)

#population_data<-readRDS("popdata.RDS")
#popdata<-population_data_kilifi

##########################################################################
#' age_groups is a data.table with the age groups you want in your matrix
#' for you, this will be the following if your 2nd agegroup is 18-59 
age_groups <- data.table(
  age_low = c(0, 19, 50),
  age_high = c(18, 49, 80)
)
age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])

#' calculate contact matrix
#'
#' @param contacts data.table with contacts datasets
#' @param participants data.table with participants datasets
#' @param population_data data.table with population data when survey was done
#' @param age_groups data.table with age groups to use in model
#' @param weight_dayofweek logical to weight day of week
#' @param use_reciprocal_for_missing logical whether to use reciprocal to fill in missing cells
#' @param symmetric_matrix logical whether to make matrix symmetric (accounting for population and sampling distribution)
#'
#' @return returns contact matrix
#' @export
#'
#' @examples
calculate_matrix <- function(
  contacts, participants, population_data, age_groups,
  weight_dayofweek = TRUE, use_reciprocal_for_missing = FALSE, symmetric_matrix = TRUE
){
  if(nrow(contacts) == 0){
    warning("contacts dataset is empty")
    return(NULL)
  }
  
  if(nrow(participants) == 0){
    warning("participants dataset is empty")
    return(NULL)
  }
  
  #weights for weekdays, needs to be adapted for countries where Friday and Saturday is the weekend
  weekday_weights <- data.table(
    day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
    weight = c(5, 5, 5, 5, 5, 2, 2)
  )
  
  #assign age-groups to participants and contacts
  for(i in 1:nrow(age_groups)){
    contacts[age_est >= age_groups[i, age_low] & age_est <= age_groups[i, age_high], "contact_age_group"] <- age_groups[i, name]
    participants[part_age >= age_groups[i, age_low] & part_age <= age_groups[i, age_high], "participant_age_group"] <- age_groups[i, name]
  }
  participants[, "participant_age_group"] <- factor(participants[, participant_age_group], age_groups$name)
  contacts <- merge(contacts, participants[, c("part_id","participant_age_group")], by = "part_id", allow.cartesian = TRUE)
  contacts[, "contact_age_group"] <- factor(contacts[, contact_age_group], age_groups$name)
  
  if(nrow(contacts) == 0){
    warning("contacts dataset is empty")
    return(NULL)
  }
  
  #get total number of contacts between participant- and contact-agegroups
  total_contacts <- lapply(
    unique(weekday_weights[, weight]),
    function(w, weekday_weights){
      data <- contacts[weekday %in% weekday_weights[weight==w, day]]
      if(nrow(data) > 0){
        total_contacts <- dcast(
          data,
          contact_age_group~participant_age_group, fun.aggregate=length,
          drop=FALSE
        )
        if(!weight_dayofweek){w <- 1}
        total_contacts_matrix <- as.matrix(total_contacts[, -"contact_age_group"])*w
        rownames(total_contacts_matrix) <- total_contacts[, contact_age_group]
        return(total_contacts_matrix) 
      }
    },weekday_weights
  )
  #combine matrices (for weekend-days and non-weekend-days)
  total_contacts <- Reduce(
    '+',
    total_contacts[!sapply(total_contacts, is.null)]
  )
  
  #get total number of study participants in age group j
  n <- rowSums(sapply(
    unique(weekday_weights[, weight]),
    function(w, weekday_weights){
      data <- participants[weekday %in% weekday_weights[weight==w, day]]
      if(!weight_dayofweek){w <- 1}
      if(nrow(data) > 0){
        as.numeric(dcast(data, .~participant_age_group, fun.aggregate=length, drop=FALSE)[,-"."])*w  
      } else {
        as.numeric(dcast(participants, .~participant_age_group, fun.aggregate=length, drop=FALSE)[,-"."])*0
      }
    },weekday_weights
  ))
  
  #average number of daily contacts between participants in age group j and contacts in age group i
  raw_contact_matrix <- t(t(total_contacts)/n)
  raw_contact_matrix[which(is.na(raw_contact_matrix))] <- NA
  
  #imputation 1
  #use reciprocity of contacts old-young to impute contacts young-old
  imputed_contact_matrix <- raw_contact_matrix
  if(use_reciprocal_for_missing){
    #warning("using reciprocity of contacts to impute missing values")
    imputed_contact_matrix[which(is.na(imputed_contact_matrix))] <- t(imputed_contact_matrix)[which(is.na(imputed_contact_matrix))] 
  }
  
  #adjust for age-population, which may be different in our sample
  if(symmetric_matrix){
    popmat <- sapply(
      1:nrow(age_groups),
      function(i, age_groups, population_data){
        sum(population_data[age >= age_groups[i, age_low] & age <= age_groups[i, age_high], total])
      }, age_groups, population_data
    )
    popmat <- sapply(popmat, function(p){p/popmat})
    population_contact_matrix <- 0.5 * (imputed_contact_matrix + t(imputed_contact_matrix) * t(popmat))
    
    return(population_contact_matrix)
  } else {
    return(imputed_contact_matrix)
  } 
}


manual_matrix_kiti<-calculate_matrix(contacts = contacts_kiti, participants = participants_kiti,population_data=population_data_kilifi, weight_dayofweek = FALSE, 
                    symmetric_matrix = TRUE,age_groups = age_groups,use_reciprocal_for_missing=TRUE)
manual_matrix_kiti

#Making prem matrices
#'prem_matrix_old_whole<- matrix(c(19.06,  5.65,   0.72, #col 1
#'                                 5.16,   9.89,   1.16,    #col 2
#'                                 2.94,   3.7 ,   1.12)    #col 3
#'                               ,nrow=3,ncol=3)

prem_temp<-manual_matrix_kiti
prem_temp[1,1]<-19.06
prem_temp[1,2]<-5.65
prem_temp[1,3]<-0.72
prem_temp[2,1]<-5.16
prem_temp[2,2]<-9.89
prem_temp[2,3]<-1.16
prem_temp[3,1]<-2.94
prem_temp[3,2]<-3.7
prem_temp[3,3]<-1.12
prem_matrix_old<-prem_temp

#new prem matrix
#'prem_matrix_whole<- matrix(c(17.89,  5.94,   0.75, #col 1
#'                             4.48,   11.39,   1.84,    #col 2
#'                           2.24,   5.69 ,   2.31)    #col 3
#'                           ,nrow=3,ncol=3)

prem_temp<-manual_matrix_kiti
prem_temp[1,1]<-17.89
prem_temp[1,2]<-5.94
prem_temp[1,3]<-0.75
prem_temp[2,1]<-4.48
prem_temp[2,2]<-11.39
prem_temp[2,3]<-1.84
prem_temp[3,1]<-2.24
prem_temp[3,2]<-5.69
prem_temp[3,3]<-2.31
prem_matrix_new<-prem_temp

prem_matrix_new<-t(prem_matrix_new)
prem_matrix_old<-t(prem_matrix_old)
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'              Adjusting for age dists between settings
#'
#'
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setting up age dists
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Kilifi
N_kilifi <- sapply(
  1:nrow(age_groups),
  function(i, age_groups, population_data_kilifi){
    sum(population_data_kilifi[age >= age_groups[i, age_low] & age <= age_groups[i, age_high], total])
  }, age_groups, population_data_kilifi
)

#informal
N_informal <- sapply(
  1:nrow(age_groups),
  function(i, age_groups, population_data_informal){
    sum(population_data_informal[age >= age_groups[i, age_low] & age <= age_groups[i, age_high], total])
  }, age_groups, population_data_informal
)

#kenya
N_kenya <- sapply(
  1:nrow(age_groups),
  function(i, age_groups, population_data_kenya){
    sum(population_data_kenya[age >= age_groups[i, age_low] & age <= age_groups[i, age_high], total])
  }, age_groups, population_data_kenya
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#adjustment code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' original is the original contact matrix that will be applied to the new population, with participants as columns and contacts as rows (matrix)
#' N_original is the total population size in each age-group in the contact matrix, for the population from where the initial study was done (numeric vector)
#' N_new is the total population size in each age-group in the contact matrix, for the new population to which the matrix will be applied (numeric vector)
#' adjusted_matrix will be the matrix applied to the new population



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~settings for adjustment
#Kilifi to informal, kiti
N_original<-N_kilifi
N_new<-N_informal
original_matrix<-manual_matrix_kiti
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end settings
adjusted_matrix = original_matrix / N_original * N_new
for(i in 1:nrow(adjusted_matrix)){
  for(j in 1:ncol(adjusted_matrix)){
    adjusted_matrix[i,j] = (adjusted_matrix[i,j] + adjusted_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
adjusted_matrix_kiti<-adjusted_matrix
adjusted_matrix_kiti

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~settings for adjustment
#Kenya to informal, prem old
N_original<-N_kenya
N_new<-N_informal
original_matrix<-prem_matrix_old
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end settings
adjusted_matrix = original_matrix / N_original * N_new
for(i in 1:nrow(adjusted_matrix)){
  for(j in 1:ncol(adjusted_matrix)){
    adjusted_matrix[i,j] = (adjusted_matrix[i,j] + adjusted_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
adjusted_prem_matrix_new<-adjusted_matrix
adjusted_prem_matrix_new

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~settings for adjustment
#Kenya to informal, prem new
N_original<-N_kenya
N_new<-N_informal
original_matrix<-prem_matrix_new
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end settings
adjusted_matrix = original_matrix / N_original * N_new
for(i in 1:nrow(adjusted_matrix)){
  for(j in 1:ncol(adjusted_matrix)){
    adjusted_matrix[i,j] = (adjusted_matrix[i,j] + adjusted_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
adjusted_prem_matrix_old<-adjusted_matrix
adjusted_prem_matrix_old

manual_matrix_kiti
adjusted_matrix_kiti
prem_matrix_old
adjusted_prem_matrix_old
prem_matrix_new
adjusted_prem_matrix_new

#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'          
#'
#'
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'

mean(manual_matrix_kiti)
mean(adjusted_matrix_kiti)
mean(prem_matrix_old)
mean(adjusted_prem_matrix_old)
mean(prem_matrix_new)
mean(adjusted_prem_matrix_new)



