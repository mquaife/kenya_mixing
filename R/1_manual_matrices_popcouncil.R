library(data.table)

#Set working directory to wherever github lives
setwd("C:/Users/matt_/Google Drive/Research/github/kenya_mixing")


##########################################################################
#' participants is a data.table with the following columns
#' part_id for participant id (integer)
#' part_age with the estimated age of the participant (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#' 

participants_import<- read.csv("Data/mixing_module_dataset_may1420_wide.csv")
participants_S1<-data.table(participants_import)


mean(participants_S1$part_id)
mean(participants_S1$part_age)
participants_S1$weekday="Tuesday"
##########################################################################
#' contacts is a data.table with the following columns
#' part_id for participant id (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#' age_est with the estimated age of the contact
##' 
contacts_import<- read.csv("Data/imputed_justcategory_60plus.csv")
contacts_S1<-data.table(contacts_import)
#
#
contacts_S1$age_est=contacts_S1$cnt_age
contacts_S1$weekday="Tuesday"
##########################################################################
#' population_data is a data.table with population estimates
#' it is programmed to be a data.table with one row for each agegroup, and the following columns:
#' age (integer)
#' total (integer with total number of people)
population_data_import<- read.csv("Data/inform_age_dist.csv")
population_data<-data.table(population_data_import)

population_data_import_kenya<- read.csv("Data/population_data_kenya_wpp.csv")
population_data_import_kilifi<- read.csv("Data/kilifi_age_dist.csv")
population_data_import_informal<- read.csv("Data/inform_age_dist.csv")

population_data_kenya<-data.table(population_data_import_kenya)
population_data_kilifi<-data.table(population_data_import_kilifi)
population_data_informal<-data.table(population_data_import_informal)


population_data<-population_data_informal

#population_data<-readRDS("popdata.RDS")
#popdata<-population_data

##########################################################################
#' age_groups is a data.table with the age groups you want in your matrix
#' for you, this will be the following if your 2nd agegroup is 18-59 
age_groups <- data.table(
  age_low = c(0, 18, 60),
  age_high = c(17, 59, 80)
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


manual_matrix<-calculate_matrix(contacts = contacts_S1, participants = participants_S1,population_data=population_data, weight_dayofweek = FALSE, 
                    symmetric_matrix = FALSE,age_groups = age_groups,use_reciprocal_for_missing=FALSE)
manual_matrix

manual_matrix_hh<-calculate_matrix(contacts = subset(contacts_S1, contacts_S1$contact_kind=="Household Contacts"), participants = participants_S1,population_data=population_data, weight_dayofweek = FALSE, 
                        symmetric_matrix = FALSE,age_groups = age_groups,use_reciprocal_for_missing=FALSE)
manual_matrix_hh

manual_matrix_nonhh<-calculate_matrix(contacts = subset(contacts_S1, contacts_S1$contact_kind=="Non-Household Contacts"), participants = participants_S1,population_data=population_data, weight_dayofweek = FALSE, 
                           symmetric_matrix = FALSE,age_groups = age_groups,use_reciprocal_for_missing=FALSE)
manual_matrix_nonhh

manual_matrix_phys<-calculate_matrix(contacts = subset(contacts_S1, contacts_S1$q4_contacttype=="Physical"), participants = participants_S1,population_data=population_data, weight_dayofweek = FALSE, 
                          symmetric_matrix = FALSE,age_groups = age_groups,use_reciprocal_for_missing=FALSE)
manual_matrix_phys

manual_matrix_nonphys<-calculate_matrix(contacts = subset(contacts_S1, contacts_S1$q4_contacttype=="Non-Physical"), participants = participants_S1,population_data=population_data, weight_dayofweek = FALSE, 
                              symmetric_matrix = FALSE,age_groups = age_groups,use_reciprocal_for_missing=FALSE)
manual_matrix_nonphys



