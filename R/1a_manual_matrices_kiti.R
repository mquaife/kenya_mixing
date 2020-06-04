#make sure pars_age_group matches format ([() and numbers of what it made with cut()
#AND this is in the participants data frame

library(data.table)
source("R/manual_contact_matrix_functions.R")
boots<-1000
#memory.limit(size=16000)

#Use the same dataset sas before, but the names are slightly different

##########################################################################
#' part is a data.table with the following columns
#' part_id for participant id (integer)
#' part_age with the estimated age of the participant (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#setwd("C:/Users/matt_/Google Drive/Research/COVID/Pop council")
participants_import_kiti<-read.csv("Data/kiti_data_wide_csv.csv")
#participants_S1<-data.frame(participants_import)
participants_kiti<-data.table(participants_import_kiti)

participants_kiti$part_age<-participants_kiti$age_class_part_2

mean(participants_kiti$part_id)
mean(participants_kiti$part_age)
participants_kiti$weekday[participants_kiti$day==1]<-"Tuesday"
participants_kiti$weekday[participants_kiti$day==0]<-"Saturday"

participants<-participants_kiti
part<-participants

participants$part_age_group

##########################################################################
#' contacts is a data.table with the following columns
#' part_id for participant id (integer)
#' weekday with day of week when survey was done (string Monday - Sunday)
#' age_est with the estimated age of the contact

contacts_import_kiti<- read.csv("Data/kiti_data_long.csv")
#contacts_S1<-data.frame(contacts_import)
contacts_kiti<-data.table(contacts_import_kiti)
#
#

contacts_kiti$cnt_age_exact<-contacts_kiti$cnt_age

contacts_kiti$age_est=contacts_kiti$cnt_age
contacts_kiti$weekday="Tuesday"
contacts_kiti$cnt_age<-NA
contacts<-contacts_kiti

#contacts_S1<-readRDS("contacts_50plus.rds")
##########################################################################
#' popdata is a data.table with population estimates
#' it is programmed to be a data.table with one row for each agegroup, and the following columns:
#' age (integer)
#' total (integer with total number of contacts)
#' 

population_data_import_kenya<- read.csv("Data/population_data_kenya_wpp.csv")
population_data_import_kilifi<- read.csv("Data/kilifi_age_dist.csv")
population_data_import_informal<- read.csv("Data/inform_age_dist.csv")

population_data_kenya<-data.table(population_data_import_kenya)
population_data_kilifi<-data.table(population_data_import_kilifi)
population_data_informal<-data.table(population_data_import_informal)

popdata<-population_data_kilifi
##########################################################
#
#                 User defined options
#
##########################################################

popyear <- 2020

#age groups for which to create contact matrix
age_groups <- data.table(
  age_low = c(0, 19, 50),
  age_high = c(18, 49, 80)
)
age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])

#' how to handle contact ages in age-group?
#' in our survey, contact ages are not given by age i.e. age=20 but in a range i.e. age_low=15 and age_high=25
#' you can set this to mean, as this does not affect your estimates as long as you don't change the agegroups in your matrix
#' however, may be useful when comparing to the Kiti data
#' mean: take the mean between lower and upper bound
#' sample_uniform: sample an age using a uniform distribution between the lower and upper bound
#' sample_popdist: sample an age using the population distribution in the year of the survey (i.e. if there are more 15 than 20yo, higher probability that this person was 15)
contact_age_process <- c("mean", "sample_uniform", "sample_popdist")[3]

#' similarly, in our survey, for some individuals we had no age data at all
#' to process these, there are the following options:
#' remove: remove them from the dataset
#' sample_partdist: sample the age from other contacts from the same participants for who ages are known
#' sample popdist: sample an age between 0 and 100 based on the population distribution (high probability the individual was a child)
contact_age_unknown_process <-c("remove", "sample_partdist", "sample_popdist")[3]

#' how many bootstrap samples are needed?
#' set to 0 to disable bootstrap
bootstrap_samples <- boots

#' how are contacts from participants bootstrapped?
#' you can leave this set to no_sample
#' the options mean:
#' sample_participants_contacts: resample the contacts listed by the individual participant
#' no_sample: just take the contacts from the participants as they were reported
#' bootstrap_all: bootstrap all contacts, regardless of the participants included in bootstrap
bootstrap_type <-c("sample_participants_contacts", "no_sample", "bootstrap_all")[2]

#' what contact matrix subsets to create?
#' if you want to create different matrices for settings, you can create filters here
#' i.e. the following command will create 3 matrices:
#' list("all" = list(), "home" = list("cnt_home" = "Yes), "work = list("cnt_work" = "Yes"))
#' one for all contacts, one for contacts where column cnt_home == "Yes", and one for contacts where column cnt_work == "Yes"
#' The filter will depend on how columns in your dataset are named
#' This will just create one single matrix for all contacts
contact_filters <- list(
  "all" = list()
)
#contact_filters <- list(
#  "all" = list(),
#  "home" = list("cnt_home" = "Yes"),
#  "work" = list("cnt_work" = "Yes"),
#  "school" = list("cnt_school" = "Yes"),
#  "other" = list("cnt_home" = "No", "cnt_work" = "No", "cnt_school" = "No")
#)

##########################################################
#
#                 END user defined options
#
##########################################################

#these are the age groups in your contact data dataset
#in your part data, should be in column part_age_group
contact_data_age_groups <- data.table(
  age_low = c(0, 19, 50),
  age_high = c(18, 49, 80)
)
contact_data_age_groups[, "name"] <- paste0(contact_data_age_groups[, age_low], "-", contact_data_age_groups[, age_high])

#this will bootstrap your participants dataset
if(bootstrap_samples == 0){
  participants_bootstrapped_sets <- list(part)
} else {
  participants_bootstrapped_sets <- lapply(
    1:bootstrap_samples,
    function(x, part){
      good_sample <- FALSE
      while(!good_sample){
        part2 <- part[sample(x=1:nrow(part), size=nrow(part),replace=T)]
        if(sum(!unique(part2[, part_age_group]) %in% unique(part[, part_age_group])) == 0){
          good_sample <- TRUE
        }
      }
      return(part2)
    },
    part=part
  ) 
}

#this creates your bootstrapped input data to create matrices with
contacts_bootstrapped <- processContacts(
  contacts, contact_data_age_groups, popdata, contact_age_process="mean", contact_age_unknown_process,
  bootstrap_samples, participants_bootstrapped_sets, bootstrap_type="no_sample"
)

#different contact sets to calculate matrices for
contacts_bootstrapped_sets <- lapply(
  contacts_bootstrapped,
  function(contacts){
    lapply(
      setNames(c(1:length(contact_filters)), names(contact_filters)),
      function(x, contact_filters, contacts){
        if(length(contact_filters[[x]]) == 0){
          return(contacts)
        } else {
          #for loop will combine akin to & statement
          for(i in 1:length(contact_filters[[x]])){
            varname_ <- names(contact_filters[[x]])[[i]]
            varval_ <- contact_filters[[x]][[i]]
            contacts <- contacts[get(varname_) == varval_]
          }
          if(nrow(contacts) == 0){
            warning(sprintf("filter %s returned an empty contact dataset", names(contact_filters)[[x]]))
          }
          return(contacts)
        }
      },
      contact_filters=contact_filters,
      contacts=contacts
    )
  }
)

#use bootstrapped datasets to create matrices
contacts_bootstrapped_matrices <- lapply(
  1:length(contacts_bootstrapped_sets),
  function(i, contacts_bootstrapped_sets, participants_bootstrapped_sets){
    message(i)
    lapply(
      contacts_bootstrapped_sets[[i]],
      calculate_matrix,
      participants = participants_bootstrapped_sets[[i]],
      population_data = popdata, age_groups = age_groups,
      weight_dayofweek = FALSE, use_reciprocal_for_missing = TRUE, symmetric_matrix = TRUE
    )
  },
  contacts_bootstrapped_sets,
  participants_bootstrapped_sets
)

#matrix may be empty, as there may be no contacts for the selected participants in some settings
#for those matrices, use the assigned null_matrix
null_matrix <- matrix(0, nrow = nrow(age_groups), ncol = nrow(age_groups), dimnames = list(age_groups$name,age_groups$name))
null_matrix[c(1:3),c(1:3)] <- NA

contacts_bootstrapped_matrices <- lapply(
  contacts_bootstrapped_matrices,
  function(x){
    lapply(
      x,
      function(y){
        if(is.null(y)){
          return(null_matrix)
        } else {
          return(y)
        }
      }
    )
  }
)




contacts_bootstrapped_matrices_kiti<-  contacts_bootstrapped_matrices
contacts_bootstrapped_matrices_kiti_mean <- Reduce("+", lapply(contacts_bootstrapped_matrices_kiti, function(x) {x$all})) / length(contacts_bootstrapped_matrices_kiti)
contacts_bootstrapped_matrices_kiti_mean 


