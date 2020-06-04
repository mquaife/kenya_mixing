#' getPopdata
#' 
#' Retrieves age-stratified population data for a specific year from UNWPP
#'
#' @param country_code country-code path as used in folder structure, e.g. 'uk'
#' @param iso3 iso3 country-code, e.g. 'GBR' or 'NLD'
#' @param year year for which data is needed, e.g. 2020
#'
#' @return returns data.table with population data for each year
#' @export
#'
#' @examples
getPopdata <- function(country_code="KEN", iso3=NULL, year=as.numeric(format(Sys.Date(), "%Y"))){
  popdata <- qs::qread(file.path(data_path, "../unwpp_data.qs"))
  
  if(is.null(iso3)){
    iso3_ <- switch(
      country_code,
      "kenya" = "KEN",
      NULL
    )
    if(is.null(iso3_)){
      stop(sprintf("Don't know how to convert country code %s in ISO3 code. Update function or specify iso3", country_code))
    }
  } else {
    iso3_ <- iso3
  }
  
  popdata <- popdata[
    iso3 == iso3_
    & year == popyear
  ]
  
  if(nrow(popdata) > 0){
    return(popdata)
  } else {
    stop("Can't find population data")
  }
}

#' Process contact ages
#' 
#' Processes ages by sampling contact range, or resampling contacts in bootstrap
#'
#' @param contact_data data.table with contacts
#' @param contact_data_age_groups data.table with age-groups used in model
#' @param population_data data.table with age-specific population data (from getPopdata)
#' @param contact_age_process how should known contact_ages be processed
#' @param contact_age_unknown_process how should unknown contact ages be processed
#' @param bootstrap_samples how many bootstrap samples are needed
#' @param bootstrap_type how are contacts bootstrapped? One of no_sample, sample_participants_contacts, or bootstrap_all
#' @param participants_bootstrapped_sets list with data.tables of participants in each bootstrap
#'
#' @return returns a list with bootstrapped data sets
#' @export
#'
#' @examples
processContacts <- function(
  contact_data,
  contact_data_age_groups,
  population_data,
  contact_age_process,
  contact_age_unknown_process,
  bootstrap_samples = 0,
  bootstrap_type="bootstrap_all", 
  participants_bootstrapped_sets = NULL
){
  
  #function used to sample vector
  #if length(vector)==1, repeats that number
  zsample <- function(x, size, ...){
    if(length(x)==1){
      return(rep(x, size))
    } else {
      return(sample(x, size, ...))
    }
  }
  
  #wrapper to do bootstraps
  #function calls itself but skips this loop during bootstrap (when bootstrap_samples = -1)
  #this code processes how contacts are samples during bootstraps
  if(bootstrap_samples >= 0){
    message("bootstrapping contact dataset")
    if(bootstrap_samples > 0){
      #loop through each bootstrap sample
      return(lapply(
        lapply(
          1:bootstrap_samples,
          function(x, bootstrap_type, participants_bootstrapped_sets){
            #resamples contacts irrespective of participants
            #may lead to a reduced number of overall contacts
            if(bootstrap_type == "bootstrap_all"){
              return(contact_data[sample(x=c(1:nrow(contact_data)), size=nrow(contact_data), replace=T)])
            #resamples the contacts a single participant has
            #takes a long time
            } else if(bootstrap_type == "sample_participants_contacts"){
              rbindlist(lapply(
                1:nrow(participants_bootstrapped_sets[[x]]),
                function(x, part_id_old, part_id_new){
                  z <- contact_data[part_id == part_id_new[x]]
                  return(z[sample(1:nrow(z), nrow(z), T)])
                },
                part_id_new = participants_bootstrapped_sets[[x]][, part_id]
              ))
            #does not resamples contacts during bootstrap
            } else if(bootstrap_type == "no_sample"){
              return(contact_data[part_id %in% participants_bootstrapped_sets[[x]][, part_id]])
            }
          },
          bootstrap_type,
          participants_bootstrapped_sets
        ),
        #recall function for bootstrapped run
        processContacts,
        contact_data_age_groups=contact_data_age_groups,
        population_data=population_data,
        contact_age_process=contact_age_process,
        contact_age_unknown_process=contact_age_unknown_process,
        bootstrap_samples=-1,
        bootstrap_type=bootstrap_type,
        participants_bootstrapped_sets=participants_bootstrapped_sets
      ))
    } else {
      return(list(processContacts(contact_data, contact_data_age_groups, population_data, contact_age_process, contact_age_unknown_process, -1)))
    }
  }
  
  message("new sample")
  
  #generalize this
  #set lower and upper bound of agegroup
  if(is.null(contact_data_age_groups)){
    if(sum(!c("cnt_age_exact" ,"cnt_age_est_min", "cnt_age_est_max") %in% colnames(contact_data)) > 0){
      stop("Don't know how to process these contacts")
    }
    contact_data[!is.na(cnt_age_exact), age_low := cnt_age_exact]
    contact_data[!is.na(cnt_age_exact), age_high := cnt_age_exact]
    
    contact_data[is.na(cnt_age_exact), age_low := cnt_age_est_min]
    contact_data[is.na(cnt_age_exact), age_high := cnt_age_est_max]
    
    contact_data[is.na(cnt_age_exact) & !is.na(age_low) & is.na(age_high), age_low := NA]
    contact_data[is.na(cnt_age_exact) & is.na(age_low) & !is.na(age_high), age_high := NA]
    
    contact_data[is.na(cnt_age_exact) & age_low == age_high, age_est := age_low]
  } else {
    contact_data <- merge(contact_data, contact_data_age_groups, by.x="cnt_age", by.y="name", all=TRUE)
  }
  
  #individuals with known (estimate or range) or completely unknown ages are processed differently
  contacts_age_known <- contact_data[!is.na(age_low)]
  contacts_age_unknown <- contact_data[is.na(age_low)]  
  
  #use the mean of age in one age-group
  if(contact_age_process == "mean"){
    contacts_age_known[, age_est := round(mean(c(age_low, age_high))), by=seq_len(nrow(contacts_age_known))]
  #sample uniformly within age-group
  } else if(contact_age_process == "sample_uniform"){
    contacts_age_known[, age_est := zsample(size=1, x=c(age_low:age_high), replace=T), by=seq_len(nrow(contacts_age_known))]
  #sample within age-group, but weighted by population distribution
  } else if(contact_age_process == "sample_popdist"){
    #generalize this
    if(is.null(contact_data_age_groups)){
      contacts_age_known[!is.na(cnt_age_exact), age_est := cnt_age_exact]
      contacts_age_known[!is.na(age_low) & age_low == age_high, age_est := age_low]
      #different age-groups (specified to speed up code)
      contacts_age_known[is.na(age_est), "tmp_agegrp"] <- paste0(
        contacts_age_known[is.na(age_est), age_low], "-", contacts_age_known[is.na(age_est), age_high]
      )
      for(agename in unique(contacts_age_known[is.na(age_est), tmp_agegrp])){
        contacts_age_known[is.na(age_est) & tmp_agegrp == agename, "age_est"] <- zsample(
          x=c(first(contacts_age_known[tmp_agegrp == agename, age_low]):first(contacts_age_known[tmp_agegrp == agename, age_high])),
          size=nrow(contacts_age_known[is.na(age_est) & tmp_agegrp == agename]),
          replace=T,
          prob=sapply(
            c(first(contacts_age_known[tmp_agegrp == agename, age_low]):first(contacts_age_known[tmp_agegrp == agename, age_high])),
            function(a){
              ifelse(a > max(population_data[,age]), population_data[age == max(age), total], population_data[age == a, total])
            }
          )
        )
      }
      contacts_age_known <- contacts_age_known[, -"tmp_agegrp"]
    } else {
      for(agename in contact_data_age_groups[!is.na(age_low), name]){
        contacts_age_known[cnt_age == agename, "age_est"] <- zsample(
          x=c(contact_data_age_groups[name == agename, age_low]:contact_data_age_groups[name == agename, age_high]),
          size=nrow(contacts_age_known[cnt_age == agename]),
          replace=T,
          prob=sapply(
            c(contact_data_age_groups[name == agename, age_low]:contact_data_age_groups[name == agename, age_high]),
            function(a){
              ifelse(a > max(population_data[,age]), population_data[age == max(age), total], population_data[age == a, total])
            }
          )
        )
      } 
    }
  }
  
  #sample agegroup of missing contacts from other contacts for same participant
  #sample age within agegroup based on population distribution
  #if participants has no contacts with known age, sample from population
  if(contact_age_unknown_process == "sample_partdist"){
    pids <- unique(contacts_age_unknown[, part_id])
    for(p in pids){
      if(nrow(contacts_age_known[part_id == p]) > 0){
        contacts_age_unknown[part_id == p, "age_est_t"] <- zsample(
          x = contacts_age_known[part_id == p, cnt_age],
          size = nrow(contacts_age_unknown[part_id == p]),
          replace = T
        )
      #sample from population if all contact ages are missing  
      } else {
        contacts_age_unknown[part_id == p, "age_est"] <- zsample(
          x = population_data[, age],
          size = nrow(contacts_age_unknown[part_id == p]),
          replace = T,
          prob = population_data[, total]
        )
      }
    }
    for(agegrp in unique(contacts_age_unknown[!is.na(age_est_t), age_est_t])){
      contacts_age_unknown[age_est_t == agegrp, "age_est"] <- zsample(
        c(contact_data_age_groups[name==agegrp, age_low]:contact_data_age_groups[name==agegrp, age_high]),
        size=nrow(contacts_age_unknown[age_est_t == agegrp]),
        replace=T,
        prob = sapply(
          c(contact_data_age_groups[name==agegrp, age_low]:contact_data_age_groups[name==agegrp, age_high]),
          function(a){
            ifelse(a > max(population_data[,age]), population_data[age == max(age), total], population_data[age == a, total])
          }
        )
      )
    }
    contacts_age_unknown <- contacts_age_unknown[, -"age_est_t"]
  #sample missing ages for contacts based on age-distribution in population
  } else if(contact_age_unknown_process == "sample_popdist"){
    contacts_age_unknown[, "age_est"] <- zsample(
      x=population_data[, age],
      size=nrow(contacts_age_unknown),
      replace = T,
      prob = population_data[, total]
    )
  #remove any contacts with missing age information
  } else if(contact_age_unknown_process == "remove"){
    contacts_age_unknown <- contacts_age_unknown[0,]
  }
  contacts <- rbindlist(list(contacts_age_known, contacts_age_unknown))
  return(contacts)
}


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