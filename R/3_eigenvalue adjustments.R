
manual_50plus<-readRDS("Data/contact_matrices/manual_50plus_boots1000.rds")
manual_50plus_phys<-readRDS("Data/contact_matrices/manual_50plus_phys_boots1000.rds")

manual_50plus_mean <- Reduce("+", lapply(manual_50plus, function(x) {x$all})) / length(manual_50plus)
manual_50plus_phys_mean <- Reduce("+", lapply(manual_50plus_phys, function(x) {x$all})) / length(manual_50plus_phys)

#replacing NA to 0s to calc eigenvalues
manual_50plus_mean[,1]<-0
manual_50plus_phys_mean[,1]<-0

#checking
manual_50plus_mean
manual_50plus_phys_mean
eigen(manual_50plus_mean)
eigen(manual_50plus_phys_mean)


#replacing first element of kiti matrix to zeros to calc eigenvalue, as these are not in pop council
kiti_matrix_comp<-adjusted_matrix_kiti
kiti_matrix_comp[,1]<-0
kiti_matrix_comp
#calculating eigenvalue of kiti
e_kiti_comp<-eigen(kiti_matrix_comp)
e_kiti_comp

#calculating eigenvalue of prem old and new

prem_old_comp<-adjusted_prem_matrix_old
prem_old_comp[,1]<-0
e_prem_old<-eigen(prem_old_comp)
e_prem_old

prem_new_comp<-adjusted_prem_matrix_new
prem_new_comp[,1]<-0
e_prem_new<-eigen(prem_new_comp)
e_prem_new


#calculating eigenvalue ratios of comparable matrices

#Manual:Prem old
scaling_factor_prem_old<-  max(eigen(manual_50plus_mean, only.values = TRUE)$values)/max(eigen(prem_old_comp, only.values = TRUE)$values)
scaling_factor_prem_old

#Manual:Prem new
scaling_factor_prem_new<-  max(eigen(manual_50plus_mean, only.values = TRUE)$values)/max(eigen(prem_new_comp, only.values = TRUE)$values)
scaling_factor_prem_new

#Manual:Kiti
scaling_factor_kiti<-  max(eigen(manual_50plus_phys_mean, only.values = TRUE)$values)/max(eigen(kiti_matrix_comp, only.values = TRUE)$values)
scaling_factor_kiti

#=-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#set population of new data
N_new<-N_informal

#Prem old~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_matrix<-manual_50plus_mean
baseline_matrix<-adjusted_prem_matrix_old

#' 2. use scaling factors to impute values for matrix
new_matrix[,1] <- baseline_matrix[,1] * scaling_factor_prem_old
#' 3. As a last step, you make matrix symmetric
population_matrix = new_matrix;
for(i in 1:nrow(population_matrix)){
  for(j in 1:ncol(population_matrix)){
    population_matrix[i,j] = (population_matrix[i,j] + population_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
manual_matrix_adj_by_prem_old<-population_matrix
manual_matrix_adj_by_prem_old

#Prem new~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_matrix<-manual_50plus_mean
baseline_matrix<-adjusted_prem_matrix_new

#' 2. use scaling factors to impute values for matrix
new_matrix[,1] <- baseline_matrix[,1] * scaling_factor_prem_new
#' 3. As a last step, you make matrix symmetric
population_matrix = new_matrix;
for(i in 1:nrow(population_matrix)){
  for(j in 1:ncol(population_matrix)){
    population_matrix[i,j] = (population_matrix[i,j] + population_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
manual_matrix_adj_by_prem_new<-population_matrix
manual_matrix_adj_by_prem_new

#Kiti~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_matrix<-manual_50plus_phys_mean
baseline_matrix<-adjusted_matrix_kiti

#' 2. use scaling factors to impute values for matrix
new_matrix[,1] <- baseline_matrix[,1] * scaling_factor_kiti
#' 3. As a last step, you make matrix symmetric
population_matrix = new_matrix;
for(i in 1:nrow(population_matrix)){
  for(j in 1:ncol(population_matrix)){
    population_matrix[i,j] = (population_matrix[i,j] + population_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
manual_matrix_adj_by_kiti<-population_matrix
manual_matrix_adj_by_kiti


#Manual to kiti
eigenratio_kiti <-max(eigen(manual_matrix_adj_by_kiti, only.values = TRUE)$values)/max(eigen(adjusted_matrix_kiti, only.values = TRUE)$values)
eigenratio_kiti
#manual to prem old
eigenratio_prem_old <-max(eigen(manual_matrix_adj_by_prem_old, only.values = TRUE)$values)/max(eigen(adjusted_prem_matrix_old, only.values = TRUE)$values)
eigenratio_prem_old
#manual to prem new
eigenratio_prem_new <-max(eigen(manual_matrix_adj_by_prem_new, only.values = TRUE)$values)/max(eigen(adjusted_prem_matrix_new, only.values = TRUE)$values)
eigenratio_prem_new

1-eigenratio_kiti
1-eigenratio_prem_old
1-eigenratio_prem_new

scaling_factor_prem_old
scaling_factor_prem_new
scaling_factor_kiti

#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'              
#'              Calculating R0
#'
#'
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' get distribution of R0
#' this is the distribution we used based on the meta-analysis Amy did, but may want to use something Kenya specific if available?
r_values <- rnorm(1000, 2.6, 0.54)

#removing NA values (comparing with adjusted kiti and prem matrices with missing components also)
for (i in 1:length(manual_50plus)){
  manual_50plus[[i]]$all[,1]<-0}

for (i in 1:length(manual_50plus_phys)){
  manual_50plus_phys[[i]]$all[,1]<-0}


#calculating scaling factors for each bootstrapped matrix

    r0_scaling_prem_new <- sapply(
      1:length(manual_50plus),
      function(x){
        max(Re(eigen(manual_50plus[[x]]$all)$values[1]))/max(Re(eigen(prem_new_comp)$values[1]))
            }
    )

    r0_scaling_prem_old <- sapply(
      1:length(manual_50plus),
      function(x){
        max(Re(eigen(manual_50plus[[x]]$all)$values[1]))/max(Re(eigen(prem_old_comp)$values[1]))
      }
    )
    
    r0_scaling_kiti <- sapply(
      1:length(manual_50plus),
      function(x){
        max(Re(eigen(manual_50plus_phys[[x]]$all)$values[1]))/max(Re(eigen(kiti_matrix_comp)$values[1]))
      }
    )
    

    
adjusted_r0_prem_new<-r0_scaling_prem_new*r_values
adjusted_r0_prem_old<-r0_scaling_prem_old*r_values
adjusted_r0_kiti<-r0_scaling_kiti*r_values

mean(adjusted_r0_prem_new)
mean(adjusted_r0_prem_old)
mean(adjusted_r0_kiti)

adj_r0_df<- data.frame(r_values, adjusted_r0_prem_new, adjusted_r0_prem_old, adjusted_r0_kiti)

head(adj_r0_df)

quantile(adjusted_r0_kiti)
quantile(adjusted_r0_prem_old)
quantile(adjusted_r0_prem_new)


s<- adjusted_r0_kiti
adj_r0_df %>%
summarise(mean(s),
std = sqrt(var(s)),
lower = mean(s) - qnorm(.975)*std/sqrt(n()),
upper = mean(s) + qnorm(.975)*std/sqrt(n()))

s<- adjusted_r0_prem_new
adj_r0_df %>%
  summarise(mean(s),
            std = sqrt(var(s)),
            lower = mean(s) - qnorm(.975)*std/sqrt(n()),
            upper = mean(s) + qnorm(.975)*std/sqrt(n()))

s<- adjusted_r0_prem_old
adj_r0_df %>%
  summarise(mean(s),
            std = sqrt(var(s)),
            lower = mean(s) - qnorm(.975)*std/sqrt(n()),
            upper = mean(s) + qnorm(.975)*std/sqrt(n()))



