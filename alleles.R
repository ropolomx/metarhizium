
# Load packages -----------------------------------------------------------

mrob_habitat <- read_tsv('data_for_structure/mrob_habitat.txt')

mrob_ag <- 
  mrob_habitat %>%
  filter(X2 == 1)

mrob_alleles <- lapply(mrob_ag[,3:ncol(mrob_ag)], unique)

mrob_nat <- 
  mrob_habitat %>%
  filter(X2 == 2)

mrob_alleles <- lapply(mrob_habitat[,3:ncol(mrob_ag)], unique)
