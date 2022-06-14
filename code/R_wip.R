source('code/R_functions.R')

# mix romansh
rm1 <- read_language('rm','cv','vallader')
rm2 <- read_language('rm','cv','sursilv')

merge(rm1,rm2, by='word', all=T)
