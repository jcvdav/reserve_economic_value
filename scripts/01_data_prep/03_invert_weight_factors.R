################################################################################
# Get invertebrate weight factors
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Computes standard weight values for all inverts analyzed
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

pacman::p_load(
  here,
  tidyverse
)

# Purple sea urchin (S. purpuratus) --------------------------------------------
#  data from Smith et al., 2021 at https://peerj.com/articles/11352/
urchin <- read.csv(
  file = "https://dfzljdn9uc3pi.cloudfront.net/2021/11352/1/Smith_raw_data_file.csv") %>% 
  drop_na(UWM, TD) %>% 
  filter(UWM > 0)

# Fit a log-log model
model <- lm(log(UWM) ~ log(TD), urchin)

# Get the coefficients
a <- exp(coefficients(model)[1])
b <- coefficients(model)[2]

# Standardized weight factor based on a 45 mm test diameter minimum catch size
s_purp <- a * (45 ^ b)

# Red sea urchin ---------------------------------------------------------------
# Data from Leus et al., 2014 at https://puha.org/wp-content/uploads/2017/11/2013-Framework-for-Estimating-Quota-Options-for-the-RSU-Fishery.pdf
# Factor using the 80 mm minium catch size
m_frans <- 0.0012659 * (80 ^ 2.7068)

# Cucumber ---------------------------------------------------------------------
# Chavez http://calcofi.org/publications/calcofireports/v52/Vol_52_136-147.Chavez.pdf
cucumber <- 280 #(Weight at first catch)

# Abalone
# Data from Rosetto et al., athttps://bioone.org/journals/journal-of-shellfish-research/volume-32/issue-1/035.032.0122/Reproductive-Potential-Can-Predict-Recruitment-Rates-in-Abalone/10.2983/035.032.0122.full
# And Catch size from and http://dof.gob.mx/nota_detalle.php?codigo=4663148&fecha=29/06/1987
abalone <- 2.24e-5 * (136 ^ 3.36)

# Lobster ----------------------------------------------------------------------
# Panulirus argus 135 milímetros de longitud abdominal =  74.6 milímetros de longitud de cefalotórax = 223 milímetros de longitud total (http://www.dof.gob.mx/normasOficiales/6114/sagarpa_06092016/sagarpa_06092016.html)
# Douglas J Neilson  (https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=41068&inline) Assessment of the California Spiny Lobster
p_argus <- 0.001 * (223 ^ 2.380) #

# Panulirus interruptus
# 82.5 milímetros de longitud de cefalotórax equivalente = longitud abdominal de 175 milímetros (http://www.dof.gob.mx/normasOficiales/6114/sagarpa_06092016/sagarpa_06092016.html)
# Murray & Jennings-Clarck An analysis of some morphometric characteristics of the spiny lobster, Panulirus argus, in St. Lucia(https://aquadocs.org/bitstream/handle/1834/28705/gcfi_44-41.pdf?sequence=1)
p_interruptus <- 0.3 * (17.5 ^ 2.51)

# Build a data.frame -----------------------------------------------------------

w_factors <- tibble(taxonomic_group = c("Holothuroidea",
                                        "Strongylocentrotus purpuratus",
                                        "Mesocentrotus franciscanus",
                                        "Haliotidae",
                                        "Panulirus argus",
                                        "Panulirus interruptus"),
                    w_fact = c(cucumber,
                               s_purp,
                               m_frans,
                               abalone,
                               p_argus,
                               p_interruptus))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
write.csv(x = w_factors,
          file = here("data", "raw_data", "invertebrate_weight_factors.csv"))
