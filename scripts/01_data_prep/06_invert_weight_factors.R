
# Purple sea urching - Smith
urchin <- read.csv("https://dfzljdn9uc3pi.cloudfront.net/2021/11352/1/Smith_raw_data_file.csv") %>% 
  drop_na(UWM, TD) %>% 
  filter(UWM > 0)

model <- lm(log(UWM) ~ log(TD), urchin)

a <- exp(coefficients(model)[1])
b <- coefficients(model)[2]

s_purp <- a * (45 ^ b)

# Red sea urchin - Leus
m_frans <- 0.0012659 * (80 ^ 2.7068)

# Cucumber
# Chavez http://calcofi.org/publications/calcofireports/v52/Vol_52_136-147.Chavez.pdf
# Not used, but here's the info for the caribbean one: https://www.dof.gob.mx/nota_detalle_popup.php?codigo=5391774
cucumber <- 280 #(Weight at first catch)


# Abalone - Rosetto and http://dof.gob.mx/nota_detalle.php?codigo=4663148&fecha=29/06/1987
abalone <- 2.24e-5 * (136 ^ 3.36)

# Lobster
# Panulirus argus 135 milímetros de longitud abdominal =  74.6 milímetros de longitud de cefalotórax = 223 milímetros de longitud total (http://www.dof.gob.mx/normasOficiales/6114/sagarpa_06092016/sagarpa_06092016.html)
# Douglas J Neilson  (https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=41068&inline) Assessment of the California Spiny Lobster
p_argus <- 0.001 * (223 ^ 2.380) #



# Panulirus interruptus
# 82.5 milímetros de longitud de cefalotórax equivalente = longitud abdominal de 175 milímetros (http://www.dof.gob.mx/normasOficiales/6114/sagarpa_06092016/sagarpa_06092016.html)
# Murray & Jennings-Clarck An analysis of some morphometric characteristics of the spiny lobster, Panulirus argus, in St. Lucia(https://aquadocs.org/bitstream/handle/1834/28705/gcfi_44-41.pdf?sequence=1)
p_interruptus <- 0.3 * (17.5 ^ 2.51)

# 0.2 * (22.3 ^ 2.4960)

#
argus <- read_csv(here("data", "raw_data", "lobster_tl_samples.csv"))

n_above <- argus %>% 
  mutate(above = carapace_length >= 74.6) %>% 
  summarize(n = sum(above) / length(above) * 100) %>% 
  pull(n) %>% 
  round(2) %>% 
  paste0("%")

p <- ggplot(data = argus,
       mapping = aes(x = carapace_length)) +
  geom_histogram(color = "black",
                 fill = "steelblue") +
  geom_vline(xintercept = 74.6, linetype = "dashed") +
  theme_bw() +
  labs(x = "Carapace length (mm)",
       y = "Absolute frequency",
       subtitle = paste(n_above, "of sampled lobster (N = 173) are above the minimum catch size"))

ggsave(plot = p,
       filename = here("results", "img", "carapace_length_histogram.png"),
       width = 6, height = 4.5)  
