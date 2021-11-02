
# Purple sea urching - Smith
urchin <- read.csv("https://dfzljdn9uc3pi.cloudfront.net/2021/11352/1/Smith_raw_data_file.csv") %>% 
  drop_na(UWM, TD) %>% 
  filter(UWM > 0)

model <- lm(log(UWM) ~ log(TD), urchin)

a <- exp(coefficients(model)[1])
b <- coefficients(model)[2]

fact <- a * (45 ^ b)

# Red sea urchin - Leus
0.0012659 * (80 ^ 2.7068)


# Abalone - Rosetto and http://dof.gob.mx/nota_detalle.php?codigo=4663148&fecha=29/06/1987
2.24e-5 * (136 ^ 3.36)

# Lobster - Neilson and http://www.dof.gob.mx/normasOficiales/6114/sagarpa_06092016/sagarpa_06092016.html
0.2 * (22.3 ^ 2.4960)
