add_CDC_EC_AC = function(df) {
  
  # Add calendar days correction
  df[dictCDC, on = c("Ynb", "Mnb"), CDC := i.CDC]
  df[Channel == "PHARMA", CDC := 1] # because PHARMA is collected on a monthly basis
  
  # Check if there are segments without CDC
  df[is.na(CDC), .N]
  
  # Extrapolation coefficients
  df[dictEC, on = c("Channel", "PS3"), EC := i.EC]
  df[PS == "Digestive Comfort" & Channel == "MT", EC := 5.29]
  df[PS == "Digestive Comfort" & Channel == "PHARMA", EC := 1.48]
  df[PS == "Hypoallergenic" & Channel == "MT", EC := 10.55]
  df[PS == "Hypoallergenic" & Channel == "PHARMA", EC := 1.73]
  df[is.na(EC), .N]
  
  # Additional correction
  df[, AC := 1]
  df[is.na(AC), .N]
  
  # Old problem, Pharma * 1.3
  df[Ynb == 2016 & Mnb %in% c(3:9) & 
       Channel == "PHARMA" & 
       PS3 != "Specials", 
     AC := 1.3]
  
  # new problem, for instant cereals only
  df[Ynb == 2017 & Mnb %in% c(5:10) & Channel == "PHARMA" & 
       PS3 == "Instant Cereals", 
     AC := 1.091] # was 1.2
  
  # Some more, Specials w/o HA
  df[Ynb == 2017 & Mnb %in% c(5:10) & Channel == "PHARMA" & 
       PS3 == "Specials" & 
       PS != "Hypoallergenic", 
     AC := 1.2]
  
  df[Ynb == 2018 & Mnb > 7 & Channel == "PHARMA" &
       PS3 == "Specials" &
       PS != "Hypoallergenic",
     AC := 0.85]
  
  df[Ynb > 2018 & Channel == "PHARMA" &
       PS3 == "Specials",
     AC := 0.85] # HA excluded. Add in case if necessity
  
  df[Ynb == 2019 & Mnb == 3 & Channel == "PHARMA" &
       PS3 == "Specials",
     AC := 0.7] # HA excluded. Add in case if necessity
  
  df[Ynb == 2018 & Mnb > 7 & Channel == "PHARMA" & 
       PS3 != "Specials" & 
       PS0 == "IMF", 
     AC := 0.906] # coefficients are calculated for 0.85
  
  df[Ynb > 2018 & Channel == "PHARMA" & 
       PS3 != "Specials" & 
       PS0 == "IMF", 
     AC := 0.906] # coefficients are calculated for 0.85
  
  #Gum Base
  df[Ynb >= 2018 & (Channel == "PHARMA" | Channel == "MT") &
       PS3 == "Base" &
       PS2 == "Gum",
     AC := 0.9]
  
  # To get the growth for nutricia, special case
  df[Ynb == 2017 & (Mnb == 1 | Mnb == 5) & PS3 == "Specials" &
       Company == "Nutricia",
     AC := 0.875]
  
  df[Ynb == 2018 & Mnb == 2 & PS3 == "specials" &
       Company == "Nutricia",
     AC := 1.151]

  
}