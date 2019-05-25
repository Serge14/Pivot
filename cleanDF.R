cleanDF = function(df) {
  
  # Delete items in pack from the name of SKUs
  df[as.numeric(ItemsInPack) > 1, SKU := gsub("_[0-9]*\\*", "_", SKU)]
  
  # Convert sales pieces into sales of individual items
  df[as.numeric(ItemsInPack) > 1, PIECES := PIECES*ItemsInPack]
  
  df = df[, Age := gsub("Oct", "10", Age)]
  df = df[, Age := gsub("Dec", "12", Age)]
  df = df[, Age := gsub("12-Jun", "6-12", Age)]
  
  # Delete wite spaces in Scent
  cols = c("Scent", "SKU")
  df[ , (cols) := lapply(.SD, function(x) {gsub(" - ", "-", x)}), .SDcols = cols]
  df[ , (cols) := lapply(.SD, function(x) {gsub("- ", "-", x)}), .SDcols = cols]
  df[ , (cols) := lapply(.SD, function(x) {gsub(" -", "-", x)}), .SDcols = cols]
  
  # Some more manipulation
  #df[OutletType == "Pharmacy", Channel := "Pharmacy"]
  df = df[OutletType != "Pharmacy"]
  df[Channel == "DRUG", Channel := "MT"]
  df[, Size := paste0(Size, UnitOfmeasure)]
  df[Brand == "Private label", `:=`(SubBrand = "", Size = "")]
  
  # Change some names
  
  df[grepl("^Friso", Brand), Brand := "Friso"]
  df[SubBrand == "Pomagaika" | SubBrand == "Shagaika", Brand := "Nestle"]
  
  df[Company == "Khorolskii Mk" & Brand == "Malysh", Brand := "Malysh Kh"] 
  df[Company == "Khorolskii Mk" & Brand == "Malyutka", Brand := "Malyutka Kh"] 
  df[Company == "Nutricia" & Brand == "Malysh Istrinskii", Brand := "Malysh Istr"] 
  df[Company == "Associaciya Detskogo Pitaniya", Company := "ADP"] 
  df[Company == "Abbott Laboratories", Company := "Abbott Lab"] 
  df[Company == "Kompaniya Ekstreyd", Company := "Ekstreyd"]
  
  # Convert SKUs
  
  df[SKU == "Nan_Premium Gipoallergennyi 3_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nestle",
     PS := "Hypoallergenic"]
  
  df[SKU == "Nan_Premium Kislomolochnyi 3_400Gr_12+_Kislomolochnyi_IMF_Gum_Specials_Gum Specials_Nestle",
     PS := "Digestive Comfort"]
  
  df[SKU == "Kabrita_Gold 3_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Hyproca Nutrition" |
       SKU == "Kabrita_Gold 3_800Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Hyproca Nutrition" |
       SKU == "Nenni_Nenni 3_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Bibicall" |
       SKU == "Nenni_Zolotaya Kozochka_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Bibicall" |
       SKU == "Alpro_Alpro_1000Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Alpro",
     PS := "Soy / Goat"]
  
  df[SKU == "Nutrilon_Pepti Junior_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia" |
       SKU == "Nutrilon_Pepti Junior_450Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia",
     PS := "Allergy Treatment"]
  
  df[SKU == "Nutrilon_Pepti Junior_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia" |
       SKU == "Nutrilon_Pepti Junior_450Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia",
     PS2 := "IF"]
  
  
  df[SKU == "Nutrilon_Nutrilon_200Gr_0+_N/S_IMF_IF_Specials_Preterm_Nutricia",
     SubBrand := "Protein Supplement"]
  
  df[SKU == "Humana_Plus_500Gr_0+_N/S_IMF_IF_Specials_Soy / Goat_Dmk Humana",
     SubBrand := "Sl Plus"] # or SL?
  
  df[SKU == "Malysh_Malysh_350Gr_4+_Oat_IMF_IF_Plus_BPFO_Khorolskii Mk" |
       SKU == "Malysh_Malysh_350Gr_4+_Rice_IMF_IF_Plus_BPFO_Khorolskii Mk",
     PS := "BPIF"]
  
  df[SKU == "Nutrilon_Komfort 1_900Gr_0+_N/S_IMF_IF_Base_BIF_Nutricia" |
       SKU == "Nutrilon_Komfort 2_900Gr_6+_N/S_IMF_FO_Base_BFO_Nutricia",
     `:=`(PS = "Digestive Comfort",
          PS3 = "Specials")]
  
  # df[SKU == "Malyutka_Plus 2_350Gr_6+_Buckwheat_IMF_FO_Plus_BPFO_Nutricia" &
  #      Ynb > 2014 ,
  #    `:=`(SKU = "Malyutka_Premium_350Gr_6-12_Buckwheat_IMF_FO_Plus_BPFO_Khorolskii Mk",
  #         SubBrand = "Premium",
  #         Age = "6-12",
  #         Company = "Khorolskii Mk")]
  
  df[SKU == "Nutrilon_Pepti Junior_450Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia",
     `:=`(SKU = "Nutrilon_Mal'absorbtsiya_400Gr_0+_N/S_IMF_IF_Specials_DR-NL_Nutricia",
          SubBrand = "Mal'absorbtsiya",
          Age = "0+",
          PS2 = "IF",
          PS = "DR-NL")]
  
  df[SKU == "Nutrilon_Pepti Junior_400Gr_12+_N/S_IMF_Gum_Specials_Gum Specials_Nutricia",
     `:=`(SKU = "Nutrilon_Pepti_400Gr_0+_N/S_IMF_IF_Specials_Allergy Treatment_Nutricia",
          SubBrand = "Pepti",
          Age = "0+",
          PS2 = "IF",
          PS = "Allergy Treatment")]
  
  # Delete SKU or data
  
  df = df[SKU != "Lasunya_Detolakt Antireflyuks_400Gr_0+_N/S_IMF_IF_Specials_Anti Reflux_Baltskii Mkkdp"]
  #df = df[SKU != "Milupa_Milupa 2_29Gr_6-12_N/S_IMF_FO_Base_BFO_Nutricia"]

  # df = df[!(SKU == "Malyutka_Malyutka_200Gr_12+_Fruits-Wheat_Foods_Dry Food_Instant Cereals_IMC_Nutricia" & 
  #             ((Ynb == 2013 & Mnb > 7)|(Ynb > 2013)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_200Gr_4+_Buckwheat_Foods_Dry Food_Instant Cereals_IPC_Nutricia"
  #           & ((Ynb == 2013 & Mnb > 8)|(Ynb > 2013)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_200Gr_5+_Apple-Oat-Wheat_Foods_Dry Food_Instant Cereals_IPC_Nutricia"
  #           & ((Ynb == 2013 & Mnb == 12)|(Ynb > 2013)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_200Gr_8+_Corn-Fruits-Rice_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2013 & Mnb > 6)|(Ynb > 2013)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_220Gr_5+_Apple-Buckwheat-Carrot_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 4)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_220Gr_5+_Pumpkin-Wheat_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 4)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_4+_Apricot-Buckwheat-Rice_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2013 & Mnb > 11)|(Ynb > 2013)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_4+_Buckwheat_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 5)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_4+_Kuraga-Rice_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 5)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_4+_Rice_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 6)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_5+_Oat_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 4)|(Ynb > 2014)))]
  # 
  # df = df[!(SKU == "Malyutka_Malyutka_250Gr_6+_Fruits-Oat_Foods_Dry Food_Instant Cereals_IMC_Nutricia"
  #           & ((Ynb == 2014 & Mnb > 5)|(Ynb > 2014)))]
  
  # Change volume
  df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_350Gr_6+_Buckwheat_IMF_FO_Plus_BPFO_Nutricia"
     & ((Ynb == 2015 & Mnb > 2)|(Ynb > 2015)),
     `:=`(SKU = "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Buckwheat_IMF_FO_Plus_BPFO_Nutricia",
          Size = "320Gr",
          Volume = Volume*0.32/0.35)]
  
  df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_350Gr_6+_Oat_IMF_FO_Plus_BPFO_Nutricia"
     & ((Ynb == 2015 & Mnb > 3)|(Ynb > 2015)),
     `:=`(SKU = "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Oat_IMF_FO_Plus_BPFO_Nutricia",
          Size = "320Gr",
          Volume = Volume*0.32/0.35)]
  
  df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_350Gr_6+_Rice_IMF_FO_Plus_BPFO_Nutricia"
     & ((Ynb == 2015 & Mnb > 3)|(Ynb > 2015)),
     `:=`(SKU = "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Rice_IMF_FO_Plus_BPFO_Nutricia",
          Size = "320Gr",
          Volume = Volume*0.32/0.35)]
  
  df[SKU == "Malysh Istrinskii_Malysh Istrinskii 1_350Gr_0+_N/S_IMF_IF_Base_BIF_Nutricia"
     & ((Ynb == 2015 & Mnb > 3)|(Ynb > 2015)),
     `:=`(SKU = "Malysh Istrinskii_Malysh Istrinskii 1_320Gr_0+_N/S_IMF_IF_Base_BIF_Nutricia",
          Size = "320Gr",
          Volume = Volume*0.32/0.35)]
  
  df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_350Gr_6+_N/S_IMF_FO_Base_BFO_Nutricia"
     & ((Ynb == 2015 & Mnb > 3)|(Ynb > 2015)),
     `:=`(SKU = "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_N/S_IMF_FO_Base_BFO_Nutricia",
          Size = "320Gr",
          Volume = Volume*0.32/0.35)]
  
  # df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Buckwheat_IMF_FO_Plus_BPFO_Nutricia"
  #    & ((Ynb == 2016 & Mnb > 3)|(Ynb > 2016)),
  #    `:=`(SKU = "Malysh_Malysh_350Gr_4+_Buckwheat_IMF_IF_Plus_BPIF_Khorolskii Mk",
  #         Brand = "Malysh",
  #         SubBrand = "Malysh",
  #         Size = "350Gr",
  #         Age = "4+",
  #         PS2 = "IF",
  #         PS = "BPIF",
  #         Company = "Khorolskii Mk",
  #         Volume = Volume*0.35/0.32)]
  # 
  # df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Oat_IMF_FO_Plus_BPFO_Nutricia"
  #    & ((Ynb == 2016 & Mnb > 3)|(Ynb > 2016)),
  #    `:=`(SKU = "Malysh_Malysh_350Gr_4+_Oat_IMF_IF_Plus_BPFO_Khorolskii Mk",
  #         Brand = "Malysh",
  #         SubBrand = "Malysh",
  #         Size = "350Gr",
  #         Age = "4+",
  #         PS2 = "IF",
  #         PS = "BPIF",
  #         Company = "Khorolskii Mk",
  #         Volume = Volume*0.35/0.32)]
  # 
  # df[SKU == "Malysh Istrinskii_Malysh Istrinskii 2_320Gr_6+_Rice_IMF_FO_Plus_BPFO_Nutricia"
  #    & ((Ynb == 2016 & Mnb > 3)|(Ynb > 2016)),
  #    `:=`(SKU = "Malysh_Malysh_350Gr_4+_Rice_IMF_IF_Plus_BPFO_Khorolskii Mk",
  #         Brand = "Malysh",
  #         SubBrand = "Malysh",
  #         Size = "350Gr",
  #         Age = "4+",
  #         PS2 = "IF",
  #         PS = "BPIF",
  #         Company = "Khorolskii Mk",
  #         Volume = Volume*0.35/0.32)]
  
  df[SKU == "Semper_Lemolac_650Gr_0-6_N/S_Foods_Wet Food_Specials_Anti Reflux_Hero Ag",
     `:=`(PS0 = "IMF", PS2 = "IF")]
  
}