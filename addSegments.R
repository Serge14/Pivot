addSegments = function(df) {
  
  ### Add additional columns
  
  # Acidified
  df[PS == "Digestive Comfort", Acidified := "Non-Acidified"]
  df[PS == "Digestive Comfort" & 
       (grepl("bifid", SubBrand, ignore.case = TRUE) | 
          grepl("kislom", SubBrand, ignore.case = TRUE)),
     Acidified := "Acidified"]
  
  # Puree & Cereals
  df[dictScent, on = c("Scent", "PS3"),
     `:=`(Scent2 = i.Scent2, ScentType = i.Type)]
  
  # Price Segments local
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Puree", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  # Price Segments global
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  # dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  dictPriceSegments[Segment == "Savoury Meal", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
 }