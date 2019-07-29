# Perhaps Pharmacy needs to be deleted

library(data.table)
library(readxl)

# Source
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/dictionaries.R")

setwd("/home/sergiy/Documents/Work/Nutricia/1/Data")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/cleanDF.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/addSegments.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/add_CDC_EC_AC.R")

# Read raw data and join
df = fread("BF_Y2019M06.csv")

# Verification
str(df)
df[, .(.N, sum(Volume)), by = Ynb] # all is correct

# Check share of blank lines
df[, df[Brand == "", sum(Value)]*100/sum(Value), by = Ynb]
# if sales of empty cells is big

df[Brand == "", .(Ynb, Comments), by = SKU] # coders should check empty cells

# select data to work with
df = df[Brand != "" & (Value + Volume) > 0]

# Clean the dataset, Pharmacy is deleted
df = cleanDF(df)

# Select data to save
df = df[, .(Pieces = sum(PIECES),
            Value = sum(Value),
            Volume = sum(Volume)),
        by = .(SKU, Ynb, Mnb, 
               Brand, SubBrand,
               Size, Age, Scent,
               PS0, PS2, PS3, PS,
               Company, Form, Additives,
               Channel, Region)]

# Add segments (Scent, Acidified, price segments, etc.)
df = addSegments(df)

# Add coefficients (calendar days correction, extrapolation, additionl correction)
df = add_CDC_EC_AC(df)

df[, `:=`(PiecesC = Pieces*CDC*EC*AC,
          ValueC = Value*CDC*EC*AC,
          VolumeC = Volume*CDC*EC*AC)]
# Delete unnecessary columns
#[, c("CDC", "EC") := NULL]

# Check if there are brands without price segments
df[(PriceSegment == "" | is.na(PriceSegment)) & (Form != "Liquid") &
     (PS0 == "IMF" | PS2 == "Dry Food" | PS3 == "Fruits" | PS3 == "Savoury Meal"), 
   .(Price = sum(ValueC)/sum(VolumeC)), by = .(Brand, PS3, Form, PriceSegment)]

# Query to select price segment for a new brand
# df[PS3 == "Instant Cereals", 
#    .(Price = sum(ValueC)/sum(VolumeC)), by = PriceSegment]

# Check if there are empty scents
df[(Scent2 == "" | is.na(Scent2)) & 
     (PS3 == "Instant Cereals" | 
        PS3 == "Fruits" | PS3 == "Savoury Meal"), 
   .N, by = .(Scent, PS3)]

df1 = fread("df.csv")
df1[, .N, by = .(Ynb, Mnb)]
df = rbindlist(list(df1, df), use.names = TRUE)

fwrite(df, "df.csv", row.names = FALSE) # Working file
fwrite(df[, .(SKU,
          Ynb, Mnb,
          Brand, SubBrand,
          Size, Age, Scent,
          PS0, PS2, PS3, PS,
          Company,
          Form, Additives,
          Channel, Region,
          Pieces, Value, Volume,
          Acidified, Scent2, ScentType,
          PriceSegment, GlobalPriceSegment,
          CDC, EC, AC,
          PiecesC, ValueC, VolumeC)], "BFpivot4.csv", row.names = FALSE) # Pivot

# Pre-processed file
# Temporarily uppercase names

df = df[Form != "Liquid"]
df[, SKU:= toupper(SKU)]
df[, PS0:=toupper(PS0)]
df[, PS2:=toupper(PS2)]
df[, PS3:=toupper(PS3)]
df[, PS:=toupper(PS)]
df[, Brand:=toupper(Brand)]
df[, Company:=toupper(Company)]
df[, PriceSegment:=toupper(PriceSegment)]
df[, GlobalPriceSegment:=toupper(GlobalPriceSegment)]
df[, Additives:=toupper(Additives)]
df[, Region:=toupper(Region)]

df = df[, .(ITEMSC = sum(PiecesC), VALUEC = sum(ValueC), VOLUMEC = sum(VolumeC)),
        by = .(Ynb, Mnb, Brand, PS0, PS2, PS3, PS, Company, PriceSegment, 
               GlobalPriceSegment, Form, Additives, Region)]
fwrite(df, "BFprocessed.csv", row.names = FALSE) # pre-processed file
