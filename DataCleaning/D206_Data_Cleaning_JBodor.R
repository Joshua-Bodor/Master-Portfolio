library(dplyr)

setwd("D:/Documents/WGU_MSDA/NUM2/Task_1_Data")
med_raw_data <- read.csv(file="medical_raw_data.csv")



med_data_selected <- select(med_raw_data, Customer_id, HighBlood, Stroke, 
                            Complication_risk, Overweight, Arthritis, Diabetes, 
                            Hyperlipidemia, BackPain, Anxiety, 
                            Allergic_rhinitis, Reflux_esophagitis, Asthma)

labels <- select(med_raw_data, Customer_id, ReAdmis)

summary(med_data_selected)

i <- 0
for (x in is.na(med_data_selected$Anxiety)) {
  i <- i + 1
  if(x == TRUE) {
    med_data_selected[i, "Anxiety"] <- 0
  }
}

i <- 0
for (x in is.na(med_data_selected$Overweight)) {
  i <- i + 1
  if(x == TRUE) {
    med_data_selected[i, "Overweight"] <- 0
  }
}

i <- 0
for (x in med_data_selected$HighBlood) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "HighBlood"] <- 1
  } else {
    med_data_selected[i, "HighBlood"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Stroke) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Stroke"] <- 1
  } else {
    med_data_selected[i, "Stroke"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Arthritis) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Arthritis"] <- 1
  } else {
    med_data_selected[i, "Arthritis"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Diabetes) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Diabetes"] <- 1
  } else {
    med_data_selected[i, "Diabetes"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Hyperlipidemia) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Hyperlipidemia"] <- 1
  } else {
    med_data_selected[i, "Hyperlipidemia"] <- 0
  }
}

i <- 0
for (x in med_data_selected$BackPain) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "BackPain"] <- 1
  } else {
    med_data_selected[i, "BackPain"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Allergic_rhinitis) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Allergic_rhinitis"] <- 1
  } else {
    med_data_selected[i, "Allergic_rhinitis"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Reflux_esophagitis) {
  i <- i + 1
  if(x == "Yes") {
    med_data_selected[i, "Reflux_esophagitis"] <- 1
  } else {
    med_data_selected[i, "Reflux_esophagitis"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Asthma) {
  i <- i + 1
  if (x == "Yes") {
    med_data_selected[i, "Asthma"] <- 1
  } else {
    med_data_selected[i, "Asthma"] <- 0
  }
}

i <- 0
for (x in med_data_selected$Complication_risk) {
  i <- i + 1
  if (x == "Low"){
    med_data_selected[i, "Complication_risk"] <- 0
  } else if (x == "Medium") {
    med_data_selected[i, "Complication_risk"] <- 0.5
  } else if (x == "High") {
    med_data_selected[i, "Complication_risk"] <- 1
  }
}


med_data_cleaned <- transform(med_data_selected, 
                              HighBlood=as.numeric(HighBlood), 
                              Stroke=as.numeric(Stroke), 
                              Complication_risk=as.numeric(Complication_risk), 
                              Arthritis=as.numeric(Arthritis), 
                              Diabetes=as.numeric(Diabetes), 
                              Hyperlipidemia=as.numeric(Hyperlipidemia), 
                              BackPain=as.numeric(BackPain), 
                              Allergic_rhinitis=as.numeric(Allergic_rhinitis), 
                              Reflux_esophagitis=as.numeric(Reflux_esophagitis), 
                              Asthma=as.numeric(Asthma))

summary(med_data_cleaned)
