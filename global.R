rm(list=ls())
cat('\014')

facebook <- read.csv('./Facebook_metrics/dataset_Facebook.csv', sep = ';', stringsAsFactors = F)
diabetes <- read.csv('./dataset_diabetes/diabetic_data.csv', stringsAsFactors = F)

head(facebook)
# Facebook synopsis - 
# This breaks down the facebook post into likes and various metrics to infer popularity
head(diabetes)
# Mostly binary data. - with many missing values 


# bubble plot
# small multiples plot
# https://blog.dominodatalab.com/visualizing-homeownership-in-the-us-using-small-multiples-and-r/
# http://www.gettinggeneticsdone.com/2010/01/ggplot2-tutorial-scatterplots-in-series.html
# binary data (yes/no) - graph with blank elements
# scatterplot matrix
# heatmap
# paracoord

# which plots will look good?
# scatter 1 vs rest

#------------------------------------------------------- Small Multiples Data
# get the variables of interest (all obscure medical terms)
medicalNames <- colnames(diabetes)[25:47]

# cast the values into counts
long_format <- diabetes[c('time_in_hospital', medicalNames)] %>% gather(medicalName, Status, -time_in_hospital )
recasted <- dcast(long_format, medicalName + time_in_hospital ~ Status, length) # status to counts

# Create a long dataframe with proportion difference from the mean proportion
small_multiples_data <- recasted %>% 
  mutate(row_num = rowSums(.[3:6])) %>%
  mutate(Down = Down/row_num,
         No = No/row_num, 
         Steady = Steady/row_num,
         Up = Up/row_num)

multiplesMean <- small_multiples_data %>% 
  group_by(medicalName)  %>% 
  summarise(downMean = mean(Down), 
            noMean = mean(No), 
            steadyMean = mean(Steady), 
            upMean = mean(Up))

small_multiples_data <- merge(small_multiples_data, multiplesMean, by = 'medicalName')
  
small_multiples_data <-small_multiples_data %>% 
  mutate(Down = Down - downMean,
         No = No - noMean,
         Steady = Steady - steadyMean,
         Up = Up - upMean) %>%
  select(-row_num, -downMean, -noMean, -steadyMean, -upMean) %>%
  gather(Status, diffToMean, -time_in_hospital, -medicalName) %>%
  mutate(Color = ifelse(diffToMean > 0, 'Above', 'Below'),
         Status = factor(Status, levels = c('Down', 'Steady', 'Up', 'No'), ordered=T))


# ------------> small_multiples_data


#------------------------------------------------------- Bubble Plot Data


bubbleData <- diabetes %>% select(age, readmitted, num_procedures, num_lab_procedures, num_medications, time_in_hospital,
                                  number_emergency, number_outpatient, number_inpatient, number_diagnoses) %>%
  group_by(age, readmitted) %>% 
  summarise(num_lab_procedures = mean(num_lab_procedures), 
            num_medications = mean(num_medications), 
            num_procedures = mean(num_procedures),
            time_in_hospital = mean(time_in_hospital),
            number_emergency = mean(number_emergency), 
            number_outpatient = mean(number_outpatient), 
            number_inpatient = mean(number_inpatient), 
            number_diagnoses = mean(number_diagnoses),
            num_patients = n())
bubbleData$ID <- c(1:nrow(bubbleData))



# ------------> small_multiples_data




# ------------------------------------------------------------- Organize paracoord data

# columns to remove - all obscure medical terms
paraCoordData_ <- diabetes[!(colnames(diabetes) %in% medicalNames)]

toRemove <- c("encounter_id", "patient_nbr", "weight", 
              "admission_type_id", "discharge_disposition_id", 
              "admission_source_id", 'payer_code',  'medical_specialty', 
              'diag_1', 'diag_2', 'diag_3', 'max_glu_serum', 'A1Cresult', 'race', 
              'gender', 'change', 'diabetesMed')
# weight is missing more than 75% of values
# removed "admission_type_id"        "discharge_disposition_id" "admission_source_id"  
# because there is no sortable intuition to them....
# medical_specialty has over 75 values...tooooo many
# removed 'max_glu_serum', 'A1Cresult' because they were both missing almost 100,000 values
paraCoordData_ <- paraCoordData_[!(colnames(paraCoordData_) %in% toRemove)]
paraCoordData_ <- paraCoordData_ %>% mutate(
  age = as.numeric(factor(age, levels = sort(unique(paraCoordData_$age)), ordered = T)), 
  readmitted = as.numeric(factor(readmitted, levels = c('NO', '<30', '>30'), ordered =T))
)

paraColumns <- colnames(paraCoordData_)

str(paraCoordData_)
