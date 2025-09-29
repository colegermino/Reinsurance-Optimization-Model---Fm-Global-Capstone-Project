library(readxl)
library(writexl)

recovery <- c()
Year <- c()
Policy <- c()
Loss <- c()
Loss_ID <- c()

#Load in Hospital.xlsx Data.
#lossData Primary Key: Index Rec
#reinsuranceData Primary Key: Arr ID
#Bridge Foreign Keys: Index Rec, Arr ID
#currentHospitals Primary Key: Index Rec

#Import Data
reinsuranceData <- read_excel("/Users/garrett/Desktop/MSU/MSU_Spring_2025/Capstone/Code/HospitalData.xlsx", "Reinsurance Data" )
lossData <- read_excel("/Users/garrett/Desktop/MSU/MSU_Spring_2025/Capstone/Code/HospitalData.xlsx", sheet = "Losses 2014-2023")
bridge <- read_excel("/Users/garrett/Desktop/MSU/MSU_Spring_2025/Capstone/Code/HospitalData.xlsx", sheet = "Loc to Arr ID")
lossData <- lossData[lossData$`Index Rec` %in% bridge$`Index Rec`, ]
lossData <- merge(lossData,bridge, by = "Index Rec", all.x = TRUE)
lossData <- lossData[lossData$`Arr ID` %in% reinsuranceData$`Arr ID`,]

reinsuranceData <- reinsuranceData[reinsuranceData$`Attachment Pt Amt USD` < 50000000,]
#Calculate column for Premium Divided by Capacity

# Loop over each row of the Loss data frame.
for (i in 1:nrow(lossData)) {
  
  # Extract the loss value and associated details.
  loss_val <- lossData$`FM Global Share Total`[i]
  arr_id <- lossData$`Arr ID`[i]
  year_val <- lossData$`Occurred Year`[i]
  loss_id <- lossData$`LossId`[i] 
  
  total_recovery <- 0
  current_layer <- 1
  
  # Loop through reinsurance layers until a terminal state is reached.
  while (TRUE) {
    # Find the matching row in Reinsurance for the given Arr ID and current layer.
    ri_row <- reinsuranceData[reinsuranceData$`Arr ID` == arr_id & reinsuranceData$Layer == current_layer,]
    
    # If no matching row is found, exit the loop.
    if(nrow(ri_row) == 0) {
      break
    }
    
    # Check if the loss exceeds the attachment point.
    if (loss_val <= ri_row$`Attachment Pt Amt USD`) {
      # If not, recovery is 0 for this layer and we stop further calculations.
      break
    } else {
      # Calculate the excess loss above the attachment point (adjusted by the actual attachment).
      X <- loss_val - ri_row$`Attachment Pt Amt USD`
      
      # If the loss is less than the cumulative limit for the layer...
      if (loss_val < ri_row$`Cumulative Amt USD`) {
        # ...calculate recovery for this layer and stop.
        recovery_layer <- X * ri_row$`Reins Share`
        total_recovery <- total_recovery + recovery_layer
        break
      } else {
        # If loss is greater than or equal to the cumulative limit,
        # calculate the recovery up to the cumulative limit.
        recovery_layer <- (ri_row$`Cumulative Amt USD` - ri_row$`Attachment Pt Act USD`) * ri_row$`Reins Share`
        total_recovery <- total_recovery + recovery_layer
        
        # Move on to the next layer.
        current_layer <- current_layer + 1
      }
    }
  }
  recovery <- append(recovery, total_recovery)
  Year <- append(Year, year_val)
  Policy <- append(Policy, arr_id)
  Loss <- append(Loss, loss_val)
  Loss_ID <- append(Loss_ID, loss_id)
}

# Print the final results data frame.
groupedRecoveries <- data.frame("Recovery" = recovery, 
                                "Year" = Year,
                                "Policy" = Policy,
                                "Loss" = Loss,
                                "Loss_ID" = Loss_ID)

#Group the reinsurance by reinsurance policy. effectively sums up each reinsurance towers premiums into a total.
groupedReinsurance <- aggregate(`Reins Premium` ~ `Arr ID`, data = reinsuranceData, FUN = sum)

#sum reinsurance losses by unique key that represents a specific re-insurance policy in a specific year. e.g. if a hospital has more than
# 1 loss in a year combine them into 1 row.
groupedRecoveries$key <- paste0(groupedRecoveries$Policy, groupedRecoveries$Year)
groupedRecoveries <- aggregate(`Recovery` ~ `key`, data = groupedRecoveries, FUN = sum)
groupedRecoveries$Policy <- substr(groupedRecoveries$key,1, (nchar(groupedRecoveries$key) -4))
groupedRecoveries$Year <- substr(groupedRecoveries$key,(nchar(groupedRecoveries$key) -3), nchar(groupedRecoveries$key))

#Build out simulated Reinsurance premiums until 2014 by taking the grouped reinsurance premiums from 2023 and applying the detrending factors for each year to 
#adjust them for inflation.

# Define the years (from 2023 down to 2014) and their corresponding de-trending factors
factors <- c(1, 0.9130, 0.8349, 0.8231, 0.8081, 0.7816, 0.771, 0.7660, 0.7582, 0.7505)
years <- c()
reinsPrem <- c()
Policy <- c()

for(i in 2023:2014){
  years <- append(years,rep(i,nrow(groupedReinsurance)))
  reinsPrem <- append(reinsPrem, groupedReinsurance$`Reins Premium` *factors[2024-i])
  Policy <- append(Policy, groupedReinsurance$`Arr ID`)
}


groupedReinsurance <- data.frame( "key" = paste0(Policy,years),
                                  "Year" = years, 
                                  "Policy" = Policy,
                                  "Reins Premium" = reinsPrem)

#combine the Recover and loss data into single data frame.
groupedReinsurance <- merge(groupedReinsurance, groupedRecoveries[,c("key", "Recovery")], by = "key", all.x = TRUE)

#Export as excel file for further analysis 
write_xlsx(groupedReinsurance, "~/Desktop/Recovery.xlsx")

