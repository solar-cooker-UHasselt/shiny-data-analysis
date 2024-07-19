############################################################
# R shiny to analyse the performance of the solar cooker   #
# according to the protocol ASAE S580.1 JAN03              #
#                                                          #
#                       PART SERVER                        #
#                                                          #
# Author: Jonas Meijerink                                  #
# Date: April 2024                                         #
############################################################

#####################
# Shiny server side #
#####################

server <- function(input, output, session) {
  original1 <- reactive({
    dat_long <- read.csv(
      input$file1$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    return(dat_long)
  })
  
  data1 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$file1meta)
    
    dat_long <- read.csv(
      input$file1$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    message(paste("Test station data processed: "))
    message(paste("Number of records: ", dim(dat_long)[1]))
    
    # Check whether the number of columns is correct
    #------------------------------------------------
    if (dim(dat_long)[2] == 14) {
      message("Correct number of columns")
    }
    if (dim(dat_long)[2] != 14) {
      message("Incorrect number of columns")
    }
    
    # Rename the variables
    #----------------------
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    
    # Create an experiment ID based on timestamp
    #--------------------------------------------
    dat_long$Date <- as.POSIXct(
      paste(
        dat_long$Year,
        dat_long$Month,
        dat_long$Day,
        dat_long$Hour,
        dat_long$Minute,
        dat_long$Second
      ),
      format = "%Y %m %d %H %M %S"
    )
    dat_long$Experiment_ID <- sub("*.csv", "", sub(".*/", "", sub(
      ".csv$", "", basename(input$file1$name)
    )))
    dat_long$Record <- with(dat_long,
                            ave(Experiment_ID, Experiment_ID, FUN = seq_along))
    dat_long$Timestamp <- ymd_hms(with(dat_long, paste(
      paste(Year, Month, Day, sep = "-"),
      paste(Hour, Minute, Second, sep = ":"),
      "UTC"
    )))
    dat_long$Time_diff <- dat_long$Timestamp - dat_long$Timestamp[1]
    dat_long$Time_period <- findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(
      dat_long$Time_diff
    )), 600))
    
    # Calculation of the mean of variables in different time periods
    #----------------------------------------------------------------
    dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] <- NA
    col_ids <- which(colSums(is.na(dat_long[7:14])) == 0) + 6
    means_per_period <- apply(dat_long[, col_ids], 2, function(x) {
      tapply(x, dat_long$Time_period, mean)
    })
    
    # Construct the output dataset
    #------------------------------
    output_dataset <- data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1], 3),
                                 Pot_ID = 1:3)
    
    nr_periods <- length(unique(dat_long$Time_period))
    outdoor_temp_mat <- matrix(nrow = 3, ncol = nr_periods)
    pot_ids <- which(grepl("pot", names(dat_long)))
    
    prev_output_dataset <- output_dataset
    for (period_id in 1:nr_periods) {
      period_names <- paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]), "_P", period_id, sep = "")
      new_output_dataset <- cbind(prev_output_dataset, matrix(
        rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3),
        nrow = 3,
        byrow = F
      ))
      names(new_output_dataset)[(ncol(prev_output_dataset) + 1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] <- period_names
      
      temp_name <- paste("Temp_pot_P", period_id, sep = "")
      temp_vals <- means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
      all_vals <- rep(NA, 3)
      all_vals[1:length(temp_vals)] <- temp_vals
      new_output_dataset <- cbind(new_output_dataset, all_vals)
      names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] <- temp_name
      
      prev_output_dataset <- new_output_dataset
    }
    
    output_dataset <- prev_output_dataset
    
    # Add start and end temperature to output dataset
    #-------------------------------------------------
    output_dataset$Start_temp <- as.numeric(c(
      dat_long$Temp_pot1[1],
      dat_long$Temp_pot2[1],
      dat_long$Temp_pot3[1]
    ))
    output_dataset$End_temp <- as.numeric(c(
      dat_long$Temp_pot1[nrow(dat_long)],
      dat_long$Temp_pot2[nrow(dat_long)],
      dat_long$Temp_pot3[nrow(dat_long)]
    ))
    
    # Add begin and end temperature to each interval
    #----------------------------------------------
    temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods * 3))
    
    colnames(temp_pairs) <- c("Start_temp_pot", "End_temp_pot")
    for (i in 0:2) {
      for (period_id in 1:(nr_periods - 1)) {
        temp_pairs[period_id + i * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == period_id)[1], 11 + i], dat_long[which(dat_long$Time_period == period_id + 1)[1], 11 + i])
      }
      temp_pairs[(i + 1) * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == nr_periods)[1], 11 + i], dat_long[tail(which(dat_long$Time_period == nr_periods), 1), 11 + i])
    }
    
    temp_pairs <- mapply(temp_pairs, FUN = as.numeric)
    
    # Read in the metadata file
    #---------------------------
    
    #############################################################################
    # The METADATA file should include the following information:               #
    # Date (calendar time): "metaData$TestDate"                                 #
    # Volume of water: "metaData$M"                                             #
    # Capacity of water: "metaData$Cv"                                          #
    # Brand of the cooker: "metaData$Cooker"                                    #
    # Type of device: "metaData$Tdevice"                                        #
    # Number of the testing station: "metaData$TestStation"                     #
    # Number of the testing probe: "metaData$TempSensor"                        #
    # Type of cooking vessel: "metaData$Pot"                                    #
    # Person performing the experiment: "metaData$Operator"                     #
    # Latitude of the testing site: "metaData$Latitude"                         #
    # Longitude of the testing site: "metaData$Longitude"                       #
    # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
    # Identification of data from the testing station: "metaData.data$Datafile" #
    # Picture of the device: "metaData$CookerPic"                               #
    #############################################################################
    
    metadata <- read.csv(
      input$file1meta$datapath,
      header = T,
      sep = ";",
      dec = ",",
      encoding = "latin1"
    )
    metadata <- metadata[order(metadata$Datafile, metadata$Pot_ID), ]
    
    # Link the metadata to the teststation data
    #-------------------------------------------
    output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
    output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TempSensor <- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
    
    
    output_dat <- reshape(
      output_dataset,
      direction = "long",
      idvar = "Pot_ID",
      varying = list(
        grep("Temp_pot", names(output_dataset)),
        grep("Outdoor_temp", names(output_dataset)),
        grep("Wind", names(output_dataset)),
        grep("Air", names(output_dataset)),
        grep("Rel", names(output_dataset)),
        grep("Solar", names(output_dataset))
      ),
      v.names = c(
        "Temp_pot",
        "Outdoor_temp",
        "Wind_speed",
        "Air_pressure",
        "Rel_hum",
        "Solar_irr"
      ),
      timevar = "Time_period"
    )
    
    output_dataset <- output_dat[order(output_dat$Pot_ID), ]
    
    
    # add the begin and end temp to this format
    
    return(cbind(output_dataset, temp_pairs))
  })
  
  original2 <- reactive({
    dat_long <- read.csv(
      input$file2$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    return(dat_long)
  })
  
  data2 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    req(input$file2meta)
    
    dat_long <- read.csv(
      input$file2$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    message(paste("Test station data processed: "))
    message(paste("Number of records: ", dim(dat_long)[1]))
    
    # Check whether the number of columns is correct
    #------------------------------------------------
    if (dim(dat_long)[2] == 14) {
      message("Correct number of columns")
    }
    if (dim(dat_long)[2] != 14) {
      message("Incorrect number of columns")
    }
    
    # Rename the variables
    #----------------------
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    
    # Create an experiment ID based on timestamp
    #--------------------------------------------
    dat_long$Date <- dat_long$Date <- as.POSIXct(
      paste(
        dat_long$Year,
        dat_long$Month,
        dat_long$Day,
        dat_long$Hour,
        dat_long$Minute,
        dat_long$Second
      ),
      format = "%Y %m %d %H %M %S"
    )
    dat_long$Experiment_ID <- sub("*.csv", "", sub(".*/", "", sub(
      ".csv$", "", basename(input$file2$name)
    )))
    dat_long$Record <- with(dat_long,
                            ave(Experiment_ID, Experiment_ID, FUN = seq_along))
    dat_long$Timestamp <- ymd_hms(with(dat_long, paste(
      paste(Year, Month, Day, sep = "-"),
      paste(Hour, Minute, Second, sep = ":"),
      "UTC"
    )))
    dat_long$Time_diff <- dat_long$Timestamp - dat_long$Timestamp[1]
    dat_long$Time_period <- findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(
      dat_long$Time_diff
    )), 600))
    
    # Calculation of the mean of variables in different time periods
    #----------------------------------------------------------------
    dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] <- NA
    col_ids <- which(colSums(is.na(dat_long[7:14])) == 0) + 6
    means_per_period <- apply(dat_long[, col_ids], 2, function(x) {
      tapply(x, dat_long$Time_period, mean)
    })
    
    # Construct the output dataset
    #------------------------------
    output_dataset <- data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1], 3),
                                 Pot_ID = 1:3)
    
    nr_periods <- length(unique(dat_long$Time_period))
    outdoor_temp_mat <- matrix(nrow = 3, ncol = nr_periods)
    pot_ids <- which(grepl("pot", names(dat_long)))
    
    prev_output_dataset <- output_dataset
    for (period_id in 1:nr_periods) {
      period_names <- paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]), "_P", period_id, sep = "")
      new_output_dataset <- cbind(prev_output_dataset, matrix(
        rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3),
        nrow = 3,
        byrow = F
      ))
      names(new_output_dataset)[(ncol(prev_output_dataset) + 1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] <- period_names
      
      temp_name <- paste("Temp_pot_P", period_id, sep = "")
      temp_vals <- means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
      all_vals <- rep(NA, 3)
      all_vals[1:length(temp_vals)] <- temp_vals
      new_output_dataset <- cbind(new_output_dataset, all_vals)
      names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] <- temp_name
      
      prev_output_dataset <- new_output_dataset
    }
    
    output_dataset <- prev_output_dataset
    
    # Add start and end temperature to output dataset
    #-------------------------------------------------
    output_dataset$Start_temp <- as.numeric(c(
      dat_long$Temp_pot1[1],
      dat_long$Temp_pot2[1],
      dat_long$Temp_pot3[1]
    ))
    output_dataset$End_temp <- as.numeric(c(
      dat_long$Temp_pot1[nrow(dat_long)],
      dat_long$Temp_pot2[nrow(dat_long)],
      dat_long$Temp_pot3[nrow(dat_long)]
    ))
    
    # Add begin and end temperature to each interval
    #----------------------------------------------
    temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods * 3))
    
    colnames(temp_pairs) <- c("Start_temp_pot", "End_temp_pot")
    for (i in 0:2) {
      for (period_id in 1:(nr_periods - 1)) {
        temp_pairs[period_id + i * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == period_id)[1], 11 + i], dat_long[which(dat_long$Time_period == period_id + 1)[1], 11 + i])
      }
      temp_pairs[(i + 1) * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == nr_periods)[1], 11 + i], dat_long[tail(which(dat_long$Time_period == nr_periods), 1), 11 + i])
    }
    
    temp_pairs <- mapply(temp_pairs, FUN = as.numeric)
    
    # Read in the metadata file
    #---------------------------
    
    #############################################################################
    # The METADATA file should include the following information:               #
    # Date (calendar time): "metaData$TestDate"                                 #
    # Volume of water: "metaData$M"                                             #
    # Capacity of water: "metaData$Cv"                                          #
    # Brand of the cooker: "metaData$Cooker"                                    #
    # Type of device: "metaData$Tdevice"                                        #
    # Number of the testing station: "metaData$TestStation"                     #
    # Number of the testing probe: "metaData$TempSensor"                        #
    # Type of cooking vessel: "metaData$Pot"                                    #
    # Person performing the experiment: "metaData$Operator"                     #
    # Latitude of the testing site: "metaData$Latitude"                         #
    # Longitude of the testing site: "metaData$Longitude"                       #
    # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
    # Identification of data from the testing station: "metaData.data$Datafile" #
    # Picture of the device: "metaData$CookerPic"                               #
    #############################################################################
    
    metadata <- read.csv(
      input$file2meta$datapath,
      header = T,
      sep = ";",
      dec = ",",
      encoding = "latin1"
    )
    metadata <- metadata[order(metadata$Datafile, metadata$Pot_ID), ]
    
    # Link the metadata to the teststation data
    #-------------------------------------------
    output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
    output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TempSensor <- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
    
    
    output_dat <- reshape(
      output_dataset,
      direction = "long",
      idvar = "Pot_ID",
      varying = list(
        grep("Temp_pot", names(output_dataset)),
        grep("Outdoor_temp", names(output_dataset)),
        grep("Wind", names(output_dataset)),
        grep("Air", names(output_dataset)),
        grep("Rel", names(output_dataset)),
        grep("Solar", names(output_dataset))
      ),
      v.names = c(
        "Temp_pot",
        "Outdoor_temp",
        "Wind_speed",
        "Air_pressure",
        "Rel_hum",
        "Solar_irr"
      ),
      timevar = "Time_period"
    )
    
    output_dataset <- output_dat[order(output_dat$Pot_ID), ]
    
    
    # add the begin and end temp to this format
    
    return(cbind(output_dataset, temp_pairs))
  })
  
  original3 <- reactive({
    dat_long <- read.csv(
      input$file3$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    return(dat_long)
  })
  
  data3 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file3)
    req(input$file3meta)
    
    dat_long <- read.csv(
      input$file3$datapath,
      sep = ";",
      header = T,
      dec = ".",
      encoding = "latin1"
    )
    message(paste("Test station data processed: "))
    message(paste("Number of records: ", dim(dat_long)[1]))
    
    # Check whether the number of columns is correct
    #------------------------------------------------
    if (dim(dat_long)[2] == 14) {
      message("Correct number of columns")
    }
    if (dim(dat_long)[2] != 14) {
      message("Incorrect number of columns")
    }
    
    # Rename the variables
    #----------------------
    names(dat_long) <- c(
      "Year",
      "Month",
      "Day",
      "Hour",
      "Minute",
      "Second",
      "Outdoor_temp",
      "Wind_speed",
      "Air_pressure",
      "Rel_hum",
      "Temp_pot1",
      "Temp_pot2",
      "Temp_pot3",
      "Solar_irr"
    )
    
    # Create an experiment ID based on timestamp
    #--------------------------------------------
    dat_long$Date <- dat_long$Date <- as.POSIXct(
      paste(
        dat_long$Year,
        dat_long$Month,
        dat_long$Day,
        dat_long$Hour,
        dat_long$Minute,
        dat_long$Second
      ),
      format = "%Y %m %d %H %M %S"
    )
    dat_long$Experiment_ID <- sub("*.csv", "", sub(".*/", "", sub(
      ".csv$", "", basename(input$file3$name)
    )))
    dat_long$Record <- with(dat_long,
                            ave(Experiment_ID, Experiment_ID, FUN = seq_along))
    dat_long$Timestamp <- ymd_hms(with(dat_long, paste(
      paste(Year, Month, Day, sep = "-"),
      paste(Hour, Minute, Second, sep = ":"),
      "UTC"
    )))
    dat_long$Time_diff <- dat_long$Timestamp - dat_long$Timestamp[1]
    dat_long$Time_period <- findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(
      dat_long$Time_diff
    )), 600))
    
    # Calculation of the mean of variables in different time periods
    #----------------------------------------------------------------
    dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] <- NA
    col_ids <- which(colSums(is.na(dat_long[7:14])) == 0) + 6
    means_per_period <- apply(dat_long[, col_ids], 2, function(x) {
      tapply(x, dat_long$Time_period, mean)
    })
    
    # Construct the output dataset
    #------------------------------
    output_dataset <- data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1], 3),
                                 Pot_ID = 1:3)
    
    nr_periods <- length(unique(dat_long$Time_period))
    outdoor_temp_mat <- matrix(nrow = 3, ncol = nr_periods)
    pot_ids <- which(grepl("pot", names(dat_long)))
    
    prev_output_dataset <- output_dataset
    for (period_id in 1:nr_periods) {
      period_names <- paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]), "_P", period_id, sep = "")
      new_output_dataset <- cbind(prev_output_dataset, matrix(
        rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3),
        nrow = 3,
        byrow = F
      ))
      names(new_output_dataset)[(ncol(prev_output_dataset) + 1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] <- period_names
      
      temp_name <- paste("Temp_pot_P", period_id, sep = "")
      temp_vals <- means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
      all_vals <- rep(NA, 3)
      all_vals[1:length(temp_vals)] <- temp_vals
      new_output_dataset <- cbind(new_output_dataset, all_vals)
      names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] <- temp_name
      
      prev_output_dataset <- new_output_dataset
    }
    
    output_dataset <- prev_output_dataset
    
    # Add start and end temperature to output dataset
    #-------------------------------------------------
    output_dataset$Start_temp <- as.numeric(c(
      dat_long$Temp_pot1[1],
      dat_long$Temp_pot2[1],
      dat_long$Temp_pot3[1]
    ))
    output_dataset$End_temp <- as.numeric(c(
      dat_long$Temp_pot1[nrow(dat_long)],
      dat_long$Temp_pot2[nrow(dat_long)],
      dat_long$Temp_pot3[nrow(dat_long)]
    ))
    
    # Add begin and end temperature to each interval
    #----------------------------------------------
    temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods * 3))
    
    colnames(temp_pairs) <- c("Start_temp_pot", "End_temp_pot")
    for (i in 0:2) {
      for (period_id in 1:(nr_periods - 1)) {
        temp_pairs[period_id + i * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == period_id)[1], 11 + i], dat_long[which(dat_long$Time_period == period_id + 1)[1], 11 + i])
      }
      temp_pairs[(i + 1) * nr_periods, ] <- c(dat_long[which(dat_long$Time_period == nr_periods)[1], 11 + i], dat_long[tail(which(dat_long$Time_period == nr_periods), 1), 11 + i])
    }
    
    temp_pairs <- mapply(temp_pairs, FUN = as.numeric)
    
    # Read in the metadata file
    #---------------------------
    
    #############################################################################
    # The METADATA file should include the following information:               #
    # Date (calendar time): "metaData$TestDate"                                 #
    # Volume of water: "metaData$M"                                             #
    # Capacity of water: "metaData$Cv"                                          #
    # Brand of the cooker: "metaData$Cooker"                                    #
    # Type of device: "metaData$Tdevice"                                        #
    # Number of the testing station: "metaData$TestStation"                     #
    # Number of the testing probe: "metaData$TempSensor"                        #
    # Type of cooking vessel: "metaData$Pot"                                    #
    # Person performing the experiment: "metaData$Operator"                     #
    # Latitude of the testing site: "metaData$Latitude"                         #
    # Longitude of the testing site: "metaData$Longitude"                       #
    # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
    # Identification of data from the testing station: "metaData.data$Datafile" #
    # Picture of the device: "metaData$CookerPic"                               #
    #############################################################################
    
    metadata <- read.csv(
      input$file3meta$datapath,
      header = T,
      sep = ";",
      dec = ",",
      encoding = "latin1"
    )
    metadata <- metadata[order(metadata$Datafile, metadata$Pot_ID), ]
    
    # Link the metadata to the teststation data
    #-------------------------------------------
    output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
    output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$TempSensor <- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
    output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
    
    
    output_dat <- reshape(
      output_dataset,
      direction = "long",
      idvar = "Pot_ID",
      varying = list(
        grep("Temp_pot", names(output_dataset)),
        grep("Outdoor_temp", names(output_dataset)),
        grep("Wind", names(output_dataset)),
        grep("Air", names(output_dataset)),
        grep("Rel", names(output_dataset)),
        grep("Solar", names(output_dataset))
      ),
      v.names = c(
        "Temp_pot",
        "Outdoor_temp",
        "Wind_speed",
        "Air_pressure",
        "Rel_hum",
        "Solar_irr"
      ),
      timevar = "Time_period"
    )
    
    output_dataset <- output_dat[order(output_dat$Pot_ID), ]
    
    
    # add the begin and end temp to this format
    
    return(cbind(output_dataset, temp_pairs))
  })
  
  # Downloadable csv of selected dataset
  #-------------------------------------------------------------------------------
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(sub(".csv$", "", basename(input$file1$name)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(sub(".csv$", "", basename(input$file2$name)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste(sub(".csv$", "", basename(input$file3$name)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data3(), file, row.names = FALSE)
    }
  )
  
  output$template_meta <- downloadHandler(
    filename = function() {
      "PEPUNILU2024XXXXXX.csv"
    },
    content = function(file) {
      file.copy("www/PEPUNILU2024XXXXXX.csv", file)
    }
  )
  
  # Make some graphs for data exploration.
  #-------------------------------------------------------------------------------
  output$Plot11 <- renderPlot({
    req(input$file1)
    
    data <- original1() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    data$Temp_pot1[data$Temp_pot1 == "x"] <- as.numeric(0.001)
    data$Temp_pot2[data$Temp_pot2 == "x"] <- as.numeric(0.001)
    data$Temp_pot3[data$Temp_pot3 == "x"] <- as.numeric(0.001)
    
    data$Temp_pot1 <- as.numeric(data$Temp_pot1)
    data$Temp_pot2 <- as.numeric(data$Temp_pot2)
    data$Temp_pot3 <- as.numeric(data$Temp_pot3)
    
    # Plot temperature vs. time
    ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
      geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1
      geom_line(aes(y = Temp_pot2, color = "Temp_pot2"), show.legend = TRUE) + # Add Temp_pot2
      geom_line(aes(y = Temp_pot3, color = "Temp_pot3"), show.legend = TRUE) + # Add Temp_pot3
      geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
      labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_y_continuous() +
      scale_color_manual(
        values = c("black", "red", "blue", "purple"),
        labels = c(
          "Outdoor temperature",
          "Temperature in pot 1",
          "Temperature in pot 2",
          "Temperature in pot 3"
        )
      ) # Define colors and labels
  })
  
  output$Plot12 <- renderPlot({
    req(input$file1)
    
    data <- original1() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
      geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 100, name = "Wind Speed (m/s)")) +
      labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#FFFF66", "#99CCFF"),
        labels = c("Solar Irradiance", "Wind Speed")
      ) # Define colors and labels
  })
  
  output$Plot13 <- renderPlot({
    req(input$file1)
    
    data <- original1() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
      geom_point(aes(y = Rel_hum * 20, color = "Relative Humidity"),
                 show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 20, name = "Relative Humidity (%)")) +
      labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#CCFFFF", "#009900"),
        labels = c("Air Pressure", "Relative Humidity")
      ) # Define colors and labels
  })
  
  output$Plot21 <- renderPlot({
    req(input$file2)
    
    data <- original2() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    data$Temp_pot1[data$Temp_pot1 == "x"] <- as.numeric(0.001)
    data$Temp_pot2[data$Temp_pot2 == "x"] <- as.numeric(0.001)
    data$Temp_pot3[data$Temp_pot3 == "x"] <- as.numeric(0.001)
    
    data$Temp_pot1 <- as.numeric(data$Temp_pot1)
    data$Temp_pot2 <- as.numeric(data$Temp_pot2)
    data$Temp_pot3 <- as.numeric(data$Temp_pot3)
    
    # Plot temperature vs. time
    ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
      geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1
      geom_line(aes(y = Temp_pot2, color = "Temp_pot2"), show.legend = TRUE) + # Add Temp_pot2
      geom_line(aes(y = Temp_pot3, color = "Temp_pot3"), show.legend = TRUE) + # Add Temp_pot3
      geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
      labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_y_continuous() +
      scale_color_manual(
        values = c("black", "red", "blue", "purple"),
        labels = c(
          "Outdoor temperature",
          "Temperature in pot 1",
          "Temperature in pot 2",
          "Temperature in pot 3"
        )
      ) # Define colors and labels
  })
  
  output$Plot22 <- renderPlot({
    req(input$file2)
    
    data <- original2() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
      geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 100, name = "Wind Speed (m/s)")) +
      labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#FFFF66", "#99CCFF"),
        labels = c("Solar Irradiance", "Wind Speed")
      ) # Define colors and labels
  })
  
  output$Plot23 <- renderPlot({
    req(input$file2)
    
    data <- original2() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
      geom_point(aes(y = Rel_hum * 20, color = "Relative Humidity"),
                 show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 20, name = "Relative Humidity (%)")) +
      labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#CCFFFF", "#009900"),
        labels = c("Air Pressure", "Relative Humidity")
      ) # Define colors and labels
  })
  
  output$Plot31 <- renderPlot({
    req(input$file3)
    
    data <- original3() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    data$Temp_pot1[data$Temp_pot1 == "x"] <- as.numeric(0.001)
    data$Temp_pot2[data$Temp_pot2 == "x"] <- as.numeric(0.001)
    data$Temp_pot3[data$Temp_pot3 == "x"] <- as.numeric(0.001)
    
    data$Temp_pot1 <- as.numeric(data$Temp_pot1)
    data$Temp_pot2 <- as.numeric(data$Temp_pot2)
    data$Temp_pot3 <- as.numeric(data$Temp_pot3)
    
    # Plot temperature vs. time
    ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
      geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1
      geom_line(aes(y = Temp_pot2, color = "Temp_pot2"), show.legend = TRUE) + # Add Temp_pot2
      geom_line(aes(y = Temp_pot3, color = "Temp_pot3"), show.legend = TRUE) + # Add Temp_pot3
      geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
      labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_y_continuous() +
      scale_color_manual(
        values = c("black", "red", "blue", "purple"),
        labels = c(
          "Outdoor temperature",
          "Temperature in pot 1",
          "Temperature in pot 2",
          "Temperature in pot 3"
        )
      ) # Define colors and labels
  })
  
  output$Plot32 <- renderPlot({
    req(input$file3)
    
    data <- original3() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
      geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 100, name = "Wind Speed (m/s)")) +
      labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#FFFF66", "#99CCFF"),
        labels = c("Solar Irradiance", "Wind Speed")
      ) # Define colors and labels
  })
  
  output$Plot33 <- renderPlot({
    req(input$file3)
    
    data <- original3() %>%
      mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format = "%Y %m %d %H %M %S"))
    
    ggplot(data, aes(x = datetime)) +
      geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
      geom_point(aes(y = Rel_hum * 20, color = "Relative Humidity"),
                 show.legend = TRUE) +
      scale_y_continuous(sec.axis = sec_axis( ~ . / 20, name = "Relative Humidity (%)")) +
      labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") + # Adjust the breaks and labels as needed
      scale_color_manual(
        values = c("#CCFFFF", "#009900"),
        labels = c("Air Pressure", "Relative Humidity")
      ) # Define colors and labels
  })
  
  output$htmloutput <- renderUI({
    tags$iframe(src = "protocol.html", style = "width:100%; height:2200px; border:none;")
  })
  
  # Single measure of performance (combining data + standardising + linear reg)
  #-----------------------------------------------------------------------------
  output$output_text1 <- renderText({
    completedata <- rbind(data1(), data2(), data3())
    
    # select only the data of pot 1
    completedata <- completedata[completedata$Pot_ID == 1, ]
    
    N <- nrow(completedata)
    
    # Calculate all the variables described in the protocol.
    completedata <- completedata %>%
      mutate(Td_temp_diff = Temp_pot - Outdoor_temp) %>%
      mutate(Pi_cooking_power = ((End_temp_pot - Start_temp_pot) * M * Cv) / 600) %>%
      mutate(Ps_std_cooking_power = Pi_cooking_power * (700 / Solar_irr))
    
    # standardise according to the protocol
    completedata <- completedata %>%
      filter(Wind_speed <= 2.5) %>%
      filter(Outdoor_temp >= 20 & Outdoor_temp <= 35) %>%
      filter(Solar_irr >= 450 & Solar_irr <= 1100) %>%
      filter(Ps_std_cooking_power >= 0)
    
    # build a linear model
    linear_model <- lm(Ps_std_cooking_power ~ Td_temp_diff, data = completedata)
    
    summary(linear_model)
    new <- data.frame(Td_temp_diff = c(50))
    output <- round(unlist(predict(linear_model, new, se.fit = TRUE)[1]), 2)
    
    paste(
      "In this report the testing has been described of the prototype",
      unique(completedata$Cooker),
      "on",
      data1()$TestDate[1],
      "The objective of the test was to evaluate the performance of this prototype.
        The results were analysed to determine the effectiveness of the solar cooker according to the ASAE Standard S-580.1 .",
      "The value for the standardized cooking power was",
      output,
      "W. It is computed for a temperature difference of 50 °C using the regression relationship found.
          A plot of the relationship between standardized cooking power and temperature difference is shown. A total of",
      nrow(completedata),
      "intervals of 10 minutes were used in the analysis and ",
      N - nrow(completedata),
      "intervals were removed according to the protocol."
    )
  })
  
  output$Plotlinearreg <- renderPlot({
    completedata <- rbind(data1()[-nrow(data1()), ], data2()[-nrow(data2()), ], data3()[-nrow(data3()), ])
    
    # Calculate all the variables described in the protocol.
    completedata <- completedata %>%
      mutate(Td_temp_diff = Temp_pot - Outdoor_temp) %>%
      mutate(Pi_cooking_power = ((End_temp_pot - Start_temp_pot) * M * Cv) / 600) %>%
      mutate(Ps_std_cooking_power = Pi_cooking_power * (700 / Solar_irr))
    
    # standardise according to the protocol
    completedata <- completedata %>%
      filter(Wind_speed <= 2.5) %>%
      filter(Outdoor_temp >= 20 & Outdoor_temp <= 35) %>%
      filter(Solar_irr >= 450 & Solar_irr <= 1100) %>%
      filter(Ps_std_cooking_power >= 0)
    
    # select only the data of pot 1
    completedata <- completedata[completedata$Pot_ID == 1, ]
    
    linear_model <- lm(Ps_std_cooking_power ~ Td_temp_diff, data = completedata)
    
    ggplot(data = completedata, aes(x = Td_temp_diff, y = Ps_std_cooking_power)) +
      geom_point() +
      labs(title = "", x = "Temperature Difference (°C)", y = "Standardised Cooking Power") +
      geom_smooth(method = "lm",
                  se = TRUE,
                  color = "blue")
  })
  
  output$logo_uhasselt <- renderImage({
    list(src = "www/uhasselt-standaard-wit.png",
         width = "85%",
         style = "left: -10%; transform: translateX(10%) translateY(5%);")
  }, deleteFile = F)
  
  
  # add notifications to make it more user friendly
  #-------------------------------------------------------------------------------
  
  observe({
    if (!is.null(input$file1) &&
        !is.null(input$file2) &&
        !is.null(input$file3) &&
        !is.null(input$file1meta) &&
        !is.null(input$file2meta) && !is.null(input$file3meta)) {
      # Proceed with processing
      # Example: Read uploaded files
      # file1_data <- read.csv(input$file1$datapath)
      # file2_data <- read.csv(input$file2$datapath)
      # file3_data <- read.csv(input$file3$datapath)
      output$statusperf <- renderText({
        showNotification("All CSV files are uploaded!",
                         type = "warning",
                         duration = 5)
      })
    } else {
      showNotification("Upload the CSV files first!",
                       type = "warning",
                       duration = NULL)
    }
  })
}
