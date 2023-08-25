library(data.table)
library(foreach)

#### BEST MEASURE WITH REGRESSIONS
best_measure_DT_reg = function(d_test_ind, 
                               # interval used for test data
                               train_dates, 
                               # interval of training data
                               end_dates,
                               method = "glm", return_mods = F, 
                               # you’ll have to construct the formula based on the requested 
                               # outcome + indicators
                               form, mtry = 1) {
  
  # loop over outcome dates
  out = foreach(i=1:length(end_dates), .combine = rbind) %dopar% {
    
    # run over recent past period
    d = 21

    # pull training data
    d_train = d_test_ind[ymd >= train_dates & ymd <= end_dates, ]
    
    # pull test data
    d_test = d_test_ind[ymd > end_dates, ]
    
    # make a list of outcomes
    outcomes = unlist(unique(d_test_ind[,"outcome_label"]))
    
    # length of outcomes
    len = length(outcomes)
    
    # loop over outcomes
    for(j in 1:len){
      
      # check that there are test data  
      if(nrow(d_test[outcome_label==outcomes[j]])>0){
        
        # fit model
        lm = glm(form, 
                 data = d_train[outcome_label==outcomes[j]], 
                 family = binomial(), 
                 weights = weight)
        
        # make predictions
        pred_temp = predict(lm, newdata = d_test[outcome_label==outcomes[j]], type = "response")
        
        # store prediction
        d_test[outcome_label==outcomes[j], "pred" := .(pred_temp)]
        
        # store other outputs
        # value is the predicted binary based on risk preferences
        d_test[, c("value", "var", "min_train_date", "max_train_date", "test_date", "weeks") := 
                 .(pred >= (1/(1+mult)), "Smoothed", 
                   as.Date(min(unlist(d_train[,"ymd"])), origin = "1970-01-01"),  as.Date(max(unlist(d_train[,"ymd"])), origin = "1970-01-01"), 
                   as.Date(max(unlist(d_test[,"ymd"])), origin = "1970-01-01"), 
                   nrow(unique(d_test[,"ymd"])))]
      }
    }
    return(d_test)
  }
  
}

# calculate sensitivity and specificity, PPV & NPV
calc_metrics_DT = function(dt, 
                           group_vars = c("var", "case_levels", "admit_levels", "perc_levels", "outcome_label")){
  
  # calculate multiplier
  dt = dt[,a:=(mult-1)/(1+mult)]
  
  # calculate metrics
  dt = dt[, .(sens = sum(weight*(outcome_value & value), na.rm = T)/sum(weight*(outcome_value & !is.na(value)), na.rm = T),
              spec = sum(weight*(!outcome_value & !value), na.rm = T)/sum(weight*(!outcome_value & !is.na(value)), na.rm = T),
              ppv = sum(weight*(outcome_value & value), na.rm = T)/sum(weight*(value & !is.na(outcome_value)), na.rm = T),
              npv = sum(weight*(!outcome_value & !value), na.rm = T)/sum(weight*(!value & !is.na(outcome_value)), na.rm = T),
              percright = sum(weight*(value==outcome_value), na.rm = T)/sum(weight*(!is.na(value) & !is.na(outcome_value)), na.rm = T),
              n = length(weight),
              n2 = sum(weight),
              fn = sum(weight*(1+a)*(outcome_value & !value), na.rm = T),
              fp = sum(weight*(1-a)*(!outcome_value & value), na.rm = T),
              tn = sum(weight*(!outcome_value & !value), na.rm = T),
              tp = sum(weight*(outcome_value & value), na.rm = T),
              pos = sum(weight*(value), na.rm = T),
              neg = sum(weight*(!value), na.rm = T)), by = group_vars]
  
  # combine into weighted accuracy
  dt = dt[, w_acc:=1-(fn + fp)/(neg+pos)]
  
}



# Create a sample input dataframe
input_data <- data.frame(
  ymd = as.Date(c("2023-03-01", "2023-03-02", "2023-03-03", "2023-03-04", "2023-03-05", "2023-03-06", "2023-03-07", "2023-03-08")),
  deaths_avg_per_100k = c(0.5, 1.2, 1.8, 1.5, 1.7, 1.8, NA, NA),
  location = c("NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY"),
  cases_weekly = c(20, 15, 25, 30, 18, 25, 29, 17),
  admits_weekly = c(12, 8, 10, 14, 9, 7, 10, 15),
  perc_covid_100 = c(8.5, 9.8, 11.2, 7.3, 6.9, 5.1, 6.2, 7.3),
  outcome_label = c("deaths_over", "deaths_over", "deaths_over", "deaths_over", "deaths_over", "deaths_over", "deaths_over", "deaths_over"),
  weight = c(0.9, 0.8, 0.85, 0.95, 0.88, .9, .85, .88)
) %>% mutate(deaths_over = deaths_avg_per_100k>10)


d_test_ind <- data.table(input_data)
d_test_ind[,mult:=1/2] # mult column = weight

# training dates
start <- as.Date("2023-03-01")
end <- as.Date("2023-03-06")

# create formula
formula <-  deaths_over ~ perc_covid_100 + admits_weekly + cases_weekly

# run model
result <- best_measure_DT_reg(d_test_ind = d_test_ind, end_dates = end, train_dates = start, form = formula)

# run sample dataset
sample <- read.csv("~/Downloads/sample dataset - Sheet1.csv")
sample <- as.data.table(sample)
sample[,mult:=1/2]
sample[,outcome_label:="deaths_over"]
sample[,deaths_over:= deaths_avg_per_100k > 1]
sampleStart <- as.Date("2022-01-01")
sampleEnd <- as.Date("2022-01-06")
result2 <- best_measure_DT_reg(sample, train_dates = sampleStart, end_dates = sampleEnd, form = formula)
