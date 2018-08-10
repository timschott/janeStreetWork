start_df <- read.csv("OrderMarkingExercise.csv",stringsAsFactors=F)
#1433 entries. 
colnames(start_df) <- c("Time", "Account", "Dir", "Qty", "SecCode", "Price", "Currency")
#the other columns are excessive let's get rid of them
start_df <- subset(start_df, select=c("Time", "Dir", "Qty"))

#make tiny DF in order to test my hypothesis

test_df <- start_df[-c(16:1433),]

#artificially mess some up 
test_df$Dir[3]<-"Sell_short"
test_df$Dir[11]<-"Sell_long"

#function time 
test_results <- rep_len("Buy", 15)
inventory<-0

for(i in 1:15){
  if(test_df$Dir[i]=="Buy"){
    inventory<-inventory+test_df$Qty[i]
    #print(inventory)
  }
  else if(test_df$Dir[i] != "Buy"){
    #cat("the inventory is", inventory)
    #cat(". The inventory less the qty is", inventory-test_df$Qty[i])
    if((inventory-test_df$Qty[i])<0){
      inventory<-inventory-test_df$Qty[i]
      #print("Short!")
      test_results[i]<-"Sell_short"
    }
    else{
      inventory<-inventory-test_df$Qty[i]
      test_results[i]<-"Sell_long"
      #print("Long!")
    }
  } 
}
test_log=!logical(length=15)
#now let's compare test_results to $Dir..
#indices of differing vals. 
differ<-c(which(test_df$Dir != test_results))

for(i in 1:length(differ)){
  if(differ[i] %in% 1:length(test_log)){
    test_log[differ[i]] = FALSE
  }
}
test_df$corrrect<-test_log
#c o o l now time to scale. 

actual_df<-start_df
length(actual_df$Dir)

actual_inventory<-0
results <- rep_len("Buy", 1433)
for(i in 1:1433){
  if(actual_df$Dir[i]=="Buy"){
    actual_inventory<-actual_inventory +actual_df$Qty[i]
  }
  else if(actual_df$Dir[i] != "Buy"){
    if((actual_inventory-actual_df$Qty[i])<0){
      actual_inventory<-actual_inventory- actual_df$Qty[i]
      results[i]<-"Sell_short"
    }
    else{
      actual_inventory<-actual_inventory- actual_df$Qty[i]
      results[i]<-"Sell_long"
    }
  } 
}
log=!logical(length=1433)
differ<-c(which(actual_df$Dir != results)) #bad indices

for(i in 1:length(differ)){
  if(differ[i] %in% 1:length(log)){
    log[differ[i]] = FALSE #fill them with false
  }
}

actual_df$Valid<-log

write.csv(actual_df, file="complete.csv")

#The incorrect indices are contained in "differ," there are 19 of them:
# 94  259  337  338  339  423  504  673  769  797  881  882 1292 1301 1303 1399 1431
# 1432 1433
