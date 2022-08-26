# Hospital Discharge Forecast

## First Step
Discharge data found on NHS website was just for April, May and June. This would not be enough to run a time series model.
So the we need to look for a data that correlates with Discharge data. I experimented on Bed Occupancy (night and Day), and A&E data
Bed Occupancy for night was best amongst all as I had about 0.71 corelation.
This can be found on the surge branch in data-raw. correlation test

## Second Step
This step involved gathering bed occupancy from  the first quater of 2011 to the fourth quater of 2021.
Ran an arima model on the sum of all trusts (percentage) and then ran on all individual trusts.
This can be found on the surge branch in data-raw, arima_model_working_percent.r file
### Things to work
1. [Line 24]  Find a better way to create a for loop
               that will clean and store the bed data with different file format  

 2. [Line 195] Find a better way to extra the quarter names and rename it automaticly
               it will save time when you decided to run your model on partcular quarters 
               different from what we used above

 3. [Line 237] I need to handle the missing data as i loose valuable data that will better 
               train my model
