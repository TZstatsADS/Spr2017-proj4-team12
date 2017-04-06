
source("../lib/data_cleaning.r")

filename <- c("AGupta", "AKumar", "CChen", "DJohnson", "JLee", "JMartin", 
              "JRobinson", "JSmith", "KTanaka", "MBrown", "MJones", "MMiller",
              "SLee", "YChen")

for (i in 1:length(filename)){
  data_cleaning(filename = filename[i])
}