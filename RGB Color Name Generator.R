
### RGB Database Generator ###

# result sample :
# R , G , B , white
# 255 , 255, 255 , white


### Create 3 pairs of color (R, G, B)
### Cross join R G B (255*255*255)

boundary <- (255+1)^3

R.list <- list(rep(0:255, each = 256^2))

G.round <- ((255+1)^2)/256
G.list <- list(rep(0:255, each = 256, times = G.round))

B.round <- ((255+1)^3)/256
B.list <- list(rep(0:255, B.round))



# Get time for first loop 
loop1.before <- Sys.time()

t.1 <- NA

# Pre-allocate output variable as vector (it helps speed up loop)
RGB_con <- vector(mode = "list", boundary)

# Concatiate R, G, B variable as vector
for (i in 1:length(RGB_con)) {
  
  # Combine RGB (this will speed up loop)
  RGB_con[[i]] <- c(R.list[[c(1,i)]], G.list[[c(1,i)]], B.list[[c(1,i)]])
  
  
  # Get iteration loop per minute (1 = 1 min)
  if(difftime(Sys.time(), loop1.before, units = "min") <= 1){loop1.lpm <- i}
  
  # Print status
  cat("\r", "Concating:", i, "/", length(RGB_con),  "Duration =", t.1, "hr(s)", loop1.lpm, "LPM")
  
  
  # Get time remaining // "i" changes during loop
  t.1 <- round((((length(RGB_con)-i)/loop1.lpm)/60), digits = 1)
  
}

# Calculate first loop time
loop1.after <- difftime(Sys.time(), loop1.before, units = "min")

print(paste0(as.numeric(loop1.after), " min(s) of operation"))



# Get time before second loop 
loop2.before <- Sys.time()

t.2 <- NA

# Pre-allocate output variable as vector (it helps speed up loop)
colorname_vec <- character(length(RGB_con))

## Loop add color name to dataframe based on RGB code
for (i in 1:length(colorname_vec)) { 
  
  
  # Get color name from r base 
  # Thanks to Henrique Dallazuanna
  colorname_vec[i] <- colors()[which.min(colSums(col2rgb(colors()) - RGB_con[[i]])[colSums(col2rgb(colors()) - RGB_con[[i]]) >= 0])]
  
 
  # Get iteration loop per minute (1 = 1min)
  if(difftime(Sys.time(), loop2.before, units = "min") <= 1){loop2.lpm <- i}
  
  # Print status
  cat("\r", "Labeling:", i, "/", length(colorname_vec), "Duration =", t.2, "hr(s)", loop2.lpm, "LPM")
  
  
  # Get time remaining // "i" changes during loop
  t.2 <- round((((length(colorname_vec)-i)/loop2.lpm)/60), digits = 1)
  
}

# Calculate second loop time
loop2.after <- as.numeric(difftime(Sys.time(), loop2.before, units = "min"))

print(paste0(as.numeric(loop2.after), " min(s) of operation"))



# Overall time used
loop.alltime <- loop1.after + loop2.after

print(paste0("Containate time spent : ", loop1.after, " mins"))
print(paste0("Labeling time spent : ", loop2.after, " mins"))
print(paste0("Overall time spent : ", loop.alltime, " mins"))




