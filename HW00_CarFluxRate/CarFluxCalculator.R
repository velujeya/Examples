fluxCal <- function(minSpeed, maxSpeed){
  # Car Flux calculation
  #Car velocity miles/hour = v
  loop <- 1
  CarFluxArray <- c()
  CarVelocityArray <- c()
  
  #Minimum car speed
  #minSpeed <- 1
  
  #Maximum car speed
  #maxSpeed <- 120
  for (v in minSpeed :maxSpeed) {
    
    #Safety distance = d 
    #Time take a human to respond = 4 seconds
    timeToRespond <- 4.0 
    #Time take any car to stop when it going on a 
    #certain velocity in miles per hour v in seconds 
    alpha <- 0.0055
    timeToStop <- alpha * (v^2)
    
    #Average Car Length
    carLength <- 10.0
    
    #miles to feet conversation rate
    conversionRate <- 5280.0
    
    # the total distance per car needed for stopping in foot
    totalDistancePerCar <- carLength + v * (timeToRespond +timeToStop)* conversionRate *(1/60)*(1/60)
    
    # Car Desensity per mile is
    carDensity <- 1/(totalDistancePerCar/conversionRate)
    
    # the flux for Car is
    carFlux <- carDensity * v
    
    #carFluxMatrix <-matrix(nrow=dim(v)[1],
    #ncol=dim(carFlux)[2])
    CarFluxArray[loop] <- carFlux
    CarVelocityArray[loop] <- v
    loop <- loop + 1
  } 
  
  #CarDataFrame with flux and speed
  CarDataFrame  = data.frame(CarFluxArray,CarVelocityArray)
  
  #Find the index for maximum car flux
  maxCarFluxIndex <- which.max(CarFluxArray)
  
  #Find the relevant car speed for maximum car flux
  optimumCarSpeed  <- CarVelocityArray[maxCarFluxIndex]
  
  #Find the maximum car flux
  maxflux <- CarFluxArray [maxCarFluxIndex]
  
  #Plotting the data frame
  p <- ggplot(data=CarDataFrame, aes(x=CarVelocityArray, y=CarFluxArray, group=1)) +
    geom_line(color="red")+
    geom_point() +
    geom_segment(aes(x=optimumCarSpeed, y=0, xend=optimumCarSpeed, yend=maxflux), color ="green", linetype=2) +
    geom_segment(aes(x=0, y=maxflux, xend=optimumCarSpeed, yend=maxflux),color ="green", linetype=2) 
  
  p+xlab("Speed (mph)") + ylab("Flux (car density x mph), or Cars per hour") 
  ggtitle("Road Efficency as a Function of Speed.") +
    theme(plot.title = element_text(family="Menlo-Bold", color = "black", size=14, face="bold", hjust=0.5))
  
  
}

install.packages(HW00_CarFluxRate)
fluxCal(0,120)


