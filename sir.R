

##CHINA
#Framing SIR Equation
sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
        dS <- -beta * I * S
        dI <-  beta * I * S - gamma * I
        dR <-  gamma * I
        return(list(c(dS, dI, dR)))
    })
}

print()


#Parameter Values
parameters_values <- c(
    beta  = (2.44529*10^-10), # infectious contact rate (/person/day)
    gamma = 0.0125429    # recovery rate (/day)
)


#Initial Values
initial_values <- c(
    S = 1.42*10^9,  # number of susceptibles at time = 0
    I =   278,  # number of infectious at time = 0
    R =   0   # number of recovered (and immune) at time = 0
)

# days
time_values <- seq(0, 12) 


#Solving SIR Equations
sir_values_1 <- ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = parameters_values 
)

sir_values_1 <- as.data.frame(sir_values_1)

print(sir_values_1)
#Plotting Graphs

with(sir_values_1, {
    # plotting the time series of susceptibles:
    plot(time, S, type = "l", col = "blue",
         xlab = "time (days)", ylab = "number of people")
    # adding the time series of infectious:
    lines(time, I, col = "red")
    # adding the time series of recovered:
    lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")
abline(v=which(sir_values_1$I==max(sir_values_1$I)),lwd=2,lty=2)
#Value of R_0
(999 + 1) * parameters_values["beta"] / parameters_values["gamma"]



