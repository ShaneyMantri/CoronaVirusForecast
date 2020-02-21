rxnrate=function(t,c,parms){
    
    # rate constant passed through a list called parms
    k1=parms$k1
    k2=parms$k2
    
    # c is the concentration of species
    
    # derivatives dc/dt are computed below
    r=rep(0,length(c))
    r[1]=-k1*c["A"]*c["B"] #dcA/dt
    r[2]=k1*c["A"]*c["B"]-k2*c["B"] #dcB/dt
    r[3]=k2*c["B"] #dcC/dt
    
    # the computed derivatives are returned as a list
    # order of derivatives needs to be the same as the order of species in c
    return(list(r))
    
}
ssq=function(parms){
    
    # inital concentration
    cinit=c(A=1419998860,B=639,C=48)
    # time points for which conc is reported
    # include the points where data is available
    t=seq(0,9)
    #t=sort(unique(t))
    # parameters from the parameter estimation routine
    k1=parms[1]
    k2=parms[2]
    # solve ODE for a given set of parameters
    out=ode(y=cinit,times=t,func=rxnrate,parms=list(k1=k1,k2=k2))
    
    # Filter data that contains time points where data is available
    outdf=data.frame(out)
    #outdf=outdf[outdf$time %in% chinaf$Time,]
    # Evaluate predicted vs experimental residual
    preddf=melt(outdf,id.var="time",variable.name="species",value.name="conc")
    expdf=melt(chinaf,id.var="Time",variable.name="species",value.name="conc")
    ssqres=preddf$conc-expdf$conc
    
    # return predicted vs experimental residual
    return(ssqres)
    
}