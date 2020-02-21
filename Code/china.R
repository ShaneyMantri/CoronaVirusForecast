
today = Sys.Date()
today.ch = as.character(today)

rm(data_path, plot_path, csv_file, dat, model, forecast.df, last.t, latest.date)

home = getwd()
cat("home : ", home)
data_path = paste(home, "/", sep="")
plot_path = paste(home,  "/", sep="")

csv_file = "auth.xlsx"
full_path = paste(data_path, csv_file, sep="")
cat("csv full path : ", full_path)


auth = read.xlsx("auth.xlsx", sheetIndex = 0)
dat[is.na(dat)]=0
dat = auth[9:nrow(auth),]
dat = dat[,c(1,3)]
dat[,"t"] = 1:nrow(dat)
names(dat)[1]="Date.of.Record"
names(dat)[2]="Confirmed"
init_intcpt = min(dat$Confirmed) / 2
model0 = lm(log(Confirmed - init_intcpt)~t, data=dat)  
alpha0 = exp(coef(model0)[1])
beta0 = coef(model0)[2]
init_param = list(alpha=alpha0, beta=beta0, intcpt=init_intcpt)


model = nls(Confirmed ~ alpha*exp(beta*t) + intcpt , data=dat, start=init_param)
res = summary(model)



latest.date = as.character(dat[nrow(dat), "Date.of.Record"])

if ( !exists("Model_param") ) Model_param = list()
Model_param[[length(Model_param)+1]] = res

if ( !exists("Daily_updates") ) Daily_updates = NULL
Daily_updates = rbind(Daily_updates, res$coefficients[,1])
rownames(Daily_updates)[nrow(Daily_updates)] = latest.date


last.t = tail(dat[,"t"],1)
forecast.df = data.frame(matrix(NA, nrow=3, ncol=ncol(dat)))
colnames(forecast.df) = colnames(dat)
dte = dat[nrow(dat), "Date.of.Record"] + 1:3
forecast.df[1:3,"Date.of.Record"] = dte
forecast.df[,"Date.of.Record"] = as.Date(forecast.df[,"Date.of.Record"], origin="1970-01-01")
forecast.df[,"t"] = last.t + 1:3

pred = predict(model, newdata=forecast.df, se.fit = TRUE, level=0.95)
forecast.df[,"Confirmed"] = pred
df = rbind(dat, forecast.df)


to_predict = as.Date(latest.date)+1
fnm = paste("daily-prediction-", to_predict, ".png",  sep="")
full_path = paste(plot_path, fnm, sep="")
png(full_path)
plot(df[,"t"], df[,"Confirmed"], type="b", 
     xlab="Date", ylab="No of confirmed cases",
     main="Coronvirus Confirmed Cases and Predictions")
abline(v=last.t, col="grey", lwd=2)
grid(NA, 10, lwd = 2)
text(x=last.t, y=100, labels=to_predict, pos=3, col="blue", cex=1)
txt = c(paste(as.character(forecast.df[1,"Date.of.Record"]), round(forecast.df[1,"Confirmed"],0), sep=" : "), 
        paste(as.character(forecast.df[2,"Date.of.Record"]), round(forecast.df[2,"Confirmed"],0), sep=" : "))
hgt = max(df[,"Confirmed"])
text(x=1, y=c(hgt*0.9, hgt*0.8), labels=txt, pos=4, col="red", cex=2)
dev.off()

file.copy(from=full_path, to=home)
file.rename(from=file.path(home, fnm), to=file.path(home, "latest-prediction.png"))

fnm = paste("Beta-history.png",  sep="")
full_path = paste(plot_path, fnm, sep="")
png(full_path, width=1080, height=480)
par(mfrow=c(1,3))
dte = as.Date(rownames(Daily_updates))
plot(dte, Daily_updates[,"beta"], type="b", cex=5, bg="red", col="black", lwd=3, pch=21,
     xlab="Date", ylab="beta",
     main="Rate of Infection (Beta)")
grid(NA, 10, lwd = 1)
plot(dte, Daily_updates[,"alpha"], type="b",  cex=5, bg="blue", col="black", lwd=3, pch=22,
     xlab="Date", ylab="alpha",
     main="Alpha")
grid(NA, 10, lwd = 1)
plot(dte, Daily_updates[,"intcpt"], type="b",  cex=5, bg="green", col="black", lwd=3, pch=25,
     xlab="Date", ylab="Intercept",
     main="Intercept")
grid(NA, 10, lwd = 1)
dev.off()


h=df[df$Date.of.Record==as.Date("2020-02-01"),3]


#Line Plot
ggplot(data=df,aes(x=Date.of.Record, y= Confirmed))+
  geom_point(size=3,col = 'blue')+
  geom_line(lwd=1)+
  geom_vline(aes(xintercept = as.Date("2020-02-01"), col='red'), show.legend = F)+
  geom_hline(aes(yintercept = df[df$Date.of.Record==as.Date("2020-02-01"),3], col='red'), show.legend = F)+
  ggtitle("Number of Confirmed Cases wrt Date")
#  scale_y_continuous(breaks = sort(c(seq(min(df$Confirmed), max(df$Confirmed), length.out=5), h)))


#Bar Plot
ggplot(data=df, aes(x=Date.of.Record, y=Confirmed))+
  geom_bar(stat = "identity", fill='red')+
  geom_text(aes(label=round(Confirmed)), position=position_dodge(width=0.9), vjust=-0.25)
