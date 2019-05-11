

##### Projet BI FINANCE ######



library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(quantmod)



# Les indices
CAC40<-(getSymbols("^FCHI", 
                from = "2014-01-01", 
                to = "2019-01-01", 
                src="yahoo",
                auto.assign=FALSE))

SP500<-(getSymbols("^GSPC", 
                   from = "2014-01-01", 
                   to = "2019-01-01", 
                   src="yahoo",
                   auto.assign=FALSE))

NIKKEI<-(getSymbols("^N225", 
                   from = "2014-01-01", 
                   to = "2019-01-01", 
                   src="yahoo",
                   auto.assign=FALSE))

# Actions US

US<-(getSymbols(c("FB","NFLX","TSLA","SBUX"), 
                  from = "2014-01-01", 
                  to = "2019-01-01", 
                  src="yahoo",
                  auto.assign=TRUE))

fb<-(getSymbols("FB", 
                             from = "2014-01-01", 
                             to = "2019-01-01", 
                             src="yahoo",
                             auto.assign=FALSE))

netflix<-(getSymbols("NFLX", 
                from = "2014-01-01", 
                to = "2019-01-01", 
                src="yahoo",
                auto.assign=FALSE))

tesla<-(getSymbols("TSLA", 
                     from = "2014-01-01", 
                     to = "2019-01-01", 
                     src="yahoo",
                     auto.assign=FALSE))

starbucks<-(getSymbols("SBUX", 
                   from = "2014-01-01", 
                   to = "2019-01-01", 
                   src="yahoo",
                   auto.assign=FALSE))
# Actions EU

EU<-(getSymbols(c("RMS.PA","TOT","CS.PA"), 
                from = "2014-01-01", 
                to = "2019-01-01", 
                src="yahoo",
                auto.assign=TRUE))

hermes<-(getSymbols("RMS.PA", 
                       from = "2014-01-01", 
                       to = "2019-01-01", 
                       src="yahoo",
                       auto.assign=FALSE))

total<-(getSymbols("TOT", 
                    from = "2014-01-01", 
                    to = "2019-01-01", 
                    src="yahoo",
                    auto.assign=FALSE))

axa<-(getSymbols("CS.PA", 
                   from = "2014-01-01", 
                   to = "2019-01-01", 
                   src="yahoo",
                   auto.assign=FALSE))

# Actions Asie

ASIE<-(getSymbols(c("FUJIY","SNE","MUFG"), 
                from = "2014-01-01", 
                to = "2019-01-01", 
                src="yahoo",
                auto.assign=TRUE))

fujifilm<-(getSymbols("FUJIY", 
                 from = "2014-01-01", 
                 to = "2019-01-01", 
                 src="yahoo",
                 auto.assign=FALSE))

toyota<-(getSymbols("TM", 
                      from = "2014-01-01", 
                      to = "2019-01-01", 
                      src="yahoo",
                      auto.assign=FALSE))

sony<-(getSymbols("SNE", 
                         from = "2014-01-01", 
                         to = "2019-01-01", 
                         src="yahoo",
                         auto.assign=FALSE))

# Premiere observation des valeurs boursières

chartSeries(axa,theme = chartTheme("white"))
chartSeries(fb,theme = chartTheme("white"))
chartSeries(fujifilm,theme = chartTheme("white"))
chartSeries(hermes,theme = chartTheme("white"))
chartSeries(toyota,theme = chartTheme("white"))
chartSeries(sony,theme = chartTheme("white"))
chartSeries(netflix,theme = chartTheme("white"))
chartSeries(starbucks,theme = chartTheme("white"))
chartSeries(tesla,theme = chartTheme("white"))
chartSeries(total,theme = chartTheme("white"))


# Création du portefeuille

axa<-to.monthly(axa)
fb<-to.monthly(fb)
fujifilm<-to.monthly(fujifilm)
hermes<-to.monthly(hermes)
toyota<-to.monthly(toyota)
sony<-to.monthly(sony)
netflix<-to.monthly(netflix)
starbucks<-to.monthly(starbucks)
tesla<-to.monthly(tesla)
total<-to.monthly(total)
SP500<-to.monthly(SP500)
CAC40<-to.monthly(CAC40)
NIKKEI<-to.monthly(NIKKEI)










pf<-cbind(monthlyReturn(axa,type = "log"),
          monthlyReturn(fb,type = "log"),
          monthlyReturn(fujifilm,type = "log"),
          monthlyReturn(hermes,type = "log"),
          monthlyReturn(toyota,type = "log"),
          monthlyReturn(sony,type = "log"),
          monthlyReturn(netflix,type = "log"),
          monthlyReturn(starbucks,type = "log"),
          monthlyReturn(tesla,type = "log"),
          monthlyReturn(total,type = "log"),
          monthlyReturn(CAC40,type = "log"),
          monthlyReturn(SP500,type = "log"),
          monthlyReturn(NIKKEI,type = "log")
          )
names(pf)<-c("axa","fb","fujifilm","hermes","toyota","sony","netflix","starbucks","tesla","total","cac40","sp500","nikkei")

# Statistiques usuels

table.Stats(pf)

# Calcul du ratio de sharp, return per unit of risk, higer ratio better

table.AnnualizedReturns(pf)

# Calcul Tracking error, Ratio d'information

InformationRatio(pf[,c("fb","netflix","starbucks","tesla")], pf[,12], scale = NA)

InformationRatio(pf[,c("axa","total","hermes")], pf[,11], scale = NA)

InformationRatio(pf[,c(3,5,6)], pf[,13], scale = NA)



#Performance

charts.PerformanceSummary(pf[,c(13,5,6,3)])
charts.PerformanceSummary(pf[,c(11,4,10,1)])
charts.PerformanceSummary(pf[,c(12,7,8,9,2)])


#Correlation

chart.Correlation(pf[,c(3,5,6,13)],histogram = TRUE, pch="+")
chart.Correlation(pf[,c(1,4,10,11)],histogram = TRUE, pch="+")
chart.Correlation(pf[,c(2,7,8,9,12)],histogram = TRUE, pch="+")


#Performance relative selon le benchmark
chart.RelativePerformance(pf[,c(3,5,6),drop=FALSE],pf[,13,drop=FALSE],colorset = rich8equal,legend.loc = "left")
chart.RelativePerformance(pf[,c(1,4,10),drop=FALSE],pf[,11,drop=FALSE],colorset = rich8equal,legend.loc = "left")
chart.RelativePerformance(pf[,c(2,7,8,9),drop=FALSE],pf[,12,drop=FALSE],colorset = rich8equal,legend.loc = "left")



