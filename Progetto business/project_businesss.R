## Import libraries

library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(lubridate)
library(gam)
library(ggcorrplot)
library(car)
library(ggplot2)
library(reshape2)
library(xgboost)

wine = read_excel("C:\\Users\\mazza\\Downloads\\global-food.xlsx", sheet=2)
grapes = read_excel("C:\\Users\\mazza\\Downloads\\global-food.xlsx", sheet=4)
wine_prices=read_excel("C:\\Users\\mazza\\Downloads\\global-food.xlsx", sheet=5)
wine_prices=wine_prices[-62,]

## EDA

### Plot Wine Prices

wine_prices_ts <- ts(wine_prices$Prezzo, start = 1961, frequency = 1)
plot(wine_prices_ts, main = "Annual prices from 1961 to 2021", xlab = "Year", ylab = "Price", col = "blue")

### Plot Wine's production

wine_production_ts <- ts(wine$`Production (t)`, start = 1961, frequency = 1)
plot(wine_production_ts, main = "Annual production (tons) from 1961 to 2021", xlab = "Year", ylab = "Production", col = "red")

### Plot Wine's exports

wine_exports_ts <- ts(wine$`Exports (t)`, start = 1961, frequency = 1)
plot(wine_exports_ts, main = "Annual exports (tons) from 1961 to 2021", xlab = "Year", ylab = "Exports", col = "black")
#nel 1986 ci fu un forte calo a causa dello scandalo del metanolo


### Plot Wine's food available for consumption
#descrizione prodotto: This measures the quantity that is available for consumption at the end of the supply chain. 
#It does not account for consumer waste, sothe quantity that is actually consumed may be lower than this value.
 
wine_food_available_ts <- ts(wine$`Food (t)`, start = 1961, frequency = 1)
plot(wine_food_available_ts, main = "Wine's food available for consumption (tons) from 1961 to 2021", xlab = "Year", ylab = "Wine's food available for consumption", col = "red")

###GRAPES
grapes_production_ts <- ts(grapes$`Production (t)`, start = 1961, frequency = 1)
plot(grapes_production_ts, main = "grapes production (tons) from 1961 to 2021", xlab = "Year", ylab = "Production", col = "black")

###############################    plot combinati     ###############################################
plot(wine_prices_ts, main = "Annual prices and exports from 1961 to 2021", xlab = "Year", ylab = "Price", col = "blue")
par(new = TRUE)
plot(wine_exports_ts, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(side = 4)

plot(wine_prices_ts, main = "Annual prices and production from 1961 to 2021", xlab = "Year", ylab = "Price", col = "blue")
par(new = TRUE)
plot(wine_production_ts, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(side = 4)

plot(wine_prices_ts, main = "Annual prices and food available from 1961 to 2021", xlab = "Year", ylab = "Price", col = "blue")
par(new = TRUE)
plot(wine_food_available_ts, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(side = 4)

plot(wine_prices_ts, main = "Annual prices and grapes production from 1961 to 2021", xlab = "Year", ylab = "Price", col = "blue")
par(new = TRUE)
plot(grapes_production_ts, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(side = 4)

######################################################################
######################################################################
###########CORRELAZIONI TRA LE VARIABILI CHE STO CONSIDERANDO#########

data_corr <- data.frame(
  Exports = wine$`Exports (t)`,
  Wine_Production = wine$`Production (t)`,
  Food = wine$`Food (t)`,
  Grapes_Production = grapes$`Production (t)`,
  Price = wine_prices$Prezzo
)

cor_matrix <- cor(data_corr)
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  ggtitle("Correlation Matrix Heatmap")
print(cor_matrix)


#Esportazioni e Disponibilità Alimentare: Correlazione negativa forte (-0.7811497). Indica che quando l'esportazione aumenta, la disponibilità di vino per il consumo alimentare diminuisce.
#Questo può implicare che una parte significativa della produzione di vino è destinata all'esportazione, riducendo la quantità disponibile per il consumo locale.

#Produzione di Vino e Disponibilità Alimentare: Correlazione positiva forte (0.7675680). Un aumento della produzione di vino è associato a una maggiore disponibilità di vino per il consumo alimentare, suggerendo che parte della produzione è destinata al mercato locale.
#Produzione di Vino e Produzione di Uva: Correlazione positiva molto forte (0.9873321). Questo indica una relazione quasi diretta tra la quantità di uva prodotta e la quantità di vino prodotta, il che è intuitivo, dato che l'uva è la materia prima per il vino.

#1. Prezzo del Vino e Esportazioni (Exports)

#Correlazione Positiva Forte (0.7775969):
# Un aumento delle esportazioni è fortemente correlato con un aumento del prezzo del vino. Questo può essere spiegato dal fatto che l'aumento delle esportazioni riduce la quantità di vino disponibile nel mercato interno, creando una pressione al rialzo sui prezzi. La riduzione dell'offerta interna spinge i consumatori locali a pagare di più per il vino, aumentando così il prezzo.

#2. Prezzo del Vino e Produzione di Vino (Wine_Production)

#Correlazione Negativa Forte (-0.7789358):
#  L'aumento della produzione di vino è associato a una diminuzione dei prezzi. Questo suggerisce che un'elevata produzione di vino aumenta l'offerta sul mercato, portando a una riduzione dei prezzi. Quando c'è abbondanza di vino, la competizione tra i produttori può portare a una riduzione dei prezzi per attrarre i consumatori.

#3. Prezzo del Vino e Disponibilità Alimentare (Food)

#Correlazione Negativa Molto Forte (-0.9579230):
# La disponibilità di vino per il consumo alimentare è fortemente negativa con il prezzo del vino. Quando c'è una maggiore quantità di vino destinata al consumo alimentare, i prezzi tendono a diminuire. Questo può essere dovuto al fatto che una maggiore disponibilità per il consumo interno riduce la scarsità percepita, facendo diminuire i prezzi.

#4. Prezzo del Vino e Produzione di Uva (Grapes_Production)

#   Correlazione Negativa Forte (-0.7815560):
#      L'aumento della produzione di uva è associato a una diminuzione dei prezzi del vino. Poiché l'uva è la materia prima per la produzione del vino, un'elevata produzione di uva porta a un'abbondanza di vino, riducendo così i costi di produzione e, di conseguenza, i prezzi del vino.


#ACF
par(mfrow=c(2,3))
Acf(wine_prices$Prezzo, main = "Prices")
Acf(wine$`Exports (t)`, main = "Exports")
Acf(wine$`Food (t)`, main = "Food Available")
Acf(wine$`Production (t)`, main = "Wine Production")
Acf(grapes$`Production (t)`, main = "Grapes Production")

#1. Serie wine_prices$Prezzo

#Descrizione del Grafico:
 
#Il grafico mostra i valori dell'autocorrelazione dei prezzi del vino (Prezzo) a vari lag.
#Le linee orizzontali tratteggiate rappresentano i limiti di confidenza a circa 95%.

#Interpretazione:

#Si osserva un'elevata autocorrelazione nei primi lag, che diminuisce gradualmente.
#Questo suggerisce che i prezzi del vino sono fortemente correlati con i loro valori passati recenti, ma questa correlazione diminuisce con l'aumentare del lag.
#Un pattern di decrescita graduale potrebbe indicare una tendenza nei dati o una componente stagionale.

#2. Serie wine$Exports (t)

#Descrizione del Grafico:

#    Il grafico mostra i valori dell'autocorrelazione delle esportazioni di vino (Exports) in tonnellate a vari lag.
#Le linee orizzontali tratteggiate rappresentano i limiti di confidenza a circa 95%.

#Interpretazione:
  
 # L'autocorrelazione è alta per i primi lag e diminuisce rapidamente.
  #  Questo indica che le esportazioni di vino sono inizialmente correlate con i loro valori passati recenti, ma questa correlazione diminuisce più rapidamente rispetto ai prezzi.
   # La rapida diminuzione potrebbe suggerire una minore dipendenza a lungo termine rispetto ai prezzi del vino.

#3. Serie wine$Food (t)

#Descrizione del Grafico:

 #   Il grafico mostra i valori dell'autocorrelazione del consumo di vino come cibo (Food) in tonnellate a vari lag.
#Le linee orizzontali tratteggiate rappresentano i limiti di confidenza a circa 95%.

#Interpretazione:
  
 # Anche in questo caso, si osserva un'alta autocorrelazione iniziale che diminuisce con l'aumentare del lag.
#La diminuzione graduale indica che il consumo di vino come cibo è correlato ai valori passati, ma questa correlazione si attenua con il tempo.
#Questo pattern è simile a quello dei prezzi del vino, suggerendo una possibile tendenza o stagionalità nei dati.

#4. Serie wine$Production (t)

#Descrizione del Grafico:
  
 # Il grafico mostra i valori dell'autocorrelazione della produzione di vino (Production) in tonnellate a vari lag.
  #  Le linee orizzontali tratteggiate rappresentano i limiti di confidenza a circa 95%.

#Interpretazione:

 #   L'autocorrelazione è alta per i primi lag e diminuisce gradualmente.
#La produzione di vino mostra una forte dipendenza dai valori passati recenti, con una diminuzione graduale della correlazione.
#Questo può indicare un ciclo di produzione stagionale o tendenze nei dati di produzione.

#5. Serie grapes$Production (t)

#Descrizione del Grafico:
  
 # Il grafico mostra i valori dell'autocorrelazione della produzione di uva (Production) in tonnellate a vari lag.
#    Le linee orizzontali tratteggiate rappresentano i limiti di confidenza a circa 95%.

#Interpretazione:

 #   Si osserva una forte autocorrelazione iniziale che diminuisce gradualmente.
  #  La produzione di uva mostra una dipendenza dai valori passati, simile alla produzione di vino, ma la diminuzione sembra essere più regolare.
   # Questo suggerisce un pattern di produzione stagionale o tendenze nei dati di produzione di uva.

#Conclusione Generale

#In tutti i grafici ACF, si osserva una forte autocorrelazione iniziale che diminuisce con l'aumentare del lag. Questo è tipico delle serie temporali con tendenze o stagionalità. La velocità di diminuzione dell'autocorrelazione può fornire informazioni sulla durata della dipendenza seriale:

#    Prezzi del vino e consumo di vino come cibo mostrano una diminuzione più graduale, suggerendo una possibile tendenza o stagionalità.
#    Esportazioni di vino mostrano una diminuzione più rapida, indicando una minore dipendenza a lungo termine.
#    Produzione di vino e produzione di uva mostrano un pattern simile, suggerendo cicli di produzione stagionali.



#PACF
par(mfrow = c(2, 3))
pacf(wine_prices$Prezzo, main = "Prices")
pacf(wine$`Exports (t)`, main = "Exports")
pacf(wine$`Food (t)`, main = "Food Available")
pacf(wine$`Production (t)`, main = "Wine Production")
pacf(grapes$`Production (t)`, main = "Grapes Production")

#1. Serie wine_prices$Prezzo

#Interpretazione:

 #   L'autocorrelazione parziale è alta solo al lag 1 e poi diminuisce rapidamente.
#Questo suggerisce che i prezzi del vino sono influenzati principalmente dal valore immediatamente precedente, ma non significativamente dai valori a lag maggiori.
#Un alto valore al lag 1 e valori insignificanti per lag successivi indicano che un modello AR(1) potrebbe essere appropriato per questi dati.

#2. Serie wine$Exports (t)

#Interpretazione:

#L'autocorrelazione parziale è significativa solo al lag 1 e poi diventa insignificante.
#Questo indica che le esportazioni di vino sono principalmente influenzate dal valore del lag 1.
#Come nel caso dei prezzi del vino, un modello AR(1) potrebbe essere adatto per catturare la struttura di dipendenza nei dati delle esportazioni di vino.

#3. Serie wine$Food (t)
#Interpretazione:

#L'autocorrelazione parziale è significativa al lag 1 e al lag 2, ma poi diventa insignificante.
#Questo suggerisce che il consumo di vino come cibo è influenzato dai valori dei primi due lag.
#Un modello AR(2) potrebbe essere appropriato per questi dati, considerando che i primi due lag hanno un'influenza significativa.

#4. Serie wine$Production (t)
#Interpretazione:
  
#L'autocorrelazione parziale è significativa ai primi tre lag (lag 1, lag 2 e lag 3), poi diventa insignificante.
#Questo indica che la produzione di vino è influenzata dai valori dei primi tre lag.
#Un modello AR(3) potrebbe essere adatto per questi dati.

#5. Serie grapes$Production (t)
#Interpretazione:
  
# L'autocorrelazione parziale è significativa ai primi due lag (lag 1 e lag 2), poi diventa insignificante.
# Questo suggerisce che la produzione di uva è influenzata dai valori dei primi due lag.
# Un modello AR(2) potrebbe essere appropriato per questi dati.

#Conclusione Generale

#In tutti i grafici PACF, possiamo vedere la struttura di dipendenza delle serie temporali:

#Prezzi del vino (Prezzo): Significativo solo al lag 1, suggerendo un modello AR(1).
#Esportazioni di vino (Exports): Significativo solo al lag 1, suggerendo un modello AR(1).
#Consumo di vino come cibo (Food): Significativo ai primi due lag, suggerendo un modello AR(2).
#Produzione di vino (Production): Significativo ai primi tre lag, suggerendo un modello AR(3).
#Produzione di uva (Production): Significativo ai primi due lag, suggerendo un modello AR(2).


######################################################################
############################DEFINIZIONE TRAIN E TEST SET############################################

train_indices <- seq_len(nrow(wine_prices)-5)

train_wp_set <- wine_prices[train_indices, ,drop=F]
test_wp_set <- wine_prices[-train_indices, ,drop=F]

train_wine_set <- wine[train_indices, ,drop=F]
test_wine_set <- wine[-train_indices, ,drop=F]

train_grapes_set <- grapes[train_indices, ,drop=F]
test_grapes_set <- grapes[-train_indices, ,drop=F]

#####################LINEAR REGRESSION WITH TREND################################## 
train_wp_ts <- ts(train_wp_set$Prezzo, frequency = 1)
test_wp_ts <- ts(test_wp_set$Prezzo, frequency = 1)

train_we_ts <- ts(train_wine_set$`Exports (t)`, frequency = 1)
test_we_ts <- ts(test_wine_set$`Exports (t)`, frequency = 1)

train_wf_ts <- ts(train_wine_set$`Food (t)`, frequency = 1)
test_wf_ts <- ts(test_wine_set$`Food (t)`, frequency = 1)

train_wprod_ts <- ts(train_wine_set$`Production (t)`, frequency = 1)
test_wprod_ts <- ts(test_wine_set$`Production (t)`, frequency = 1)

train_gr_ts <- ts(train_grapes_set$`Production (t)`, frequency = 1)
test_gr_ts <- ts(test_grapes_set$`Production (t)`, frequency = 1)

##############################################################################
#######################################linear model####################################

linear_ts_wp <- tslm(train_wp_ts ~ trend)
summary(linear_ts_wp)
aic_value <- AIC(linear_ts_wp)
aic_value


#AIC=-26.4707   R2_adj= 0.9418   RMSE= 0.17  MAPE=6.65%

par(mfrow = c(1, 1))       
fit <- fitted(linear_ts_wp)         
plot(train_wp_ts, ylab = "Wine Price", xlab = "Time", xaxt = "n", main="adaptation of the model ")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
lines(fit, col='red')

forecast <- forecast(linear_ts_wp, h = 5)
plot(forecast,ylab="Price", xlab="Time", main="",xaxt="n")
title(main="Forecast of wine prices from Linear Regression Model")
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red") #sistema linea rossa osservazioni test
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

metrics_linear_wp_ts <- accuracy(as.numeric(forecast$mean), test_wp_ts)
metrics_linear_wp_ts

#Sovrastima Sistemica: Le previsioni tendono a sovrastimare i valori reali, come indicato dal ME positivo e dall'MPE.
 #   Buona Accuratezza: Le metriche RMSE, MAE e MAPE indicano una buona accuratezza generale delle previsioni, con errori moderati e una bassa percentuale di errore rispetto ai valori reali.
  #  Autocorrelazione Residua: La presenza di una leggera autocorrelazione nei residui suggerisce che il modello potrebbe beneficiare di un miglioramento per catturare meglio la struttura temporale dei dati.
   # Comparazione con Modelli Naïve: Theil's U vicino a 1 indica che il modello con trend è solo leggermente migliore di un modello di previsione naïve. Potrebbe essere utile esplorare modelli più complessi o includere ulteriori covariate per migliorare la precisione delle previsioni.

#ANALYSIS OF RESIDUALS

checkresiduals(linear_ts_wp)


#1. Residui nel Tempo (Grafico in Alto)

#Questo grafico mostra i residui del modello in funzione del tempo.

#Interpretazione: Dovresti vedere un pattern casuale, senza trend evidente o ciclicità. Se noti pattern sistematici, significa che il modello non cattura tutte le informazioni dai dati.
#Osservazioni: In questo caso, sembra esserci un trend nei residui, il che indica che il modello potrebbe non aver catturato qualche struttura nei dati, come trend o stagionalità.

#2. Autocorrelazione dei Residui (ACF, Grafico in Basso a Sinistra)

#Questo grafico mostra la funzione di autocorrelazione dei residui.

#Interpretazione: Le linee tratteggiate rappresentano i limiti di confidenza al 95%. Se i punti superano questi limiti, suggerisce autocorrelazione significativa nei residui.
#Osservazioni: Ci sono alcune barre che superano i limiti di confidenza, indicando autocorrelazione nei residui. Questo suggerisce che i residui non sono completamente indipendenti, un segno che il modello potrebbe essere migliorato.

#3. Istogramma dei Residui (Grafico in Basso a Destra)

#Questo grafico mostra la distribuzione dei residui.

#Interpretazione: L'istogramma dovrebbe apparire approssimativamente come una distribuzione normale centrata su zero. La curva sovrapposta è la distribuzione normale teorica.
#    Osservazioni: L'istogramma sembra approssimativamente normale, ma ci sono alcune deviazioni che potrebbero indicare che i residui non sono perfettamente normali.

#Considerazioni Generali

#Non Normalità dei Residui:
 # La presenza di deviazioni dalla normalità nei residui suggerisce che gli errori non seguono una distribuzione normale, il che può influenzare le inferenze statistiche del modello.

#Autocorrelazione nei Residui:
 # La presenza di autocorrelazione indica che i residui non sono indipendenti. Questo può significare che esiste una struttura temporale nei dati non catturata dal modello.

#Pattern nei Residui nel Tempo:
 # La presenza di un trend o pattern nei residui indica che il modello non ha catturato completamente la struttura dei dati.

##########ORA AGGIUNGO LE ALTRE VARIABILI COME COVARIATE DEL MODELLO E VEDO QUALI SONO SIGNIFICATIVE
tslm_model <- tslm(train_wp_ts ~ trend + train_we_ts  + train_wprod_ts)
summary(tslm_model)
aic_value <- AIC(tslm_model)
aic_value

#AIC=-68.02   R2_adj=0.973 rmse=0.15  mape=4.2%

par(mfrow = c(1, 1))
fit <- fitted(tslm_model)
plot(train_wp_ts, ylab = "Wine Price", xlab = "Year", xaxt = "n", main="adaptation of the model")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
lines(ts(fit), col='red')


new_data <- data.frame(
  trend = 57:61,  # Proiezione dei prossimi 5 anni
  train_we_ts = test_wine_set$`Exports (t)`,
  train_wprod_ts = test_wine_set$`Production (t)`
  )
forecast <- forecast(tslm_model, h = 5, newdata=new_data)
plot(forecast,ylab="Price", xlab="Year", main="",xaxt="n")
title(main="Forecast of wine prices from Linear Regression Model")
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

metrics_linear_wp_ts <- accuracy(as.numeric(forecast$mean), test_wp_ts)
metrics_linear_wp_ts

checkresiduals(tslm_model)

#analisi dei residui migliora un sacco, rimane autocorrelazione quindi vediamo con altri modelli se cammbia, p_val residui <0.05 non tifiuto ipotesi indipendenza

#############################################################################################
#############################################################################################
#I modelli ARIMA sono particolarmente utili quando si hanno dati che mostrano trend o autocorrelazione. 
#Se i dati annuali mostrano queste caratteristiche, un modello ARIMA può essere appropriato.

######ARIMA(1,1,0) + drift
auto_arima_wp <- auto.arima(train_wp_ts, stepwise=FALSE, approximation=FALSE)
summary(auto_arima_wp)


#AIC=-187.31  R2_adj=na  rmse=0.18  mape= 5.4%   
fit <- fitted(auto_arima_wp)
plot(train_wp_ts, ylab = "Wine Price", xlab = "Year", xaxt = "n", main="adaptation of the model")
lines(fit, col = 'red')
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))


forecast <- forecast(auto_arima_wp, h = 5)
plot(forecast,ylab="Price", xlab="Year", main="",xaxt="n")
title(main="Forecast of wine price from Arima Model")
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red")  
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
metrics_arima_wp <- accuracy(as.numeric(forecast$mean), test_wp_ts)
metrics_arima_wp

#We can notice that the errors not decrease maybe for some outliers in the prevision thanks to the ARIMA model
#cerca su chat perchè metriche di merda

#ANALYSIS OF ARIMA MODEL RESIDUALS
checkresiduals(auto_arima_wp)

#Grafico dei Residui nel Tempo

#Osservazioni:
#La maggior parte dei residui oscilla intorno allo zero, il che è un buon segno.
#Ci sono alcune fluttuazioni significative, indicando possibili variazioni non catturate dal modello.

#Osservazioni:

#Tutte le barre dell'ACF rientrano entro i limiti di confidenza (linee blu tratteggiate), suggerendo che non vi è autocorrelazione significativa nei residui a nessun lag.

#Interpretazione:

 #   Se i residui sono rumore bianco, tutte le barre dovrebbero rientrare entro i limiti di confidenza, indicando nessuna autocorrelazione significativa.
  #  In questo caso, poiché tutte le barre rientrano nei limiti di confidenza, possiamo concludere che i residui non mostrano evidenza di autocorrelazione significativa, suggerendo che il modello cattura bene le dinamiche della serie temporale.

#Istogramma dei Residui con Curva Normale Sovrapposta

#Osservazioni:
#  L'istogramma dei residui mostra una distribuzione approssimativamente normale, ma con alcune deviazioni.
#        Ci sono alcuni outlier significativi a entrambi gli estremi della distribuzione.

 #   Interpretazione:
  #      L'idealità è che i residui seguano una distribuzione normale con media zero.
#Le deviazioni dalla curva normale indicano che i residui non seguono perfettamente una distribuzione normale, il che può influenzare l'affidabilità delle previsioni del modello.

#P-value > 0.05: The null hypothesis is not rejected, suggesting that there is not enough evidence 
#to claim that there is autocorrelation in the residuals. 

############################################################################################

#######################################################################################################
######ARIMA(1,1,0) + drift

# Adattamento del modello ARIMAX con le covariate ridotte
fit_arimax <- auto.arima(train_wp_ts, xreg = as.matrix(scale(log(train_we_ts))), stepwise = F, approximation = F)

# Riassunto del modello
summary(fit_arimax)

#AIC=-188.4    R2_adj=na  rmse=0.14 mape=4.10%    
fit <- fitted(fit_arimax)
plot(train_wp_ts, ylab = "Wine Price", xlab = "Year", xaxt = "n", main="adaptation of the model")
lines(fit, col = 'red')
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

# Generare previsioni future utilizzando i nuovi regressori
mtr = as.matrix(scale(log(test_we_ts)))
colnames(mtr)="Series 1"
forecast_arimax <- forecast(fit_arimax, xreg = mtr, h = 5)
plot(forecast_arimax,ylab="Price", xlab="Year", main="",xaxt="n")
title(main="Forecast of wine price from Arimax Model")
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
metrics_arima_wp <- accuracy(as.numeric(forecast_arimax$mean), test_wp_ts)
metrics_arima_wp
#We can notice that the errors not decrease maybe for some outliers in the prevision thanks to the ARIMA model


#ANALYSIS OF ARIMA MODEL RESIDUALS
checkresiduals(fit_arimax)

#buoni residui ma pval>0.5


##############################################################################################
########################## GAM with spline  ######################################

train_wp_set$Anno <- as.numeric(train_wp_set$Anno)
train_wp_set$Anno[42]=2002

annopd=s(train_wp_set$Anno)
grapespd=s(train_gr_ts)
exportpd=s(train_we_ts)
productionpd=s(train_wprod_ts)
foodpd=s(train_wf_ts)

fit_gam <- gam(train_wp_set$Prezzo ~ annopd + grapespd+ exportpd )
summary(fit_gam)

#AIC=-71.22  R2_ADJ=0.972  RMSE=   MAPE=
############CALCOLO R2#########
predicted <- fitted(fit_gam)
observed <- train_wp_set$Prezzo

ss_total <- sum((observed - mean(observed))^2)
ss_residual <- sum((observed - predicted)^2)
r_squared <- 1 - (ss_residual / ss_total)
n <- length(observed)
p <- length(fit_gam$coefficients) - 1  # numero di predittori (escluso l'intercetta)
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared_adj
#######################################
aic_value=AIC(fit_gam)
aic_value

#AIC=-67.49  R2_adj= 0.97 RMSE=0.14   MAPE=4.01% 
par(mfrow = c(1, 1)) 
fit <- fitted(fit_gam)
plot(train_wp_ts, ylab = "Wine Price", xlab = "Year", xaxt = "n", main="adaptation of the model")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
lines(ts(fit), col='red')


new_data <- data.frame(
  "annopd" = 2017:2021,
  "exportpd"=s(test_we_ts),
  "grapespd"=s(test_gr_ts)
  )
predictions <- predict(fit_gam, newdata = new_data)
#aggiusta grafico completo conprevisioni 
plot(predictions,ylab="Price", xlab="Time", main="",xaxt="n")
title(main="Forecast of wine prices from Linear Regression Model")

plot(forecast_arimax,ylab="Price", xlab="Year", main="",xaxt="n")
title(main="Forecast of wine price from Arimax Model")
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))




#lines(ts(test_wp_set$Prezzo, frequency = 1, start=2.38),col="red")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))


metrics_linear_wp_ts <- accuracy(as.numeric(predictions), test_wp_ts)
metrics_linear_wp_ts

checkresiduals(fit_gam)

##############################################################################################
################################################################################################
holt_wp_ts <- holt(train_wp_ts)
summary(holt_wp_ts)

#AIC=-112  R2_ADJ=0.99   RMSE=0.2   MAPE=5.9%

predicted <- fitted(holt_wp_ts)
observed <- train_wp_set$Prezzo

ss_total <- sum((observed - mean(observed))^2)
ss_residual <- sum((observed - predicted)^2)
r_squared <- 1 - (ss_residual / ss_total)
n <- length(observed)
p <- length(fit_gam$coefficients) - 1  # numero di predittori (escluso l'intercetta)
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared_adj
#AIC-112.19     R2_adj=0.996  
# Plot dei dati originali e del modello Holt
par(mfrow = c(1, 1))
fit <- fitted(holt_wp_ts)
plot(train_wp_ts, ylab = "Wine Price", xlab = "Year", xaxt = "n", main = "Wine Prices with Holt's Exponential Smoothing Fit")
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
lines(fit, col = 'red')

# Generare previsioni future
forecast_holt <- forecast(holt_wp_ts, h = 5)
plot(forecast_holt, ylab = "Price", xlab = "Time", main = "", xaxt = "n")
title(main = "Forecast of Wine Prices from Holt's Exponential Smoothing Model")
axis(1, at = seq(1, 64, by = 5), labels = seq(1961, 2024, by = 5))
lines(ts(test_wp_set$Prezzo, frequency = 1, start=57),col="red")

# Calcolare le metriche di accuratezza
test_wp_ts <- ts(test_wp_set$Prezzo, start = 57, frequency = 1)
metrics_holt_wp_ts <- accuracy(forecast_holt$mean, test_wp_ts)
metrics_holt_wp_ts

# Analisi dei residui
checkresiduals(holt_wp_ts)

#residui buoni, indipendenti ma previsioni su test male

##################################################################################################
##################################################################################################

auto_arima_wp <- auto.arima(log(train_we_ts), stepwise=FALSE, approximation=FALSE)
summary(auto_arima_wp)

fit <- fitted(auto_arima_wp)
plot(log(train_we_ts), ylab = "Wine Price", xlab = "Time", xaxt = "n")
lines(fit, col = 'red')
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))


forecast <- forecast(auto_arima_wp, h = 5)
plot(forecast,ylab="Price", xlab="Time", main="",xaxt="n")
title(main="Forecast of wine price from Arima Model")
#lines(test_wp_ts,col="red")   porcodio aggiuysta la linessss
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))
metrics_arima_wp <- accuracy(as.numeric(forecast$mean), log(test_we_ts))
metrics_arima_wp

#ANALYSIS OF ARIMA MODEL RESIDUALS
checkresiduals(auto_arima_wp)

######real previsioni

real_prev_exp <- auto.arima(log(wine_exports_ts), stepwise=FALSE, approximation=FALSE)
summary(real_prev_exp)

fit <- fitted(real_prev_exp)
plot(log(wine_exports_ts), ylab = "Wine Price", xlab = "Time", xaxt = "n")
lines(fit, col = 'red')
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

forecast <- forecast(real_prev_exp, h = 5)
plot(forecast,ylab="Price", xlab="Time", main="",xaxt="n")
title(main="Forecast of wine price from Arima Model")
#lines(test_wp_ts,col="red")   porcodio aggiuysta la linessss
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

##############################################################################################
real_prev_wp <- auto.arima(wine_prices_ts, xreg = as.matrix(scale(log(wine_exports_ts))), stepwise = F, approximation = F)

# Riassunto del modello
summary(real_prev_wp)

#AIC=-188.61    R2_adj=na    
fit <- fitted(real_prev_wp)

plot(wine_prices_ts, ylab = "Wine Price", xlab = "Time", xaxt = "n")
lines(fit, col = 'red')
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))

# Generare previsioni future utilizzando i nuovi regressori
previsioni_esp_ts=ts(forecast$mean)
mtr = as.matrix(scale(log(previsioni_esp_ts)))
colnames(mtr)="Series 1"
forecast_arimax <- forecast(real_prev_wp, xreg = mtr, h = 5)
plot(forecast_arimax,ylab="Price", xlab="Year", main="",xaxt="n")

title(main="Forecast of wine price from Armax Model")
#lines(test_wp_ts,col="red")   porcodio aggiuysta la linessss
axis(1, at = seq(1, 61, by = 5), labels = seq(1961, 2022, by = 5))



