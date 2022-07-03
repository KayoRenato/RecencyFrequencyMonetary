
# Problema de Negocio - Realizar segmentacao de clientes com base em analise RFM (Recencia, Frequencia e Valor Monetario)

library(tidyverse) # para manipular os dados
library(dplyr) # para manipular os dados
library(ggplot2) # para criar graficos
library(caret) # para modelo de aprendizado de maquina
library(plotly) # para criar graficos
library(readxl) # para ler os dados
library(rfm) # para realizar a segmentacao de clientes
library(stats) # para calcular dados estatisticos
library(factoextra) # para trabalhar com variaveis de formas diferentes

# Funcao para carregar os dados da planilha Excel
carrega_dados <- function()
{
  sheet1 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2009-2010')
  sheet2 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2010-2011')
  dados_combinados <- rbind(sheet1, sheet2)
  return(dados_combinados)
}


# Executa a funcao
dados <- carrega_dados()
dim(dados)
View(dados)

# Funcao para checar valores ausentes
verifica_missing <- function(x)
{
 miss <- round(colSums(is.na(x))/dim(x)[1]*100,2)
  return(miss)
}

# Executa a funcao
verifica_missing(dados)


# Excluir os registros com valores ausentes

# Funcao para limpar e pre-processar os dados
preprocessa_dados <- function(data1)
{
  # Remove registros com valores ausentes
  data1 <- na.omit(data1)
  
  # Removemos as linhas da coluna Invoice que contem a letra C (o que significa que este pedido foi cancelado)
  data1 <- data1[!grepl("C",data1$Invoice),]
  
  # Criando uma coluna chamada TotalPrice
  data1$TotalPrice <- data1$Quantity * data1$Price
  
  
  return(data1)
  
}

# Executa a funcao
dataset <- preprocessa_dados(dados)
dim(dataset)
View(dataset)
str(dataset)

verifica_missing(dataset)

# Verificando a distribuicao da variavel Total Price
ggplot(dataset,
       aes(x = TotalPrice)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 3.5) +
  labs(title = 'Distribuicao da Variavel TotalPrice')

# Numero de clientes
length(dataset$`Customer ID`) # numero de transacoes 
length(unique(dataset$`Customer ID`)) # numero de cliente

# Total monetario gasto por cliente
total_gasto <- dataset %>%
  group_by(`Customer ID`) %>%
  summarise(Sum = sum(TotalPrice))

View(total_gasto)

# Criando uma data customizada (Natal de 2011)

max(dataset$InvoiceDate) # verificar a ultima data de compra
date1 = as.Date.character("25/12/2011","%d/%m/%Y") # criacao de data de referencia para recencia

# Funcao para converter as datas do formato POISxt para o formato Date
converte_data <- function(x)
{
  options(digits.secs = 3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
}

# Executa a funcao
dataset$InvoiceDate <- converte_data(dataset)
View(dataset)
str(dataset)


# Funcao para calcular Recencia, Frequencia e Valor Monetario
calcula_rfm <- function(x){
  z <- x %>% group_by(`Customer ID`) %>%
    summarise(Recency = as.numeric(date1 - max(InvoiceDate)), # o quanto recente o cliente realizou a compra
              Frequency = n(), # o numero de compra que cada cliente realizou
              Monetary = sum(TotalPrice), #o total de compra que o cliente realizou
              primeira_compra = min(InvoiceDate)) # a data da primeira compra do cliente 
  return(z)
}

wo_outlier <- function(z){
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  
  # Removendo transacoes com valores acima do 3 Quartil e abaixo do Quartil 1 (removendo outliers)
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR))
  return(z)
}


outlier <- function(z){
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  
  # Removendo transacoes com valores entre o 1 e  3 Quartil (selecionando os outliers)
  z <- subset(z, z$Monetary < (Q1 - 1.5*IQR) | z$Monetary > (Q3 + 1.5*IQR))
  return(z)
  
}
# Executa a funcao
valores_rfm <- calcula_rfm(dataset)
str(valores_rfm)
dim(valores_rfm)
View(valores_rfm)

# Dataset sem os outliers
df_sem_outlier <- wo_outlier(valores_rfm)
str(df_sem_outlier)
dim(df_sem_outlier)
View(df_sem_outlier)

valores_rfm[valores_rfm != df_sem_outlier]

# Dataset dos outliers
df_outlier <- outlier(valores_rfm)
str(df_outlier)
dim(df_outlier)
View(df_outlier)


# ------------ Machine Learning - Clusterizacao Kmeans

# Set seed
set.seed(1029)

# Funcao para a segmentacao de clientes com base nos valores RFM
segmenta_cliente <- function(rfm)
{
  # Cria uma lista
  resultados <- list()
  
  # Obtem os valores RFM
  dados_rfm <- select(rfm, c('Recency','Frequency','Monetary')) # removendo apenas as colunas do RFM do dataset
  
  # Cria o modelo
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  
  # Plot do modelo - 1 elemento da lista
  resultados$plot <- fviz_cluster(modelo_kmeans,
                                  data = dados_rfm,
                                  geom = c('point'),
                                  ellipse.type = 'euclid')
  
  # Organiza os dados - 2 elemento da lista
  dados_rfm$`Customer ID` <- rfm$`Customer ID`
  dados_rfm$clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
  
  return(resultados)
}

# Executa a funcao

# Para toda base 
grafico <- segmenta_cliente(valores_rfm)[1] # o 1 elemento da lista atribuido na funcao (plot)
grafico

tabela_rfm <- segmenta_cliente(valores_rfm)[2] # o 2 elemento da lista atribuido na funcao (tabela)
View(as.data.frame(tabela_rfm))

# Para a base sem os outliers
grafico_wo <- segmenta_cliente(df_sem_outlier)[1]
grafico_wo

tabela_rfm_wo <- segmenta_cliente(df_sem_outlier)[2]
View(as.data.frame(tabela_rfm_wo))
