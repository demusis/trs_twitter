# Carregando as bibliotecas necessárias
library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)
library(tidyverse)
library(tidytext)

# Inserindo as chaves de autenticação da API do Twitter
chave_api = "" # sua chave_api
segredo_api = "" # seu segredo_api 
token_acesso = "" # seu token_acesso 
segredo_token_acesso = "" # seu segredo_token_acesso 

# Configurando o OAuth do Twitter
setup_twitter_oauth(chave_api, segredo_api, token_acesso, segredo_token_acesso)

# Definindo o usuário de interesse
usuario = getUser("IUCNRedList")

# Obtendo a lista de amigos do usuário
amigos = usuario$getFriends()

# Exibindo os primeiros registros da lista de amigos
head(amigos)

# Convertendo a lista de amigos em um dataframe e exibindo seus primeiros registros
df_amigos = twListToDF(amigos) %>% rownames_to_column()
head(df_amigos)

# Obtendo a lista de seguidores do usuário
seguidores = usuario$getFollowers()

# Convertendo a lista de seguidores em um dataframe e exibindo seus primeiros registros
df_seguidores = twListToDF(seguidores) %>% rownames_to_column()
head(df_seguidores)

# Selecionando os 10 seguidores com mais seguidores
top_seguidores = df_seguidores %>%
  mutate(data = as.Date(created, format = "%Y-%m-%d"),
         hoje = as.Date("2017-11-07", format = "%Y-%m-%d"),
         dias = as.numeric(hoje - data),
         quantidadeStatus_porDia = statusesCount / dias) %>%
  select(screenName, followersCount, quantidadeStatus_porDia) %>%
  arrange(desc(followersCount)) %>%
  .[1:10, ]
head(top_seguidores)

# Selecionando os 10 seguidores com mais tweets por dia
top_tweets = df_seguidores %>%
  mutate(data = as.Date(created, format = "%Y-%m-%d"),
         hoje = as.Date("2017-11-07", format = "%Y-%m-%d"),
         dias = as.numeric(hoje - data),
         quantidadeStatus_porDia = statusesCount / dias) %>%
  select(screenName, followersCount, quantidadeStatus_porDia) %>%
  arrange(desc(quantidadeStatus_porDia)) %>%
  .[1:10, ]
head(top_tweets)

# Combinando as duas seleções anteriores e eliminando duplicatas
top_seguidores_tweets = rbind(top_seguidores, top_tweets) %>% unique()
top_seguidores_tweets

# Selecionando os 100 seguidores com maior "score" (seguidores * tweets por dia)
top_seguidores2 = df_seguidores %>%
  mutate(data = as.Date(created, format = "%Y-%m-%d"),
         hoje = as.Date("2017-06-07", format = "%Y-%m-%d"),
         dias = as.numeric(hoje - data),
         quantidadeStatus_porDia = statusesCount / dias) %>%
  select(screenName, followersCount, quantidadeStatus_porDia) %>%
  mutate(pontuacao = followersCount * quantidadeStatus_porDia) %>%
  arrange(desc(pontuacao)) %>%
  .[1:100, ]
head(top_seguidores2)

# Adicionando a descrição do seguidor ao dataframe anterior e criando uma coluna de ID
top_seguidores_tweets2 = top_seguidores2 %>%
  left_join(select(df_seguidores, screenName, description), by = "screenName") %>%
  mutate(id = seq_along(1:n()))
head(top_seguidores_tweets2)

