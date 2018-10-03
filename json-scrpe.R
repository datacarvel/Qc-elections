## Commençons donc

library(httr)
library(jsonlite)
library(rlist)
library(dplyr)

## Obtenons donc les données des résultats officiels, en format JSON

urlDG <- "https://dgeq.org/resultats.json"
response <- GET(url = urlDG)
resultatsJSON <- content(response, as = "text")

## Isolons ce qui nous intéresse, soit le nom des comtés, leurs candidats et leurs scores obtenus

candidats <- fromJSON(resultatsJSON)$circonscriptions$candidats
liste_comtes <- fromJSON(resultatsJSON)$circonscriptions$nomCirconscription
names(candidats) <- liste_comtes
candidatsDF <- list.stack(candidats, idcol = TRUE)

## Seulement plus facile à voir ainsi

candidatsDFtbl <- tbl_df(candidatsDF)

## Variables d'intérêt: .id, numeroPartiPolitique, abreviationPartiPolitique, nbVoteTotal, tauxVote - les noms de colonnes donnés par Élections Québec

Resultats <- select(candidatsDFtbl, .id, numeroPartiPolitique, abreviationPartiPolitique, nbVoteTotal, tauxVote)
Resultats$numeroPartiPolitique <- as.character(Resultats$numeroPartiPolitique)

## Numéros d'identification des partis dans les données: PLQ-6, PQ-8, CAQ-27, QS-40

## Je veux savoir, en gros: 
  # Dans combien de comtés CAQ ou PLQ QS est arrivé 2e, et lesquels
  # Dans combien de comtés CAQ ou PLQ QS est arrivé 3e, et lesquels
  # Dans combien de comtés CAQ ou PLQ PQ est arrivé 2e, et lesquels
  # Dans combien de comtés CAQ ou PLQ PQ est arrivé 3e, et lesquels
  # Dans combien de comtés CAQ ou PLQ le vote QS + PQ est supérieur au gagnant

## Les résultats complets dans la variable suivante, sans rien filtrer ou isoler, mais en ajoutant une colonne qui attribue une position dans le score (Gagnant = 1, Deuxième = 2, etc.)

ResultatsEtPosition <- Resultats %>% 
                          group_by(.id) %>% 
                          mutate(Position = row_number())

ResultatsEtPosition$Position <- as.character(ResultatsEtPosition$Position)

PartisProg <- c("8", "40") # Soit le PQ et QS, respectivement
PartisDroite <- c("6", "27") # Soit le PLQ et la CAQ, respectivement
Positions2et3 <- c("2", "3") # Les positions #2 et #3 dans le rang électoral de chaque comté, afin d'éventuellement éliminer les comtés dans lesquels le PQ ou QS l'ont emporté

PartisProg_Pos2_3 <- ResultatsEtPosition %>% 
  filter(numeroPartiPolitique %in% PartisProg) %>% 
  filter(Position %in% Positions2et3) %>% 
  group_by(.id) %>% 
  filter(n() == 2) ## Cette dernière ligne est très importante, elle permet d'identifier les comtés ou QS et le PQ se sont échangés les 2e et 3e positions, tout en éliminant les comtés ou, par exemple, QS aurait fini 3e mais le PQ 4e

length(unique(PartisProg_Pos2_3$.id))

## Donc il y a 28 circonscriptions dans lesquelles le PQ était 2e ou 3e ***ET*** QS était 2e ou 3e. En d'autres mots, si les deux partis étaient un seul parti, ce sont les comtés où il aurait fini au moins 2e

## Mais cela sert vraiment juste à dire que QS ou le PQ était 2e ou 3e dans tel nombre de comtés - ça ne dit pas si le parti a ramassé 5 000 ou 100 votes de moins que le gagnant

## Mais additionnons justement les voix obtenues de PQ et QS dans tous les comtés où aucun des deux n'a gagné (sans égard à la position précise de QS et du PQ, puisqu'on s'intéresse au score obtenu, pas au rang)

## Les comtés réellement gagnés par QS ou le PQ le 1er octobre 2018 (19)

PartisProg_Win19 <- ResultatsEtPosition %>% 
  filter(numeroPartiPolitique %in% PartisProg) %>% 
  group_by(.id)

PartisProg_Win19_V <- unique(PartisProg_Win19$.id)

## Les comtés réellement gagnés par la CAQ ou le PLQ 1er octobre 2018 (106)

PartisDroite_Win106 <- ResultatsEtPosition %>%
  filter(!.id %in% PartisProg_Win19_V) %>%
  group_by(.id)

## Les comtés dans lesquels les voix additionnées de PQ et de QS sont supérieures à celles du parti gagnant (mais non pas PLQ + CAQ!)

PartisProgBeatDroite <- PartisDroite_Win106 %>%
  summarise(WIN = tauxVote[Position %in% 1] < sum(tauxVote[numeroPartiPolitique %in% PartisProg])) %>%
  group_by(.id)

sum(PartisProgBeatDroite$WIN)

## 15 comtés de plus seulement... ? 

## Voici les comtés dans lesquels les voix de QS et du PQ sont supérieures à celles du gagnant, étant soit le PLQ ou la CAQ (ComtesProgPerdus_V)

ComtesPartisProgBeatDroite <- PartisProgBeatDroite %>%
  filter(WIN == TRUE)

ComtesProgPerdus_V <- ComtesPartisProgBeatDroite$.id

## En somme, QS et le PQ, s'ils formaient une sorte de coalition, auraient obtenu 15 sièges de plus, augmentant leur députation à 34 plutôt que les 19 réellement obtenus

## C'est significatif, mais à peine plus important que ce que le PLQ a obtenu

## Mais au fait, des 15 partis que PQ-QS auraient gagné de plus ensemble, à qui cette coalition progressite les auraient enlevé?

ComtesProgPerdus_CAQouPLQ <- PartisDroite_Win106 %>%
  filter(.id %in% ComtesProgPerdus_V) %>%
  filter(Position %in% "1") %>%
  summarise(CAQ = sum(numeroPartiPolitique %in% "27"), PLQ = sum(numeroPartiPolitique %in% "6")) %>%
  summarise(sum(CAQ), sum(PLQ))

## La CAQ aurait donc 10 sièges de moins et le PLQ, 5. Ce qui fait que la CAQ aurait 64 sièges plutôt que 74, soit 1 de plus qu'il n'en faut pour être un gouvernement majoritaire...
## En d'autres mots, une sorte d'alliance PQ-QS n'aurait dans le cas de cette élection que fragiliser la majorité caquiste, et n'aurait pas enlevé grand chose au PLQ

## Et pour finir, tout cela est très, très, très hypothétique, imaginaire et spéculatif, car on ne peut assumer que dans le cas d'un pacte électoral ou le PQ et QS n'auraient jamais présenter leurs candidats dans les mêmes comtés, les votes du PQ auraient tous été récupérés par QS et vice versa. Le 2e choix de plusieurs péquistes était la CAQ, où ils semblent en fait avoir migrer, tandis que les électeurs solidaires n'aiment pas tous le PQ.

## Tout ça pour dire que la députation de l'Assemblée nationale n'aurait pas été énormément plus progressiste. En d'autres mots, il semble que le Québec n'avait pas soif de progressisme cette année.

## Tableau complet des circonscriptions concernées:

Resultats15comtesProg <- ResultatsEtPosition %>% filter(.id %in% ComtesProgPerdus_V) %>% group_by(.id)