#### Installer/Charger les packages ####
# install.packages("tidyverse") #pour faciliter la création de graphiques avec ggplot2 et la manipulation des données
# install.packages("psych") #pour faciliter la description 
# install.packages("car") #pour, par exemple, tester l'homogénéité des variances (condition d'application de l'ANOVA notamment ; le test de Shapiro-Wilk pour le test de normalité est, quant à lui, inclus de base.
# install.packages("rstatix") #pour utiliser des fonctions et des analyses statistiques telles que le wilcox_test. 
# install.packages("emmeans") #pour faire des comparaisons 2 à 2 (post hoc) si, par exemple, une ANOVA est significative. Cela permet notamment de dire quel groupe se détache des autres.
# install.packages("lme4") #pour faire des modèles linéaires mixtes (plus puissant que les ANOVA mixtes, notamment s'il y a des données manquantes ou des variables aléatoires que l'on souhaite "contrôler")
# install.packages("MuMIn") #pour décrire la proportion de variance expliquée par le(s) facteur(s) fixe(s) après avoir effectué un modèle linéaire mixte.
library(tidyverse)
library(psych)
library(car)
library(rstatix)
library(emmeans)
library(lme4)
library(MuMIn)





#### 1. IMPORTER ET COPIER LE DATASET ####
df <- IMDb #pour ne pas travailler directement sur la base de données originale.





#### 2. Examiner votre jeu de données avec la fonction "describe", "summary" ou "lapply" ####
describe(df)
summary(df)
lapply(df,class)





#### 3. Convertir les variables numériques en "numeric" ####
df <- df %>%
  mutate(Date = as.numeric(Date)) %>%
  mutate(Rate = as.numeric(Rate)) %>%
  mutate(Votes = as.numeric(Votes)) %>%
  mutate(Duration = as.numeric(Duration)) %>%
  mutate(Episodes = as.numeric(Episodes))





#### 4. Si nécessaire, supprimer les films et séries qui ont des valeurs manquantes dans les colonnes Date, Rate et Votes ####
df <- df %>%
  drop_na(Date, Rate, Votes)





#### 5. Identifier le film qui a la meilleure note (Rate)
df_films <- df %>% #Utiliser la fonction "filter" avec "==" pour ne conserver que le type "Film"
  filter(Type == "Film") #ou bien avec "!=" pour conserver tout sauf le type "Film"

df_films <- df_films %>%  #Classer les films en fonction de la note en ordre décroissant
  arrange(desc(Rate))
df_films
# La film le mieux noté est "Toma" sorti en 2021.

# Je pourrais maintenant m'interroger sur la relation entre le nombre de votes (Votes) et la note (Rate).
# Calculer la corrélation entre Rate et Votes
correlation <- cor(df_films$Votes, df_films$Rate)
correlation

# Faire le graphique avec ggplot2
ggplot(df_films, aes(x = Votes, y = Rate)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE) # Droite de régression

# Le même graphique avec des améliorations esthétiques
ggplot(df_films, aes(x = Votes, y = Rate)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Droite de régression
  labs(x = "Votes (en centaine de milliers)", y = "Notes", title = "Relation entre le nombre de votes et les notes des films") +
  theme_classic(base_size = 12) + 
  geom_text(x = max(df_films$Votes), y = min(df_films$Rate), 
            label = paste("Corrélation =", round(correlation, 2)), 
            hjust = 1, vjust = 0, size = 4, color = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre du graphique


#### Ecarter les valeurs situées en-dessous du dixième percentile ####
# Trouver la valeur correspondant au dixième percentile
q10 <- quantile(df_films$Votes, probs = 0.10)

# Ne concerver les valeurs qu'au-dessus du 10e percentile
df_films_filtered <- df_films %>%
  filter(Votes >= q10) # ici, "q10" correspond à 13.0972) 

#### Ecarter les valeurs situées à plus de 3 écart-types au-dessus et au-dessous de la moyenne ####
## Calculer la moyenne et l'écart-type
# mean_votes <- mean(df_films$Votes, na.rm = TRUE)
# sd_votes <- sd(df_films$Votes, na.rm = TRUE)

## Définir la plage basée sur l'écart-type (par exemple, 3 écarts-types)
# lower_limit <- mean_votes - 3 * sd_votes
# upper_limit <- mean_votes + 3 * sd_votes

## Filtrer les données en dehors de la plage définie par l'écart-type
# df_films_filtered <- df_films %>%
# filter(Votes >= lower_limit, Votes <= upper_limit)





#### 6. Refaire le 5. avec "df_films_filtered" au lieu de "df_films" ####
# Calculer la corrélation entre Rate et Votes
correlation <- cor(df_films_filtered$Votes, df_films_filtered$Rate)
correlation

# Le même graphique avec des améliorations esthétiques
ggplot(df_films_filtered, aes(x = Votes, y = Rate)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Droite de régression
  labs(x = "Votes (en centaine de milliers)", y = "Notes", title = "Relation entre le nombre de votes et les notes des séries") +
  theme_classic(base_size = 12) + 
  geom_text(x = max(df_films_filtered$Votes), y = min(df_films_filtered$Rate), 
            label = paste("Corrélation =", round(correlation, 2)), 
            hjust = 1, vjust = 0, size = 4, color = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre du graphique


df_films_filtered <- df_films_filtered %>% 
  arrange(desc(Rate))
df_films_filtered

# La film le mieux noté est désormais "12 Angry Men" sorti en 1957.





#### 7. Faire la même chose pour les séries ####
df_series <- df %>%
  filter(Type == "Series")

df_series <- df_series %>% 
  arrange(desc(Rate))
df_series
# La série la mieux notée est "Aspirants" sortie en 2004.

#### Ecarter les valeurs situées en-dessous du dixième percentile ####
# Trouver la valeur correspondant au dixième percentile
q10 <- quantile(df_series$Votes, probs = 0.10)

# Ne concerver les valeurs qu'au-dessus du 10e percentile
df_series_filtered <- df_series %>%
  filter(Votes >= q10) # ici, "q10" correspond à 3.285 ) 

# Là encore, je pourrais maintenant m'interroger sur la relation entre le nombre de votes (Votes) et la note (Rate).
# Calculer la corrélation entre Rate et Votes
correlation <- cor(df_series_filtered$Votes, df_series_filtered$Rate)
correlation

# Le même graphique avec des améliorations esthétiques
ggplot(df_series_filtered, aes(x = Votes, y = Rate)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Droite de régression
  labs(x = "Votes (en centaine de milliers)", y = "Notes", title = "Relation entre le nombre de votes et les notes des séries") +
  theme_classic(base_size = 12) + 
  geom_text(x = max(df_series_filtered$Votes), y = min(df_series_filtered$Rate), 
            label = paste("Corrélation =", round(correlation, 2)), 
            hjust = 1, vjust = 0, size = 4, color = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre du graphique






#### 8. Comparer s'il y a le même nombre de films et de séries avec un test statistique pour variables nominales ####
# Nous pourrions travailler directement sur df.
# Mais si nous souhaitons concerver notre filtrage au dixième percentile appliqué à "df_films_filtered" et "df_series_filtered",
# nous allons fusionner ces derniers avec la fonction "rbind".
df2 <- rbind(df_films_filtered, df_series_filtered)

# Pour utiliser un test Chi-2 pour échantillons (indépendants), il faut une table de contingence avec la variable "Type"
table_type <- table(df2$Type)
table_type

# Effectuer un test du chi-deux sur la table de contingence
chisq_test <- chisq.test(table_type)

# Afficher les résultats du test
chisq_test

# Créer un graphique en barres pour visualiser la comparaison des 2 catégories
# D'abord, créer un dataframe à partir de la table de contingence
df_table <- as.data.frame(table_type)
names(df_table) <- c("Type", "Count") # pour changer les noms des variables

# Créer le graphique
ggplot(df_table, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 0.5) + # "identity" signifie : utiliser directement la valeur unique de la colonne "Count" 
  coord_cartesian(ylim = c(0, 3500))  + # Définir la limite supérieure de l'axe y à 4000
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) + # Définir les graduations sur l'axe y tous les 500
  labs(x = "Type", y = "Count", title = "Comparaison du nombre de Films et Series") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()





#### 9. Est-ce que les films et les séries sont, en moyenne, évaluées de manière équivalente ? ####
# Comparer les notes (Rate) entre les Films et les Séries avec un t de student pour échantillons indépendants.

shapiro.test(df_films_filtered$Rate) # Tester la normalité de la distribution de "Rate" pour les films

ggplot(df_films_filtered, aes(x = Rate)) + # Faire une représentation graphique
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.5, fill = "orange", color = "black") +
  labs(title = "Distribution des évaluations pour les Films", x = "Rate", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()

shapiro.test(df_series_filtered$Rate) # Tester la normalité de la distribution de "Rate" pour les films

ggplot(df_series_filtered, aes(x = Rate)) + # Faire une représentation graphique
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.5, fill = "orange", color = "black") +
  labs(title = "Distribution des évaluations pour les Séries", x = "Rate", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()

leveneTest(Rate ~ Type, data = df2) # Tester l'homogénéité des variances

# Les prémisses du test t indépendant n'étant pas respectés, je ne peux pas l'utiliser :
# t.test(Rate ~ Type, data = df2) # t-test

# A la place, on utilise son équivalent non-paramétrique : Le test de Mann-Whitney U (aussi appelé test de Wilcoxon)
wilcox.test(Rate ~ Type, data = df2)

# Alternative au code précédent :
stat.test <- df2 %>% 
  wilcox_test(Rate ~ Type) %>% # si ne fonctionne pas, réinstaller le package rstatix
  add_significance()
stat.test

# Calculer la taille d'effet :
df2 %>% wilcox_effsize(Rate ~ Type)

# En faire une représentation graphique
# Créer un boxplot car les moyennes et écart-types ne sont pas pertinents dans le cas d'un test non-paramétrique
ggplot(df2, aes(x = Type, y = Rate, fill = Type)) +
  geom_boxplot() +
  labs(x = "Type", y = "Rate", title = "Comparaison des évaluations entre Films et Séries") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()





#### 10. Est-ce que les évaluations des mêmes spectateurs sont différentes après un second visionnage ? ####
#### Pour les besoins de l'exercice, appliquer le code ci-dessous pour artificiellement créer une colonne "Rate_2" ####
#### Générer des données aléatoires corrélées à 0.6 avec la colonne "Rate"
set.seed(123)  # Fixer la graine aléatoire pour la reproductibilité

# Générer une colonne aléatoire indépendante
n <- nrow(df2)  # Nombre de lignes dans le dataframe
random_values <- rnorm(n)  # Générer des valeurs aléatoires

# Calculer une deuxième colonne corrélée avec la colonne "Rate"
rate_values <- df2$Rate  # Récupérer les valeurs de la colonne "Rate"
correlated_values <- 0.6 * rate_values + sqrt(1 - 0.6^2) * random_values  # Générer des valeurs corrélées

# Ajouter une colonne "Rate_2" avec les valeurs corrélées
df2$Rate_2 <- correlated_values

# Renommer la variable "Rate" en "Rate_1" pour marquer la différence avec "Rate_2"
df2 <- df2 %>% rename(Rate_1 = "Rate")

# Vérifier la corrélation entre "Rate_1" et "Rate_2"
cor(df2$Rate_1, df2$Rate_2)





#### 10.bis Est-ce que les évaluations des mêmes spectateurs sont différentes après un second visionnage ? ####
shapiro.test(df2$Rate_1) # Tester la normalité de la distribution de "Rate_1"

ggplot(df2, aes(x = Rate_1)) + # Faire une représentation graphique
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.5, fill = "orange", color = "black") +
  labs(title = "Distribution des évaluations des Rate_1", x = "Rate_1", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()

shapiro.test(df2$Rate_2) # Tester la normalité de la distribution de "Rate_2

ggplot(df2, aes(x = Rate_2)) + # Faire une représentation graphique
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.5, fill = "orange", color = "black") +
  labs(title = "Distribution des évaluations de la Rate_2", x = "Rate_2", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()


# Transformez en données en "long format" :
# Rassembler les évaluations Rate_1 et Rate_2 dans la même colonne
df3 <- df2 %>% gather(key = "Evaluator", value = "Global_rate", Rate_1, Rate_2)
df3

# Effectuer un test t pour échantillons appariés entre "Rate_1" et "Rate_2"
result <- t.test(Global_rate ~ Evaluator, data = df3, paired = TRUE)

# Afficher les résultats du test
print(result)

# Calculer la taille d'effet d de Cohen
df3 %>% cohens_d(Global_rate ~ Evaluator, paired = TRUE)

# Pour avoir les moyennes pour rapporter les résultats
describeBy(df3$Global_rate, df3$Evaluator)

# Nuage de points avec moyennes et barres d'erreurs pour visualiser la comparaison 
df3.summary <- df3 %>%
  group_by(Evaluator) %>%
  summarise(N    = length(Global_rate),
            mean = mean(Global_rate, na.rm = TRUE),
            sd   = sd(Global_rate, na.rm = TRUE),
            se   = sd / sqrt(N))

ggplot(df3.summary, aes(x = Evaluator, y = mean)) +
  geom_point(data = df3, aes(y = Global_rate), color = "brown", size = 2, position = position_jitter(width = 0.4, height = 0), alpha = 0.2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, group = Evaluator), size = 1.5, width = 0.4, alpha = 0.5) + # Les geom_ sont des couches/calques qui se superposent.
  geom_point(aes(group = Evaluator), fill = "black", size = 4, position = position_dodge(width = 0.4)) + # Ainsi, ce geom_ s'affichera par dessus les 2 précédents.
  labs(title = "Comparaison des évaluations Rate_1 et Rate_2", y = "Global_rate") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 32),  # Centrer + Taille du texte du titre
    axis.title = element_text(size = 28),  # Taille du texte des axes
    legend.text = element_text(size = 24),  # Taille du texte de la légende
    axis.text = element_text(size = 20)  # Taille des graduations sur les axes
  )





#### 11. En cas de besoin, voici l'équivalent non-paramétrique du t test pour échantillons appariés ####
# Effectuer un test des rangs signés de Wilcoxon sur échantillons appariés entre "Rate_1" et "Rate_2"
result <- wilcox.test(Global_rate ~ Evaluator, data = df3, paired = TRUE)
print(result)

# Alternative au code précédent :
stat.test <- df3 %>% 
  wilcox_test(Global_rate ~ Evaluator, paired = TRUE) %>% # Noter le simple ajout de "paired = TRUE" par rapport au Mann-Whitney U.
  add_significance() # si ne fonctionne pas, réinstaller le package rstatix
stat.test

# Calculer la taille d'effet :
df3 %>% wilcox_effsize(Global_rate ~ Evaluator, paired = TRUE) # Idem

# Créer un boxplot pour comparer les évaluations entre les deux "Evaluator"
ggplot(df3, aes(x = Evaluator, y = Global_rate, fill = Evaluator)) +
  geom_boxplot() +
  labs(x = "Evaluator", y = "Global Rate", title = "Comparaison des évaluations") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique : ne pas mettre avant la ligne "theme_classic"()

describeBy(df3$Global_rate, df3$Evaluator) # pour avoir les moyennes
