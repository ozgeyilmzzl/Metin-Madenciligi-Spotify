# Gerekli kutuphaneleri yukleyin
install.packages("tm")                  # Metin madenciligi iCin temel paket
install.packages("textclean")           # Metin temizleme islemleri iCin
install.packages("tidyverse")           # Veri isleme ve gorsellestirme iCin
install.packages("wordcloud")           # Kelime bulutlari olusturma
install.packages("syuzhet")             # Duygu analizi
install.packages("quanteda")            # Metin analizi iCin gelismis araClar
install.packages("stringr")             # Metin manipulasyonu
install.packages("tidytext")            # Metin analizi iCin tidy format
install.packages("text2vec")            # Derin metin analizi
install.packages("ggplot2")             # Gorsellestirme iC'in
install.packages("writexl")             # Excel dosyasD1 yazma
install.packages("sentimentr")          # Duygu analizi
install.packages("lubridate")           # Zaman verisi manipulasyonu
install.packages("igraph")              # grafikteki araclari gorsellestirme
install.packages("topicmodels")         # Konu modelleme
install.packages("RTextTools")          # Metin sD1nD1flandD1rma
install.packages("text")                # NLP iCin modern araClar
install.packages("tidyr")               # Veri biCimleme
install.packages("caret")               # Makine ogrenimi araClarD1
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("openxlsx")  
library(tm)                 # Metin madenciligii paketini yukle
library(textclean)          # Metin temizleme islemi iCin
library(tidyverse)          # Veri isleme ve gorsellestirme
library(wordcloud)          # Kelime bulutlari
library(syuzhet)            # Duygu analizi yapmak iCin
library(quanteda)           # Metin analizi ve modelleme
library(stringr)            # Metin manipulasyonu
library(tidytext)           # Tidy text islemleri
library(text2vec)           # Derin metin analizi
library(ggplot2)            # Gorsellestirme iCin
library(writexl)            # Excel dosyasD1na yazmak iCin
library(sentimentr)         # Duygu analizi iCin
library(lubridate)          # Tarih ve saat verisi manipC<lasyonu
library(igraph)             # grafikte araclarD1 gorsellestirme
library(topicmodels)        # Konu modelleme
library(RTextTools)         # Metin sD1nD1flandD1rma islemleri icin
library(text)               # NLP islemleri
library(tidyr)              # Veri biCimleme ve duzenleme
library(caret)              # Makine ogrenimi
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(openxlsx)

# Veriyi yC<kleyin

data <- read.csv("C:/Users/Asus/Desktop/train.csv")

data_filtered <- data %>%
  mutate(release_year = year(ymd(track_album_release_date))) %>%          # Tarihten yD1lD1 C'D1kar
  filter(release_year >= 2015 & release_year <= 2024)                     # YD1l aralD1DD1na gC6re filtrele

train_selected <- data_filtered %>%
  select(lyrics, playlist_genre, track_album_release_date)

write.xlsx(train_selected, "selected_train.xlsx")


train_selected$lyrics <- trimws(train_selected$lyrics)
train_selected$lyrics <- tolower(train_selected$lyrics)
train_selected$lyrics <- gsub("[[:punct:]]", "", train_selected$lyrics)
train_selected$lyrics <- gsub("[0-9]", "", train_selected$lyrics)
train_selected$lyrics <- removeWords(train_selected$lyrics, stopwords("en"))


write_xlsx(train_selected, "cleaned_train.xlsx")

train_pop <- train_selected %>% filter(playlist_genre == "pop")
write_xlsx(train_pop, "train_pop.xlsx")

# Durak kelimeleri C6zelleEtirmek iC'in listeyi oluEturun

custom_stop_words <- c("yeah", "oh", "na", "and", "but", "or", "the", "im", "que", "just", "got", "la", 
                       "de", "te", "youre", "wanna", "aint", "dont", "ooh", "el", "yo", "en", "ya", "lo", "hey", 
                       "DD1", "DD1m", "DD1ts", "DD1f", "if", "ah", "es", "tu", "DD1ve", "se", "si", "mi", "bitch", "uh", 
                       "em", "shit", "fuck", "DD1t", "DD1n", "don", "version", "taylor", "DD1s", "song", "ll", "una", "Dif",
                       "LL", "if", "im", "dont", "yeah", "ooh","go","can","cause","know","now","una","ive","us","one","take","let","way","much","litte")
stop_words_extended <- bind_rows(stop_words, tibble(word = custom_stop_words, lexicon = "custom"))

# Durak kelimeleri C'D1karma ve kelimeleri sayma

filtered_word_counts <- train_pop %>%
  unnest_tokens(word, lyrics) %>%                                       # EarkD1 sC6zlerini kelimelere ayD1rma
  mutate(word = tolower(word)) %>%                                      # Kelimeleri kC<C'C<k harfe C'evirme
  anti_join(tibble(word = custom_stop_words), by = "word") %>%          # Durak kelimeleri C'D1karma
  count(word, sort = TRUE)                                              # Kelime frekansD1nD1 sayma ve sD1ralama
                                                                        # En sD1k kullanD1lan 15 kelimeyi seC'me ve gC6rselleEtirme
filtered_word_counts %>%
  slice_max(n, n = 15) %>%                                              # En sD1k kullanD1lan 15 kelimeyi seC'me
  ggplot(aes(reorder(word, n), n)) +                                    # Kelimeleri frekansa gC6re sD1ralama
  geom_bar(stat = "identity", fill = "dimgray") +                       # Bar grafik oluEturma
  coord_flip() +                                                        # Yatay barlar oluEturma
  labs(
    title = "En ??ok Kullan??lan Kelimeler",                              # Grafik baElD1DD1
    x = "Kelime",                                                       # X ekseni etiketi
    y = "Frekans"                                                       # Y ekseni etiketi
  ) +
  theme_minimal()                                                       # Minimal tema

library(wordcloud)
library(RColorBrewer)

# Grafik alan??n?? geni??letmek i??in

par(mar = c(1, 1, 1, 1))                                                # Alt, sol, ??st ve sa?? kenar bo??luklar??n?? azaltma

                                                                        # Kelime bulutunu olu??turma
wordcloud(
  words = filtered_word_counts$word,                                    # Kelimeler
  freq = filtered_word_counts$n,                                        # Frekanslar
  min.freq = 10,                                                        # Minimum frekans (10'dan az olanlar dahil edilmez)
  max.words = 80,                                                       # Maksimum kelime say??s?? (daha fazla kelime)
  scale = c(3, 0.5),                                                    # Kelime boyut aral?????? (frekansa g??re boyut de??i??ir)
  colors = brewer.pal(8, "Dark2"),                                      # Renk paleti
  random.color = TRUE,                                                  # Rastgele renk kullan??m??
  random.order = FALSE,                                                 # Kelimeler s??ral?? ??ekilde yerle??tirilsin
  rot.per = 0.3                                                         # Kelimelerin d??nd??r??lme oran?? (0.3 ise %30 rastgele d??nd??r??l??r)
)

library(tidytext)
library(dplyr)
library(ggplot2)

# NRC lexicon'nu y??kleyin

nrc <- get_sentiments("nrc")
                                                                        # Lyrics s??tunundan kelimeleri ????kar??n
sonuc <- train_pop %>%
  mutate(linenumber = row_number()) %>%                                 # Sat??r numaras??n?? olu??turma
  unnest_tokens(word, lyrics) %>%                                       # lyrics s??tunundan kelimeleri ????kartma
  inner_join(nrc, by = c("word" = "word")) %>%                           # NRC lexicon ile birle??tirme
  group_by(linenumber) %>%                                               # Sat??r numaras??na g??re grupla
  summarise(sentiment_score = sum(ifelse(sentiment == "positive", 1,
                                         ifelse(sentiment == "negative", -1, 0)))) %>%
  left_join(train_pop %>% mutate(linenumber = row_number()), by = "linenumber")
                                                                          # Duygu kategorilerini hesapla
neutral <- length(which(sonuc$sentiment_score == 0))
positive <- length(which(sonuc$sentiment_score > 0))
negative <- length(which(sonuc$sentiment_score < 0))
                                                                          # Toplam duygu say??s??n?? hesapla
toplam <- positive + neutral + negative
                                                                          # Oranlar?? hesapla
Duygular <- c("Pozitif", "Notr", "Negatif")
Degerler <- c((positive / toplam) * 100, (neutral / toplam) * 100, (negative / toplam) * 100)
                                                                            # Sonu??lar?? data frame'e yerle??tir
output <- data.frame(Duygular, Degerler)
                                                                            # Fakt??r olarak d??zenle
output$Duygular <- factor(output$Duygular, levels = Duygular)

# Bar grafi??i olu??turma

ggplot(output, aes(x = Duygular, y = Degerler)) +
  geom_bar(stat = "identity", aes(fill = Duygular), show.legend = FALSE) +
  ggtitle("Duygu Analizinin Oranlari") +
  theme_minimal() +
  xlab("Duygu Kategorileri") +
  ylab("Oran (%)") 

library(dplyr)
library(ggplot2)
library(tidytext)

# 8 duygu kategorisi

nrc_8 <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("joy", "anger", "surprise", "fear", "sadness", "disgust", "trust", "anticipation"))
                                                                                      # Duygular?? sayma ve oran hesaplama
sonuc_8 <- train_pop %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(nrc_8, by = "word") %>%
  count(sentiment) %>%
  mutate(Oran = n / sum(n) * 100)
                                                                                        # Grafi??i olu??turma
ggplot(sonuc_8, aes(y = reorder(sentiment, -Oran), x = Oran, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Duygu Kategorilerinin Oranlar??", y = "Duygular", x = "Oran (%)") +
  theme_minimal()

#POLAR??TE

polarite <- get_sentiment(train_pop$lyrics, method = "syuzhet")
train_pop$word_count <- sapply(train_pop$lyrics, function(x) str_count(x, "\\w+"))

tablo <- data.frame(word_count = train_pop$word_count, polarite = polarite)

ggplot(tablo, aes(word_count, polarite)) +
  geom_point(color="blue") +
  geom_hline(yintercept = mean(tablo$polarite), color= "red", linewidth=1) +          
  labs(y= "Polarite Skoru", x= "Kelime Say??s??") +
  theme_minimal() +
  labs() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))


# ??statistiksel hesaplamalar

mean_polarite <- mean(tablo$polarite, na.rm = TRUE)
se_mean <- sd(tablo$polarite, na.rm = TRUE) / sqrt(length(tablo$polarite))
CI_mean_0.95 <- mean_polarite + c(-1, 1) * qt(0.975, df = length(tablo$polarite) - 1) * se_mean
var_polarite <- var(tablo$polarite, na.rm = TRUE)
std_dev_polarite <- sd(tablo$polarite, na.rm = TRUE)
coef_var <- (std_dev_polarite / mean_polarite) * 100

stat_summary <- data.frame(
  mean = mean_polarite,
  se.mean = se_mean,
  CI.mean.0.95 = CI_mean_0.95[2] - CI_mean_0.95[1],  #                                                 CI fark??n?? alal??m (??st - alt)
  var = var_polarite,
  std.dev = std_dev_polarite,
  coef.var = coef_var
)

print(stat_summary)









