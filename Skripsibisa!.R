library(e1071)
library(caret)
library(syuzhet)
library(tm)
library(NLP)
library(stringr)
library(dplyr)
library(parallel)
library(tau)
library(tokenizers)
library(devtools)

#Read file 
skripsi <- read.csv(file.choose(), header = T)
View(skripsi)

#Deskripsi tabel
glimpse(skripsi)

#Mengubah teks dalam corpus
ulasan <- iconv(skripsi$Menurut.kamu.bagaimana.seharusnya.pembelajaran.jarak.jauh.yang.efektif., to = "utf-8")
ulasan <- Corpus(VectorSource(ulasan))
inspect(ulasan[1:20])

#Data Cleaning Pre-processing
#Mengubah huruf kapital menjadi huruf kecil
lower <- tm_map(ulasan, tolower)
inspect(lower[1:20])

#Menghapus tanda baca
cleantext <- tm_map(lower, removePunctuation)
inspect(cleantext[1:20])

#Menghapus simbol
remove.NL <- function(x)gsub("\n","",x)
data_NL <- tm_map(cleantext, remove.NL)
inspect(data_NL[1:20])

#Stopword
stop_word <- readLines("E:/Nisa/Teknoka/Data/stopword.csv")
data_stopword <- tm_map(data_NL, removeWords, stop_word)
inspect(data_stopword[1:20])

#Menghapus nomor
data_number <- tm_map(data_stopword, removeNumbers)
inspect(data_number[1:20])

#Tambahan stopword
tambahan_stopword <- tm_map(data_number, removeWords,
                            c('no', 'kali', 'sesamamenurut', 'yg', 'karna','terimakasih',
                              'idk','tsbt','maya','terimakasih','memanfaatkan',
                              'digital','dosen','masuk','mengajar','kelas','materinya', 
                              'memahami', 'tsbt','disediakan','mahasiswa','memfasilitasi','belajar',
                              'mata','kuliah','praktek','mengadakan','kuliah','lab','bengkel','nya',
                              'sangata','ketat','teknik','merasakan','lapangan','online','aja','gk',
                              'efektifkarna','ya','bikin','apapun','klo','negatif','kendala','memiliki',
                              'wifi','pengajar','pengertian','kendala','sebutkan','disesuaikan',
                              'kebutuhan','pendidikan','sd','smk','sederajat','pelajaran','dibangku','perkuliahan',
                              'kelas','karyawan','memilih','pembelajaran','online','pemberian','berat','memaklumi',
                              'sinyal','buruk','praktek','terkadang','mahasiswai','mengulang','menghemat','wkw',
                              'ga','nya','ubah','formal','kaku','usahakan','mengobrol','santai','tekan','siswa',
                              'duduk','berjam','jam','resah','menyimak','sisi','penggunaan',
                              'teknologi','lancar','daring','mencari','bentuk','jarak','efektif'))

inspect(tambahan_stopword[1:20])

#Menghapus spasi berlebih
cleanset <- tm_map(tambahan_stopword, stripWhitespace)
inspect(cleanset[1:20])

#Menyimpan data bersih
databersih <- data.frame(text=unlist(sapply(cleanset, '[')),stringsAsFactors = F)
write.csv(databersih, file ="E:/Nisa/Teknoka/Data/databersih4.csv")

#Mengimport databersih
databersih <- read.csv(file.choose(), header = TRUE)
View(databersih) 

#Tokenizing
corpustext <- Corpus(VectorSource(databersih$text))
inspect(corpustext[1:20])

text = cleanset

strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(text)

#Pembuatan Wordcloud=
library(wordcloud)
library(RColorBrewer)
text <- as.character(databersih)
wordcloud(text, max.words = 200,rot.per = 0.3,random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

library(wordcloud2)
w <- data.frame(names(freq),freq)
colnames(freq) <- c('word','freq')
wordcloud2(w,
           size = 0.8,
           shape = 'circle')


tdm <- TermDocumentMatrix(corpustext)
tdm

t <- removeSparseTerms(tdm, sparse = 0.95)

#Matriks kata
library(factoextra)
m <- as.matrix(t)

freq <- rowSums(m)
freq

barplot(freq, las=2, col = rainbow(10), cex.names = 1.0, cex.axis = 0.7)
barplot

library(syuzhet)
tubes <- read.csv(file.choose(), header = TRUE)
View(tubes)

class(tubes)
review <- as.character(tubes$text)
review <- as.data.frame(review)

emotions <- get_nrc_sentiment(tubes$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar,emotion=names(emo_bar)) 

review_1_combine <- cbind(review,emotions)

ggplot(emo_sum, aes(x=reorder(emotion,-count),y=count)) +
  geom_bar(stat = 'identity') +
  labs(title = "Classify of Emotion", x = "Emotion", y = "Number of Emotion")

library(e1071)
library(caret)

sampel = sample(1:nrow(emotions),0.8*nrow(emotions))
training = data.frame(emotions)[sampel,]
testing = data.frame(emotions)[-sampel,]
modelNB=naiveBayes(positive~.,data = training)
prediksi = predict(modelNB,testing)
hasil = confusionMatrix(table(prediksi,testing$positive))
hasil

library(wordcloud)
wordcloud(corpusdatabersih, min.freq = 4, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8,"Dark2"))





