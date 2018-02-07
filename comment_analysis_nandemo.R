#------------------------------------------------
# コメントデータのテキスト分析
#------------------------------------------------

library(data.table)
library(dplyr)
library(RMeCab)
library(stringr) #Rでテキストの正規表現使えるパッケージ

library(wordcloud2)
library(RColorBrewer)


#-----------------------------------------------------------------------------------
#「なんでも」カテゴリのデータで、topic_idごとにファイルを分割したデータをインポート
#----------------------------------------------------------------------------------
setwd("~/Documents/BBS_analysis/Format_data/")

#-----------------------------------------------------
# 除外ワードを抽出するために、品詞の詳細分類まで見る
#------------------------------------------------------
setwd("~/Documents/BBS_analysis/Format_data/Nandemo")

files <- dir()
work.df<-data.frame()
posdf <-data.frame()
ic <-0
for (i in files){
  #if(i == "topic_id_2890642"){
  ic <- ic+1
  work.df <- as.data.frame(RMeCabFreq(i))
  posdf <- rbind(posdf,work.df)
 # }
}


dim(posdf)
head(posdf)
#-- 137303ワードが存在する
#-- この中から「名詞」を抽出しそのInfo2(詳細分類)毎の頻度を確認する

posdf %>% dplyr::filter(Info1 =="名詞") %>% dplyr::group_by(Info2) %>% dplyr::summarise(cnt=sum(Freq)) %>%
  dplyr::arrange(desc(cnt))

#-----------------------------------------------------
# ナイ形容詞語幹とはどういうものか？(あまりないが)
#-----------------------------------------------------
posdf %>% dplyr::filter(Info1 =="名詞",Info2=="ナイ形容詞語幹") 
#--　このままだと、同じTermが文書ごとにたくさんレコードに重複しているので、集計する。
posdf %>% dplyr::filter(Info1 =="名詞",Info2=="ナイ形容詞語幹") %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20)

#--　仕方、問題、申し訳、間違い、頼り、だらし、素っ気など「ない」が後ろにつく名詞
#--除外するかどうかの判断→除外しない。

#-----------------------------------------------------
# 動詞非自立的とはどういうものか？(あまりないが)
#-----------------------------------------------------
posdf %>% dplyr::filter(Info1 =="名詞",Info2=="動詞非自立的")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20)

#--　具体例：ごらん、ちょ、ちょうだい、ご覧、頂戴　この５つだけ
#-- 除外するかどうかの判断。除外する。

posdf %>% dplyr::filter(Info1 =="名詞",Info2=="動詞非自立的") 　%>% head(30)
#--　仕方、問題、申し訳、間違い、頼り、だらし、素っ気など「ない」が後ろにつく名詞
#--除外するかどうかの判断→除外しない。

#-----------------------------------------------------
# 副詞可能　名詞とはどういうものか？(あまりないが)
#-----------------------------------------------------
posdf %>% dplyr::filter(Info1 =="名詞",Info2=="副詞可能")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20)

exdf<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="副詞可能")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20) %>% as.data.frame()

write.table(exdf,"/Users/y_takeda/Documents/BBS_analysis/20180130/名詞_副詞可能.tsv",sep='\t',row.names=F,quote=F)

#ワードの取りだし
meishi_hukushi<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="副詞可能")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()

meishi_hukushi.list <- meishi_hukushi$Term

#-- 
#-----------------------------------------------------
# 非自立　名詞とはどういうものか？
#-----------------------------------------------------
posdf %>% dplyr::filter(Info1 =="名詞",Info2=="非自立")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20)

ex3<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="非自立")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20) %>% as.data.frame()

write.table(ex3,"/Users/y_takeda/Documents/BBS_analysis/20180130/名詞_非自立.tsv",sep='\t',row.names=F,quote=F)

#ワードの取りだし
meishi_hijiritsu<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="非自立")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()

meishi_hijiritsu.list<-meishi_hijiritsu$Term

#-- 名詞のなかで、"非自立"、"代名詞","数" ,"接尾","副詞可能"

#-----------------------------------------------------
# 名詞のうちの数詞を抽出
#-----------------------------------------------------
meishi_suushi<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="数")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()

meishi_suushi.list<- meishi_suushi$Term

#-----------------------------------------------------
# 名詞のうちの代名詞を抽出
#-----------------------------------------------------
meishi_daimeishi<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="代名詞")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()

meishi_daimeishi.list<- meishi_daimeishi$Term

#-----------------------------------------------------
# 名詞のうちの接尾を抽出
#-----------------------------------------------------
meishi_setsubi<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="接尾")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()

meishi_setsubi.list<- meishi_setsubi$Term

ex4<-posdf %>% dplyr::filter(Info1 =="名詞",Info2=="接尾")  %>% dplyr::group_by(Term) %>%
  dplyr::summarise(Freq = sum(Freq)) %>% dplyr::arrange(desc(Freq)) %>% head(20) %>% as.data.frame()

write.table(ex4,"/Users/y_takeda/Documents/BBS_analysis/20180130/名詞_接尾.tsv",sep='\t',row.names=F,quote=F)

#-----------------------------------------------------
# 本当に必要な部分は以下から
#-----------------------------------------------------

#-----------------------------------------------------------------
# 名詞の中から、以下の詳細分類に該当するワードを除外することにする
#"非自立","代名詞","数" ,"接尾","副詞可能"
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# ここから、3カ月分ログデータから取得した除外ワードファイルを読み込む。
# 以下の詳細分類に該当するワードを除外することにする
#"非自立","代名詞","数" ,"接尾","副詞可能"
#-----------------------------------------------------------------

#除外ワードリスト-- 3982ワードもある
exclude_word <- c(meishi_hijiritsu.list,meishi_daimeishi.list,meishi_setsubi.list,meishi_hukushi.list,meishi_suushi.list)
exclude_word.df <- as.data.frame(exclude_word)


#-- 一応、exportして見られるようにしておく。
write.table(exclude_word.df,"/Users/y_takeda/Documents/BBS_analysis/Format_Data/除外ワード_名詞.tsv",row.names=F,quote=F)

#-- 除外ワードファイルを読み込む。
exclude_df <- read.table("/Users/y_takeda/Documents/BBS_analysis/Format_Data/除外ワード_名詞.tsv",header=T)

#--リストにする
exclude_word <- exclude_df$exclude_word

setwd("~/Documents/BBS_analysis/Format_data")

#--------------------------------------------------------------------------------------------------------
#ディレクトリNandemoの中にある全てのファイル(topic_xxxxxxx)を読み、ターム文書行列を作成するdocMatrix2()関数
#minFreq=2とすると、全文書を通じての合計頻度が２以上のタームが判定対象となり、term/doc行列に含まれる。
#--------------------------------------------------------------------------------------------------------

system.time(res_df <-docMatrix2( "Nandemo",pos=c("名詞"),minFreq=5))


#docDF関数でターム文書行列作成(type=0)
#res_df.comp<-docDF("Nandemo",type=0) #ERRでない
#POS1、POS2属性がつかない

# 単語の頻度の集計(type=1)名詞に限定した集計
#res_df.comp2<-docDF("Nandemo",type=1,pos="名詞") #ERRとなった

#res_df.comp2<-docDF("Nandemo",type=1) ERRとなった

#--row_namesがリストの中に含まれるかどうかを判断

res_df <- as.data.frame(res_df)
res_df$Term <- row.names(res_df)
head(res_df)

#--データフレームから除外ワードを除く。7021ワードが、名詞として抽出されたターム・文書行列
dim(res_df)
#-- 除外ワードの数(名詞のうちの、数詞、)
# length(exclude_word)
#[1] 1038

#--このやり方より、filterで一発で絞り込める方が良いのでこちらはナシ
# for (i in 1:length(exclude_word)){
#      remove_word <- exclude_word[i]
#      res_df[res_df$Term == remove_word,"stopword"]<-1
#     #print(remove_word)
# }

res_df.stop <- res_df %>% dplyr::filter(!(Term %in% exclude_word))

dim(res_df.stop)
head(res_df.stop)
#res_df %>% dplyr::filter(stopword ==1) %>% dplyr::select(Term) %>% head(50)
#--テスト stopwordフィールドがNULLのレコードを抽出
#res_df %>% dplyr::filter(is.na(stopword)==TRUE ) %>% nrow()

#--ストップワードを除外したターム・文書行列
#res_df.stop <- res_df %>% dplyr::filter(is.na(stopword)==TRUE ) 

head(res_df.stop)
#------------------------------------------------------------------------
dim(res_df.stop)

#ターム毎の全文書を通じての頻度 1:50が列のカラム番号、Term列とstopword列が存在するので除く
wordFreq<- as.data.frame(rowSums(res_df.stop[,1:50]))
head(wordFreq)
str(wordFreq)
wordFreq$term <- res_df.stop$Term
colnames(wordFreq) <- c("Freq","term")
#列の入れ替え
wordFreq<-wordFreq %>% dplyr::select(term,Freq)

#全文書を通じたワードクラウド
library(wordcloud2)

wc_df <-wordFreq %>% dplyr::select(term,Freq) %>% dplyr::arrange(desc(Freq)) %>% head(300)

wc_df %>% dplyr::arrange(desc(Freq)) %>% head(20)

# 以下は「名詞」だけを抽出して、名詞の除外ワードを除外
# 全ての文書でワードカウントを行った結果
# term  Freq
# 1        笑笑 30799
# 2         >>1  4511
# 3        子供  3585
# 4        自分  2549
# 5          笑  2248
# 6        旦那  1730
# 7          親  1400
# 8          ー  1217
# 9        仕事  1125
# 10 マンション  1013
# 11       トピ   997
# 12       配信   980
# 13       お金   962
# 14       ママ   877
# 15   コメント   789
# 16     気持ち   780
# 17       無駄   743
# 18       息子   724
# 19         嫌   664
# 20       普通   649
##

#--1) 意味の無い言葉(「笑笑」、「笑」「ー」)をさらに除く
#--これは、除外ワード名詞.tsvに追加した
#wc_dfa <- wc_df %>% dplyr::filter(!(term %in% c("笑笑","笑","ー")))

#--2) >>数字で始まる「引用の記号」は除く
wc_dfa<- wc_df %>% dplyr::filter(!(str_detect(term,"^>>[0-9]")))
 
wc_dfa %>% dplyr::arrange(desc(Freq)) %>% head(20)

wordcloud2(wc_dfa,size=1.1,minSize=0)

#------------------------------------------------------------------
# N-gram(全てのトピックコメント)
#------------------------------------------------------------------
setwd("~/Documents/BBS_analysis/Format_data/ALL_merge")
#document <- readLines("all_topic")



#--単語2gramの抽出
ngram.result <- Ngram("all_topic",type=1,pos=c("名詞","形容詞","動詞","副詞"))

#--結果確認
head(ngram.result,10)

#--結果から、「子供」を含む単語Ngramを抽出

#--子供
find_word <- "子供"
ng.res.1 <- ngram.result %>% dplyr::filter(str_detect(Ngram,find_word) ) %>% dplyr::arrange(desc(Freq)) %>% 
  head(30)

#--マンション
find_word <- "マンション"
ng.res.2<- ngram.result %>% dplyr::filter(str_detect(Ngram,find_word) ) %>% dplyr::arrange(desc(Freq)) %>% 
  head(30)  

#--旦那
find_word <- "旦那"
ng.res.3<- ngram.result %>% dplyr::filter(str_detect(Ngram,find_word) ) %>% dplyr::arrange(desc(Freq)) %>% 
  head(30)  

setwd("~/Documents/BBS_analysis/20180205")
write.table(ng.res.1,"ngram_allcomment_kodomo",row.names=F,sep="\t",quote=F)
write.table(ng.res.2,"ngram_allcomment_apartment",row.names=F,sep="\t",quote=F)
write.table(ng.res.3,"ngram_allcomment_husband",row.names=F,sep="\t",quote=F)

#------------------------------------------------------------------
# コロケーションの分析
#------------------------------------------------------------------


collocation_analysis <- function(test_word,type_var){
  #--------------------------------------------------------------------------#
  #引数で与えられたワードの共起語をT値の降順で算出する関数
  #その際に与えられた文書ファイルのワードから、名詞、動詞、形容詞だけに絞る。
  #名詞でも、非自立語、接尾、数(詞)、接続詞的、副詞可能は除外する
  #引数１個目 test_word ：共起語を調べたい対象のワード
  #引数2個目、T値なら1，MI値なら2
  #使い方: response<- collocation_analysis("旦那",1)
  #--------------------------------------------------------------------------#
  
  setwd("~/Documents/BBS_analysis/Format_data/ALL_merge")
  col.result <- collocate("all_topic",node=test_word,span=5)
  
  #------------------------------------------------------------------
  # コロケーションの分析(Tスコアによる共起度を測る)
  #------------------------------------------------------------------
  
  #--共起語の分析結果を利用して、共起強度の指標（TスコアとMIスコア）
  colscore.result <- collScores(col.result,node=test_word,span=5)
  if(type_var == 1) {
    #-- T値によるソート
    #--コーパス言語学では、Tは1.65以上で、2つのタームの共起は偶然ではないと考える。
    colscore.res.T<- colscore.result %>% dplyr::filter(T >= 1.65) %>% dplyr::arrange(desc(T)) 
  } else if (type_var == 2){
    colscore.res.T<- colscore.result %>% dplyr::filter(MI >= 1.58) %>% dplyr::arrange(desc(MI)) 
  }
  
  #------------------------------------------------------------------
  # 品詞を限定したコロケーション(共起度)の分析
  #------------------------------------------------------------------
  
  file <- "all_topic" #--全てのトピックをマージしたファイル
  #ドキュメントの形態素解析、品詞情報付きの結果(実行できる)
  res <-RMeCabFreq(file)
  
  #--ここから、Info1が名詞、形容詞、動詞、を抽出する
  res.subset <- res %>% dplyr::filter(Info1 %in% c("名詞","形容詞","動詞"))
  #--非自立語を除く
  res.subset <- res.subset %>% dplyr::filter(!(Info2 =="非自立"))
  #--接尾を除く
  res.subset <- res.subset %>% dplyr::filter(!(Info2 =="接尾"))
  #--名詞のうち、数詞を除く
  res.subset <- res.subset %>% dplyr::filter(!(Info2 =="数"))
  #--名詞のうち、接続詞的　はどういうもの？確認した上で、省かないことにする。
  #res.subset %>% dplyr::filter(Info2=="接続詞的") %>% head(30)
  #--VSと、兼と、対しかない
  #--副詞可能名詞は、除く
  ## res.subset %>% dplyr::filter(Info2=="副詞可能") %>% head(30)
  res.subset <- res.subset %>% dplyr::filter(!(Info2 == "副詞可能"))
  
  #--品詞カテゴリ毎頻度
  ## res.subset %>% dplyr::group_by(Info1,Info2) %>% dplyr::summarise(count=sum(Freq))
  
  #--動詞非自立的な名詞とは？これは除かない
  #res.subset %>% dplyr::filter(Info1=="名詞",Info2=="動詞非自立的") %>% head(20)
  # 
  # Term Info1        Info2 Freq
  # 1     ごらん  名詞 動詞非自立的   14
  # 2       ご覧  名詞 動詞非自立的    2
  # 3       ちょ  名詞 動詞非自立的    9
  # 4 ちょうだい  名詞 動詞非自立的    4
  # 5       頂戴  名詞 動詞非自立的    2
  
  
  #--特殊名詞とは？これも省かない
  ## res.subset %>% dplyr::filter(Info1=="名詞",Info2=="特殊") %>% head(20)
  # Term Info1 Info2 Freq
  # 1   そ  名詞  特殊   37
  # 2 そう  名詞  特殊   48
  
  #--以下の単語だけが、共起語を分析する対象となるワード word_listに格納する
  word_list<-res.subset$Term
  #--共起分析の結果から、分析対象となるワードだけを抽出
  colscore.res.T.gentei<-colscore.res.T %>% dplyr::filter(Term %in% word_list)　
  
  if(type_var==1){
    res.collocation<-colscore.res.T.gentei %>% dplyr::arrange(desc(T))
  } else if (type_var==2){
    res.collocation<-colscore.res.T.gentei %>% dplyr::arrange(desc(MI))
  }
  #T値とMI値の桁を少数点3桁にする。
  
  res.collocation$T <- round(res.collocation$T,3)
  res.collocation$MI <- round(res.collocation$MI,3)
  
  return(res.collocation)
}

#--子供の例
#test_word <- "子供"
#test_word<- "子ども"
setwd("~/Documents/BBS_analysis/20180206")

#--関数を使って「旦那」の共起語を調べる.パラメータ1は、T値、2は、MI値
collocation.res<-collocation_analysis("旦那",2)

head(collocation.res)

write.table(collocation.res,"collocation_旦那_MI値.tsv",row.names=F,quote=F,sep="\t")
dim(res.collocation.kodomo)


#--「よね」は、形態素解析で分類されていない。結果となっている。
res %>% dplyr::filter(str_detect(Term,"よね"))


#----------------------------------------------------------------------
# 単語の共起ネットワーク(co-occurrence network)の可視化
#
#----------------------------------------------------------------------
library(igraph)
# NgramDFによる共起語の集計
setwd("~/Documents/BBS_analysis/Format_data/ALL_merge")
# NgramDF関数 type=1は、ワード毎の共起
#--名詞、動詞、形容詞だけにする
#--前処理
NgramDF.result <- NgramDF("all_topic",type =1, N=2, pos ="名詞") #このままだと落ちる。

file <-"topic_id_2904249" #--今日の夕飯
setwd("~/Documents/BBS_analysis/Format_data/Nandemo")

NgramDF.result <- NgramDF(file,type=1, N=2, pos ="名詞") #これだとRは落ちない。

#--共起度3以上のペアだけ抽出
NgramDF.result.2<- NgramDF.result %>% dplyr::filter(Freq >=4) %>% dplyr::arrange(desc(Freq)) 

dim(NgramDF.result.2)
head(NgramDF.result.2)
#--ネットワークの描画
g<- graph.data.frame(NgramDF.result.2,directed=FALSE)
plot(g,vertex.label=V(g)$name,vertex.color="gray")
#----

graph.df<-graph.data.frame(NgramDF.result.2) #ネットワークマップデータを作成

E(graph.df)$weight <- NgramDF.result.2$Freq #共起の頻度を辺の重みとする

#別なウィンドウでワードの共起関係をプロットする
tkplot(graph.df, vertex.label=V(graph.df)$name,vertex.size=1,layout=layout.fruchterman.reingold,
       edge.label=E(graph.df)$weight)

#Rのウィンドウでネットワークグラフの表示
plot(graph.df,vertex.label=V(graph.df)$name,vertex.color="gray",
     edge.label=E(graph.df)$weight,vertex.size=1,
     layout=layout.fruchterman.reingold)

#--networkD3ライブラリを利用
library(networkD3)
# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction

edgeList <- NgramDF.result.2
colnames(edgeList) <- c("SourceName", "TargetName", "Weight")


# グラフオブジェクトの生成
# simplify関数を使い、重複エッジや、自己ループがないか確認する
gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))

# ノードリストオブジェクトを生成(実際にはデータフレームオブジェクト)
# ノード間の情報を含んでいる
# networkD3ライブラリは、IDsに0スタートを要求している
#--IDとnNameというカラムのデータフレームを作成する。
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),
                       nName = igraph::V(gD)$name)

# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}
# And add them to the edge list
# 3つの変数を作り、その中身をfunction(x)で渡す。
edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))


#--ggplot2で描画
library(ggplot2)

#----------------------------------------------------------------------
#１つのトピックだけでのワードクラウド
#=- topic_id_2889641トピックのワードクラウド
#--このトピックのトピックメッセージ
#
#-- 2889641トピのメッセージ本文は?
# 身長についてのアンケート】女で身長１７５センチと１４５センチだったらどっちになりたい？
setwd("/Users/y_takeda/Documents/BBS_analysis/Format_data/Nandemo/")
#file <-"topic_id_2904249" #--今日の夕飯
#file <-"topic_id_2913167" #--一戸建て・マンション
file <-"topic_id_2905452" #--ベビーカー
#-夕飯何？のトピでは、名詞、形容詞、動詞にした
#tmp_term_ps<-docMatrix2(file,pos=c("名詞","形容詞","動詞"))
#-一戸建てのメリットってあるの？のトピでは、名詞、形容詞にした
tmp_term_ps<-docMatrix2(file,pos=c("名詞"))

 
tmpdf<-as.data.frame(tmp_term_ps)
tmpdf$term <- row.names(tmpdf)
colnames(tmpdf) <- c("Freq","term")
#列の入れ替え
tmpdf<-tmpdf %>% dplyr::select(term,Freq)
head(tmpdf)
#降順にソート
tmpdf %>% dplyr::arrange(desc(Freq)) %>% head(20)

# [>>1]を除外する
tmpdf <- tmpdf %>% dplyr::filter(!(str_detect(term,"^>>[1-9]")))

#ここで、名詞の中で除外するワードを除外する
tmpdf.1 <- tmpdf %>% dplyr::filter(!(term %in% exclude_word)) %>% arrange(desc(Freq)) %>% head(100)


#-- 確認する
tmpdf.1 %>% head(15)

#--ベビーカーのトピックで、出力しておく。
setwd("~/Documents/BBS_analysis/20180202")
write.table(tmpdf.1,"topic_2905452_freq.tsv",row.names=F,
            quote=F,sep="\t")

#--「一戸建て」「戸建て」は同じ言葉なのに、別のワードとして認識されてしまう。
#--これをまとめる
library(stringr)

# [>>1]を除外する
#-- termの列にある「一戸建て」を「戸建て」に置換する
tmpdf.2 <- tmpdf.2 %>% dplyr::mutate(term = str_replace(tmpdf.2$term,"一戸建て","戸建て"))

#--同じ「戸建て」で、集計する
tmpdf.2<- tmpdf.2 %>% dplyr::group_by(term) %>% dplyr::summarise(Freq = sum(Freq)) %>% 
  dplyr::arrange(desc(Freq)) %>% as.data.frame()

head(tmpdf.2)
dim(tmpdf.2)
setwd("~/Documents/BBS_analysis/20180202")
write.table(tmpdf.1,"topic_2905452_freq.tsv",row.names=F,
            quote=F,sep="\t")

#
#setwd("~/Documents/BBS_analysis/20180201")
#write.table(tmpdf.2,"マンション戸建てトピfreq.tsv",sep="\t",row.names=F,quote=F)
#--むりやり値を変える
#tmpdf.2[tmpdf.2$term=="マンション","Freq"]<-400
#--1つのトピックでのワードクラウド上位100ワードに限定した。

#foo<- read.table("マンション戸建てトピfreq.tsv",header=T)
wordcloud2(tmpdf.1,size=1.5,minSize=0)
#minSizeは、最も頻度の低いワードが何個か？ということ？
wordcloud2(tmpdf.1,size=2,minSize=0)
wordcloud2(tmpdf.1,size=1,minSize=0)
wordcloud2(tmpdf.1,size=2,minSize=0)

wordcloud2(tmpdf.1,size=2.0,minSize=10)
wordcloud2(tmpdf.1,size=2.0,minSize=30)

wordcloud2(tmpdf.1,size=3.0,minSize=0)

#gridSizeオプション--gridサイズが大きいほど、ワード間の間隔が大きくなる
wordcloud2(tmpdf.1,size=2.0,gridSize=1)

wordcloud2(tmpdf.1,size=2.0,gridSize=1)

wordcloud2(tmpdf.1,size=2.0,gridSize=8)

wordcloud2(tmpdf.1,size=0.7,minSize=0)
wordcloud2(tmpdf.1,size=0.9,minSize=10)

#マンション、戸建てがとても強調して表示され、あとはほとんど区別がつかない小さなフォントになる
wordcloud2(tmpdf.2,size=0.7)


wordcloud2(tmpdf.2,size=0.7)

wordcloud2(tmpdf.2,size=0.73)
wordcloud2(tmpdf.2,size=0.78)
#--戸建てとマンションとが両方出力されるのは、0.79が限界だとインタラクティブに確かめた。
wordcloud2(tmpdf.2,size=0.79)

#-- size=0.9で初めて最上位の「マンション」が消える」2位の「戸建て」が最大のフォントになる。
wordcloud2(tmpdf.2,size=0.9)
wordcloud2(tmpdf.2,size=1)

#size=2とデフォルト1に対してsizeが大きくなると、下位のワードを大きなフォントで表現する
wordcloud2(tmpdf.2,size=2,minSize=0)
#--上位のワード「マンション」「一戸建て」を除き、下位のワードを大きなフォントでだす
wordcloud2(tmpdf.2,size=3,minSize=0)

#size=1と比べるとこれ
# 頻度が低いワードのサイズが大きく表示され、頻度が高いワード「マンション」「戸建て」がどちらも消える
wordcloud2(tmpdf.2,size=1.3)

library(wordcloud)
library(RColorBrewer)

#ワードクラウド--名詞、動詞、形容詞,1000以上
#par(family = "HiraKakuProN-W3")
#--wordcloudパッケージによる可視化ワードクラウド、こちらならできる。
#random.color=FALSEの場合、出現頻度が高い順に色を指定する
#library(wordcloud)
#wordcloud(tmpdf.2$term,tmpdf.2$Freq,min.freq=10,scale=c(4,.5),colors = brewer.pal( 8, "Dark2" ),random.color = FALSE)

#--pngに上手く出力されない。自動化困る
--png("wordcloud_2889461.png")
#wordcloud2(tmpdf,size=1.1,minSize=0)
#dev.off()
#----------------------------------------------------------------------
# 行列の重み付け--名詞、形容詞 TFIDFが高いワードを各文書ごとに抽出する
# まずは名詞だけを対象とする。
#----------------------------------------------------------------------
#minFreq=5は、文書全体を通じて名詞を対象とする
setwd("~/Documents/BBS_analysis/Format_data/")

tf_df <-docMatrix2( "Nandemo",pos=c("名詞"),weight="tf*idf")





# tf_dfはmatrixだった
#class(tf_df)
tf_df<- as.data.frame(tf_df)
tf_df$Term <- row.names(tf_df)

#確認
tf_df[1:5,c(1:3,49:51)]

#ここで除外ワードを除くプロセスを行う。
#--以下は、コードとして冗長
# for (i in 1:length(exclude_word)){
#   remove_word <- exclude_word[i]
#   #--注意 ワードのカラム名は、自分でつけているから確認すること！
#   tf_df[tf_df$Term == remove_word,"stopword"]<-1
#   #print(remove_word)
# }
#tf_df %>% dplyr::filter(stopword ==1) %>% head(5)
#--テスト stopwordフィールドがNULLのレコードを抽出
#tf_df %>% dplyr::filter(is.na(stopword)==TRUE ) %>% nrow()
#tf_df.stop <- tf_df %>% dplyr::filter(is.na(stopword)==TRUE ) 

tf_df.stop <- tf_df %>% dplyr::filter(!(Term %in% exclude_word) ) 
dim(tf_df.stop)

tf_df.stop1<-tf_df.stop %>% dplyr::filter(!(str_detect(Term,"^>>[1-9]")))
#確認
#tf_df.stop1 %>% dplyr::filter(str_detect(Term,"^>>[1-9]"))

tf_df.stop1[1:5,c(1:3,49:51)]


#各文書(列)毎にtfidf1以上を抽出する
#--ロジックチェック
setwd("~/Documents/BBS_analysis/Format_data/Nandemo")
x_var <-"topic_id_2904249"
example_tfidf<-tf_df.stop1 %>% dplyr::select(Term,x_var) %>% dplyr::filter( get(x_var) >= 1)  %>% 
  dplyr::arrange(desc(get(x_var))) %>% head(20)  %>% as.data.frame() 

#-- 出力
setwd("~/Documents/BBS_analysis/20180131")
write.table(example_tfidf,"topic_2904249_tfidf上位.tsv",row.names=F,quote=F,sep="\t")
#--------------------------------------------------------------------- 
doc_topic<-colnames(tf_df.stop1)[1:50]

#-----------------------------------------------------------
# 全てのトピックについてTFIDF上位ワードを抽出する。
#-----------------------------------------------------------
i = 0
important_word<- data.frame()
for (ic in doc_topic){
  i <- i+1
  #print(ic)
  result<-tf_df.stop1 %>% dplyr::select(Term,ic) %>% dplyr::filter( get(ic) >= 1)  %>% 
    dplyr::arrange(desc(get(ic))) %>% head(20) %>% as.data.frame() %>% t()
  
  important_word <- rbind(important_word,result)
#  if( i >3 ) break
}

#トピックIDを各行に付与
#-- 2レコードずつ同じトピックの結果が格納されている(ワード、TFIDF)
doc_order<-rep(doc_topic,2) %>% sort()
important_word$topic_id <- doc_order
#ファイル名を最初にする。

important_word <- important_word %>% dplyr::select(topic_id,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)


setwd("~/Documents/BBS_analysis/20180201")
write.table(important_word,"topic_tfidf_norm.tsv",row.names=F,quote=F,sep="\t")
#-----------------------------------------------------------------------------------------------------------------------------
# トピック毎のコメント数の集計ファイルの読み込み
# awk -F'\t' '{print $1}' nandemo_top50_comment.tsv |sort |uniq -c | awk '{print($2,'\t',$1)}' > nandemo_number_of_comment.tsv
#-----------------------------------------------------------------------------------------------------------------------------
setwd("~/Documents/BBS_analysis/Format_data")
tp_attrib <- read.table("nandemo_number_of_comment.tsv",header=F)
colnames(tp_attrib) <- c("topic_id","num_comment")

res$row_names
head(df_nd)
colnames(df_nd)<-c("topic_id","comment_no","handle_name","message","category_id","category_name","tp_title","tp_message","datetime")

# 結合用のtopic_id_intを作る
important_word <- important_word  %>% dplyr::mutate(topic_id_int = as.integer(sub("topic_id_","",topic_id))) 

#トピック毎のTF_IDFの結果とコメント数のデータと結合する
important_word.cm<-left_join(x = important_word, y= tp_attrib, by = c("topic_id_int"="topic_id"))

important_word.cm <- important_word.cm %>% dplyr::select(-topic_id_int)  

important_word.cm_sort <-important_word.cm %>% dplyr::arrange(desc(num_comment)) 
setwd("~/Documents/BBS_analysis/20180201")
write.table(important_word.cm_sort,"topic_tfidf_norm_comment.tsv",row.names=F,quote=F,sep="\t")

#-----------------------------------------------------------------------------------------------------------------------------

str(df_nd$message)
#-- topic_idごとに、messageをまとめる。


#topic_idのユニーク値のリスト
topic_id_list <- unique(df_nd$topic_id)

foo<-df_nd %>% dplyr::filter(topic_id == 2903588) %>% head(10)

dim(df_nd)[1]
### MeCabによる形態素解析の例

#形態素活用形（読み込んだままの表層形）を返す
# minFreq=5で全文書を通して５回以上使われたワードでないとトークンとして集計しない

#res<-docMatrixDF(sample_df[,"message"],pos=c("名詞","形容詞","動詞"),minFreq=5)

#rownames(res)

# 一部を確認する(rowname:ワード、列名ROW.1,ROW.2が文書番号)
res[1:10,1:10]

#ワード毎の頻度をカウントする
Freq_word <- as.data.frame(rowSums(res))

#フィルタする
stopwords<- Exword$exclude_word

#stopwords()は、英語でiとかmeとかmyなどのデフォルトのリスト
stopwords

# 
text <- VectorSource(sample_df$message)
text <- VCorpus(text)
text <- tm_map(text,content_transformer(tolower))

text <- tm_map(text,removeWords,stopwords)
text <- tm_map(text,stripWhitespace)

text[[1]]$content
text[[2]]$content
text[[100]]$content
#TFIDFによる重み付け

tf<-docMatrixDF(sample_df[,"message"],pos=c("名詞","形容詞","動詞"),minFreq=5,weight="tf*idf")
```

## 感情分析用の辞書をインストール
# 東京工業大学の高村研究室で公開されている PN Table を導入


