library(arules)
transaksi_dqlab_retail.tran<- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)

data_item <- itemFrequency(transaksi_dqlab_retail.tran, type="absolute")
data_item_top <- sort(data_item, decreasing = TRUE)
data_item_top <- data_item_top[1:10]
data_item_top <- data.frame("Nama Barang"=names(data_item_top), "Jumlah"=data_item_top, row.names=NULL)
write.csv(data_item_top, file="top10_item.txt", eol = "\r\n") 

data_item_bottom <- sort(data_item, decreasing = FALSE)
data_item_bottom <- data_item_bottom[1:10]
data_item_bottom <- data.frame("Nama Barang"=names(data_item_bottom), "Jumlah"=data_item_bottom, row.names=NULL)
write.csv(data_item_bottom, file="bottom10_item.txt", eol = "\r\n")

rules = apriori(data = transaksi_dqlab_retail.tran, parameter = list(minlen=2, maxlen=3, support = 0.003, confidence = 0.5))
pr = sort(rules, decreasing = TRUE, by="lift")
inspect(pr)
#inspect(sort(rules, by = "lift")[1:10])
write(pr, file="top10_itemset.txt")


rules = apriori(data = transaksi_dqlab_retail.tran, parameter = list(support = 0.003, minlen=2, maxlen=3, confidence = 0.1))
rtm=subset(subset(rules, rhs %in% "Tas Makeup", by = 'lift')[1:3])
inspect(rtm)
write(rtm, file="Top3 Tas Makeup.txt")
#inspect(subset(rules, rhs %in% "Tas Makeup", by = 'lift')[1:3])


rules = apriori(data = transaksi_dqlab_retail.tran, parameter = list(support = 0.003, minlen=2, maxlen=3, confidence = 0.1))
rbrpa= subset(rules, rhs %in% "Baju Renang Pria Anak-anak", by = 'lift')[1:3]
inspect(rbrpa)
write(rbrpa, file="Top3 Baju Renang Pria Anak.txt")
#inspect(subset(rules, rhs %in% "Baju Renang Pria Anak-anak", by = 'lift')[1:3])

