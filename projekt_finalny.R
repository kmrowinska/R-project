library(Hmisc)
library(purrr)
library(dplyr)
library(rlang)
library(dunn.test)
library(FSA)
library(dplyr)
library(ggpubr)

setwd("C:/Users/kinga/Desktop")
dane_med <- read.csv2(file = "przykladoweDane-Projekt.csv")
#print(dane_med)

#punkt 1
cat('Zastêpujemy braki danych w naszej tabeli œredni¹.\n')
for (i in 1:ncol(dane_med)) {
  if(sum(is.na(dane_med[,i])) > 0){
    dane_med[ ,i] <- impute(dane_med[,i], mean)
  }
}
#print(dane_med)
cat('Za pomoc¹ wykresów boxplot, graficznie reprezentujemy poszczególne grupy w zale¿noœci od parametru\n')
columns <- ncol(dane_med)
par(mfrow = c(2, columns-4))
for (i in 1:ncol(dane_med)){
  if(is.numeric(dane_med[,i])){
    boxplot.stats(dane_med[,i])$out
    boxplot(dane_med[,i] ~ dane_med[,1] ,
            ylab = 'wartoœci',
            main = paste(colnames(dane_med[i])),
            col = c('#89CFF0', 'pink', '#CA9BF7'))
    
    
  }
  #dev.off()
}

#punkt 2
cat('Charakterystyki dla poszczególnych grup i ich parametrów w strukturze tabelarycznej:\n')
dane_med_p <- split(dane_med, dane_med[,1])
dane_med_p %>% map(summary)

#punkt 3
cat('Ocena zgodnoœci danych z rozk³adem normalnym\n')
names_of_columns <- colnames(dane_med)
y <- parse_expr(names_of_columns[1])
eval(y) #uzywamy dopiero jak wywolujemy dana rzecz

pvaluesS <- list()
#Shapiro (zgodnoœæ z rozk³adem)
for (i in names_of_columns){
  if(is.numeric(dane_med[,i]))
  {
    y <- parse_expr(i)
    pvalueShapiroTest <- group_by(dane_med, dane_med[1]) %>%
      summarise(
        statistic = shapiro.test(eval(y))$statistic,
        p.value = shapiro.test(eval(y))$p.value
      )
    pvalueShapiroTest
    pvaluesS[[i]]<- ifelse(all(pvalueShapiroTest$p.value > 0.05), TRUE, FALSE)
  }
}
pvaluesS

cat('Ocena homogenicznoœci wariancji i testy statystyczne\n')
pvaluesL <- list()
#Levene (jednorodnoœæ wariancji)
for (i in names_of_columns)
  {
  if(is.numeric(dane_med[,i]))
    {
    y <- parse_expr(i)
    x <- parse_expr(names_of_columns[1])
    leveneTest(eval(y) ~ eval(x), data = dane_med)$"Pr(>F)"
    
    pvaluesL[[i]]<- ifelse((leveneTest(eval(y) ~ eval(x), data = dane_med)$"Pr(>F)") > 0.05, TRUE, FALSE)
    
    if(count(unique(dane_med[1])) == 2)
      {
      if(pvaluesS[[i]] == TRUE)
        {
        if(pvaluesL[[i]] == TRUE)
          {
          #cat('test Studenta\Welcha\Wilcoxona\n')
        }
      }
    }else
      {
      if(pvaluesS[[i]] == TRUE)
        {
        if(pvaluesL[[i]] == TRUE)
          {
          cat('test Anova\n')
          pvalueAOVtest <- summary(aov(eval(y) ~ eval(x), data = dane_med))[[1]][["Pr(>F)"]][[1]]
          if(pvalueAOVtest < 0.05)
            {
            cat(pvalueAOVtest, "< 0.05 - wystêpuj¹ ró¿nice miêdzy grupami\n")
            cat('Ró¿nice:\n')
            print(TukeyHSD(aov(dane_med[,i] ~ dane_med[,1], data = dane_med)))
          }else
            {
            cat(pvalueAOVtest, "> 0.05 - brak ró¿nic miêdzy grupami\n")
          }
        }else
          {
          cat('test Kruskala\n', )
          pvalueKWtest <- kruskal.test(eval(y) ~ eval(x), data = dane_med)$p.value
          if(pvalueKWtest < 0.05)
            {
            cat(pvalueKWtest, '< 0.05 - wystêpuj¹ ró¿nice miêdzy grupami\n')
            cat('Ró¿nice:\n')
            print(dunnTest(as.numeric(dane_med[,i]), dane_med[,1]))
          }else
            {
            cat(pvalueKWtest, '> 0.05 - brak ró¿nic miêdzy grupami\n')
          }
        }
      }else
        {
        cat('test Kruskala\n')
        pvalueKWtest <- kruskal.test(eval(y) ~ eval(x), data = dane_med)$p.value
        if(pvalueKWtest < 0.05)
          {
          cat(pvalueKWtest, '< 0.05 - wystêpuj¹ ró¿nice miêdzy grupami\n')
          cat('Ró¿nice:\n')
          print(dunnTest(as.numeric(dane_med[,i]), dane_med[,1]))
        }else
          {
          cat(pvalueKWtest, '> 0.05 - brak ró¿nic miêdzy grupami\n')
        }
      }
    }
    
  }
}

#punkt 4
testyKOR <- split(dane_med, dane_med[,1])
cat('Ocena zale¿noœci miêdzy parametrami\n')
cat('Test korelacji Pearsona\n')
pdf('C:/Users/kinga/Desktop/wykresy.pdf')
for (dane_test in testyKOR)
{
  for (i in 1:ncol(dane_test)-1)
  {
    for (j in (i+1:ncol(dane_test[i])))
      {
      if(is.numeric(dane_test[,i]))
      {
        korelacja <- cor.test(dane_test[,i], dane_test[,j], method = 'pearson')
        cat('Korelacja miêdzy grup¹', colnames(dane_test[i]), 'a grup¹', colnames(dane_test[j]), '\n')
        cat('korelacja: ',korelacja$estimate, '\n')
        cat('pvalue:',korelacja$p.value, '\n')
        
        pvalueKOR <- korelacja$p.value
        cat('Wspó³czynnik korelacji: ')
        if(pvalueKOR > 0){cat('korelacja dodatnia – gdy zmienna X roœnie to Y tak¿e roœnie\n')}
        if(pvalueKOR == 0){cat('brak korelacji – gdy zmienna X roœnie to Y czasem roœnie a czasem maleje\n')}
        if(pvalueKOR < 0){cat('korelacja ujemna – gdy zmienna X roœnie to Y maleje\n')}

        cat('Si³a korelacji: ')
        if(-1 < pvalueKOR & pvalueKOR < -0.7){cat('bardzo silna korelacja ujemna\n')}
        if(-0.7 < pvalueKOR & pvalueKOR < -0.5){cat('silna korelacja ujemna\n')}
        if(-0.5 < pvalueKOR & pvalueKOR < -0.3){cat('korelacja ujemna o œrednim natê¿eniu\n')}
        if(-0.3 < pvalueKOR & pvalueKOR < -0.2){cat('s³aba korelacja ujemna\n')}
        if(-0.2 < pvalueKOR & pvalueKOR < 0.2){cat('brak korelacji\n')}
        if(0.2 < pvalueKOR & pvalueKOR < 0.3){cat('s³aba korelacja dodatnia\n')}
        if(0.3 < pvalueKOR & pvalueKOR < 0.5){cat('korelacja dodatnia o œrednim natê¿eniu\n')}
        if(0.5 < pvalueKOR & pvalueKOR < 0.7){cat('silna korelacja dodatnia\n')}
        if(0.7 < pvalueKOR & pvalueKOR < 1){cat('bardzo silna korelacja dodatnia\n')}
        cat('\n')
        
        
        wykres <- ggscatter(dane_test, x=colnames(dane_test[i]), y=colnames(dane_test[j]),
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = 'pearson',
                  color = colnames(dane_test[1]), fill = colnames(dane_test[1]),
                  ylab = colnames(dane_test[i]),
                  xlab = colnames(dane_test[j])
        )
        print(wykres)
      }
    }
  }
}
dev.off()


