###########前進消去関数###########
Forward_elimination <- function (Calculate.Matrix){
  #行列数カウントと方程式の回答位置を指定（列数-1）
  Row.Cnt = nrow(Calculate.Matrix)
  Ans.Point = ncol(Calculate.Matrix) - 1
  
  #現在ピボットとなっている行と列
  Pibot.Row = 1
  Pibot.Col = 1
  
  #行数分(i)前進消去実施
  while(Pibot.Row <= Row.Cnt){
    #ピボットを決め（i行,i列）て単位化する
    Pibot <- Calculate.Matrix[Pibot.Row,Pibot.Col]
    #Pibotが0の場合、行を入れ替えて、Piobtを再取得
    if(Pibot == 0){
      dm.Calculate.Matrix <- Calculate.Matrix
      Calculate.Matrix[Pibot.Row+1,] <- dm.Calculate.Matrix[Pibot.Row,]
      Calculate.Matrix[Pibot.Row,] <- dm.Calculate.Matrix[Pibot.Row+1,]
      Pibot <- Calculate.Matrix[Pibot.Row,Pibot.Col]
    }
    Calculate.Matrix[Pibot.Row,] <- Calculate.Matrix[Pibot.Row,]/Pibot
    
    #残りの行数分繰り返す（Pibotが最後の行じゃない時だけループする）
    if(Pibot.Row != Row.Cnt){
      for(j in (Pibot.Row+1):Row.Cnt){
        #Pibotの下の行が0となるようにPibotを含む行を定数倍した値を引く
        targetNum <- Calculate.Matrix[j,Pibot.Col]
        Calculate.Matrix[j,] <- Calculate.Matrix[j,] - (Calculate.Matrix[Pibot.Row,]*targetNum)
        #定数倍して引いた行が全部0の場合、解なしまたは解が無数にあるので判定する
        if(length(which(Calculate.Matrix[j,1:Ans.Point]==0))==Ans.Point){
          #browser()
          #行すべてが0の場合、解無数に存在
          if(as.numeric(Calculate.Matrix[j,(Ans.Point+1)])==0){
            return("解無数") 
          }else{
            return("解なし") 
          }
        }
      }
    }
    
    #対象となる行と列を前進させる
    Pibot.Row = Pibot.Row + 1
    Pibot.Col = Pibot.Col + 1
  }
  return(Calculate.Matrix)
}

###########後退消去（代入）関数###########
Retreat.Erased <- function(Result.Forward){
  #行列数カウントと方程式の回答位置を指定（列数-1）
  Row.Cnt = nrow(Result.Forward)
  Ans.Point = ncol(Result.Forward) - 1
  #現在ピボットとなっている行と列
  Pibot.Row = Row.Cnt
  Pibot.Col = Ans.Point
  
  #行数分(i)前進消去実施
  while(Pibot.Row>=1){
    #ピボットを決め（i行,i列）て単位化する
    Pibot <- Result.Forward[Pibot.Row,Pibot.Col]
    Result.Forward[Pibot.Row,] <- Result.Forward[Pibot.Row,]/Pibot
    
    #残りの行数分繰り返す（Pibotが最後の行じゃない時だけループする）
    if(Pibot.Row != 1){
      for(j in (Pibot.Row-1):1){
        #Pibotの上の行が0となるように、Pibot行を定数倍した値を引く
        targetNum <- Result.Forward[j,Pibot.Col]
        Result.Forward[j,] <- Result.Forward[j,] - (Result.Forward[Pibot.Row,]*targetNum)
      }
    }
    
    #対象となる行と列を前進させる
    Pibot.Row = Pibot.Row - 1
    Pibot.Col = Pibot.Col - 1
  }
  return(Result.Forward)
}

##############【Main】今回計算する連立方程式を定義##############
#例1．普通のガウス消去
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,6,3,41)
Equation3 <- c(8,8,9,83)
#例2．途中でPibotが0になる場合
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,2,5,39)
Equation3 <- c(8,8,9,83)
#例3．途中でPibotが0になる場合：解なし
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,2,5,39)
Equation3 <- c(8,4,4,83)
#例4．途中でPibotが0になる場合：解無数
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,6,3,41)
Equation3 <- c(2,9,3,37)


##方程式をマトリックスに入れる
Calculate.Matrix <- matrix(0,3,4) 
Calculate.Matrix[1,] <- Equation1
Calculate.Matrix[2,] <- Equation2
Calculate.Matrix[3,] <- Equation3
#class(Calculate.Matrix[1,])

##前進消去
Result.Forward <- Forward_elimination(Calculate.Matrix)
##解があれば後退消去
if(class(Result.Forward) == "matrix"){
  Retreat.Erased(Result.Forward)
}else{
  Result.Forward
}
