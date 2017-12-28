###########�O�i�����֐�###########
Forward_elimination <- function (Calculate.Matrix){
  #�s�񐔃J�E���g�ƕ������̉񓚈ʒu���w��i��-1�j
  Row.Cnt = nrow(Calculate.Matrix)
  Ans.Point = ncol(Calculate.Matrix) - 1
  
  #���݃s�{�b�g�ƂȂ��Ă���s�Ɨ�
  Pibot.Row = 1
  Pibot.Col = 1
  
  #�s����(i)�O�i�������{
  while(Pibot.Row <= Row.Cnt){
    #�s�{�b�g�����߁ii�s,i��j�ĒP�ʉ�����
    Pibot <- Calculate.Matrix[Pibot.Row,Pibot.Col]
    #Pibot��0�̏ꍇ�A�s�����ւ��āAPiobt���Ď擾
    if(Pibot == 0){
      dm.Calculate.Matrix <- Calculate.Matrix
      Calculate.Matrix[Pibot.Row+1,] <- dm.Calculate.Matrix[Pibot.Row,]
      Calculate.Matrix[Pibot.Row,] <- dm.Calculate.Matrix[Pibot.Row+1,]
      Pibot <- Calculate.Matrix[Pibot.Row,Pibot.Col]
    }
    Calculate.Matrix[Pibot.Row,] <- Calculate.Matrix[Pibot.Row,]/Pibot
    
    #�c��̍s�����J��Ԃ��iPibot���Ō�̍s����Ȃ����������[�v����j
    if(Pibot.Row != Row.Cnt){
      for(j in (Pibot.Row+1):Row.Cnt){
        #Pibot�̉��̍s��0�ƂȂ�悤��Pibot���܂ލs��萔�{�����l������
        targetNum <- Calculate.Matrix[j,Pibot.Col]
        Calculate.Matrix[j,] <- Calculate.Matrix[j,] - (Calculate.Matrix[Pibot.Row,]*targetNum)
        #�萔�{���Ĉ������s���S��0�̏ꍇ�A���Ȃ��܂��͉��������ɂ���̂Ŕ��肷��
        if(length(which(Calculate.Matrix[j,1:Ans.Point]==0))==Ans.Point){
          #browser()
          #�s���ׂĂ�0�̏ꍇ�A�𖳐��ɑ���
          if(as.numeric(Calculate.Matrix[j,(Ans.Point+1)])==0){
            return("�𖳐�") 
          }else{
            return("���Ȃ�") 
          }
        }
      }
    }
    
    #�ΏۂƂȂ�s�Ɨ��O�i������
    Pibot.Row = Pibot.Row + 1
    Pibot.Col = Pibot.Col + 1
  }
  return(Calculate.Matrix)
}

###########��ޏ����i����j�֐�###########
Retreat.Erased <- function(Result.Forward){
  #�s�񐔃J�E���g�ƕ������̉񓚈ʒu���w��i��-1�j
  Row.Cnt = nrow(Result.Forward)
  Ans.Point = ncol(Result.Forward) - 1
  #���݃s�{�b�g�ƂȂ��Ă���s�Ɨ�
  Pibot.Row = Row.Cnt
  Pibot.Col = Ans.Point
  
  #�s����(i)�O�i�������{
  while(Pibot.Row>=1){
    #�s�{�b�g�����߁ii�s,i��j�ĒP�ʉ�����
    Pibot <- Result.Forward[Pibot.Row,Pibot.Col]
    Result.Forward[Pibot.Row,] <- Result.Forward[Pibot.Row,]/Pibot
    
    #�c��̍s�����J��Ԃ��iPibot���Ō�̍s����Ȃ����������[�v����j
    if(Pibot.Row != 1){
      for(j in (Pibot.Row-1):1){
        #Pibot�̏�̍s��0�ƂȂ�悤�ɁAPibot�s��萔�{�����l������
        targetNum <- Result.Forward[j,Pibot.Col]
        Result.Forward[j,] <- Result.Forward[j,] - (Result.Forward[Pibot.Row,]*targetNum)
      }
    }
    
    #�ΏۂƂȂ�s�Ɨ��O�i������
    Pibot.Row = Pibot.Row - 1
    Pibot.Col = Pibot.Col - 1
  }
  return(Result.Forward)
}

##############�yMain�z����v�Z����A�����������`##############
#��1�D���ʂ̃K�E�X����
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,6,3,41)
Equation3 <- c(8,8,9,83)
#��2�D�r����Pibot��0�ɂȂ�ꍇ
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,2,5,39)
Equation3 <- c(8,8,9,83)
#��3�D�r����Pibot��0�ɂȂ�ꍇ�F���Ȃ�
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,2,5,39)
Equation3 <- c(8,4,4,83)
#��4�D�r����Pibot��0�ɂȂ�ꍇ�F�𖳐�
Equation1 <- c(2,1,1,15)
Equation2 <- c(4,6,3,41)
Equation3 <- c(2,9,3,37)


##���������}�g���b�N�X�ɓ����
Calculate.Matrix <- matrix(0,3,4) 
Calculate.Matrix[1,] <- Equation1
Calculate.Matrix[2,] <- Equation2
Calculate.Matrix[3,] <- Equation3
#class(Calculate.Matrix[1,])

##�O�i����
Result.Forward <- Forward_elimination(Calculate.Matrix)
##��������Ό�ޏ���
if(class(Result.Forward) == "matrix"){
  Retreat.Erased(Result.Forward)
}else{
  Result.Forward
}