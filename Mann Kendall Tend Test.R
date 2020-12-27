rm(list = ls())
#設定資料讀取路徑
setwd("D:/資料分析彙整/Project/2016 Taiwan Map")
#資料來源名稱
filename <- "Mann_Kendall_T"
#資料格式：預設列為不同的傷害數據，欄為年份資料，資料檔不含變項名稱?W??
input.data.mx <- as.matrix(read.csv(paste("raw data/",filename,".csv",sep=""), header=FALSE))

compute.data.mx <- matrix(NA, nrow=dim(input.data.mx)[2], ncol=dim(input.data.mx)[2])

create.diff.mx <- function(data.input){
  for(length.data in 1:(length(data.input)-1)){
    compute.data.mx[length.data,-c(1:length.data)] <- data.input[-c(1:length.data)]-data.input[length.data]
  }
  
  positive.nu <- apply(compute.data.mx, 1, function(x) length(which(x>0)))
  negative.nu <- apply(compute.data.mx, 1, function(x) length(which(x<0)))
  compute.data.mx <- cbind(compute.data.mx, positive.nu, negative.nu)
  return(compute.data.mx)
}

statistic.mx <- function(data.mx){
  nu.positive.diffs <- sum(data.mx[,"positive.nu"])
  nu.negative.diffs <- sum(data.mx[,"negative.nu"])
  Sign.diff <- nu.positive.diffs-nu.negative.diffs
  n <- dim(data.mx)[1]
  Var.Sign <- n*(n-1)*(2*n+5)/18
  
  if(n>=10){
    if(Sign.diff>0){
      Z.obs <- (Sign.diff-1)/sqrt(Var.Sign)
    }else if(Sign.diff<0){
      Z.obs <- (Sign.diff+1)/sqrt(Var.Sign)
    }else{
      Z.obs <- 0
    }
    
    if(Z.obs<0){
      p.value <- 2*pnorm(Z.obs)
      
      if(p.value<0.01){
        p.value <- "<0.01"
      }
      
      if(p.value<0.05 & p.value>=0.01){
        tend <- "Decreasing"
      }else if(p.value<0.01){
        tend <- "More Decreasing"
      }else{
        tend <- "No Trend"
      }
    }else{
      p.value <- 2*(1-pnorm(Z.obs))
      
      if(p.value<0.01){
        p.value <- "<0.01"
      }
      
      if(p.value<0.05 & p.value>=0.01){
        tend <- "Increasing"
      }else if(p.value<0.01){
        tend <- "More Increasing"
      }else{
        tend <- "No Trend"
      }
    }
  }else{
    if(n==5){
      if(abs(Sign.diff)>=10){
        p.value <- "<0.05"
        if(Sign.diff>0){
          tend <- "Increasing"
        }else{
          tend <- "Decreasing"
        }
      }else{
        p.value <- "N/A"
        tend <- "N/A"
      }
    }else if(n==6){
      if(abs(Sign.diff)>=13){
        p.value <- "<0.05"
        if(Sign.diff>0){
          tend <- "Increasing"
        }else{
          tend <- "Decreasing"
        }
      }else{
        p.value <- "N/A"
        tend <- "N/A"
      }
    }else if(n==7){
      if(abs(Sign.diff)>=15){
        p.value <- "<0.05"
        if(Sign.diff>0){
          tend <- "Increasing"
        }else{
          tend <- "Decreasing"
        }
      }else{
        p.value <- "N/A"
        tend <- "N/A"
      }
    }else if(n==8){
      if(abs(Sign.diff)>=18){
        p.value <- "<0.05"
        if(Sign.diff>0){
          tend <- "Increasing"
        }else{
          tend <- "Decreasing"
        }
      }else{
        p.value <- "N/A"
        tend <- "N/A"
      }
    }else if(n==9){
      if(abs(Sign.diff)>=20){
        p.value <- "<0.05"
        if(Sign.diff>0){
          tend <- "Increasing"
        }else{
          tend <- "Decreasing"
        }
      }else{
        p.value <- "N/A"
        tend <- "N/A"
      }
    }else{
      p.value <- "N/A"
      tend <- "N/A"
    }
    Z.obs <- "N/A"
  }
  
  return(c(Sign.diff, Var.Sign, Z.obs, p.value, tend))
}

result.mx <- matrix(NA, nrow=dim(input.data.mx)[1], ncol=5)
                    
for(raw.number in 1:dim(input.data.mx)[1]){
  data.input <- as.vector(input.data.mx[raw.number,])
  data.mx <- create.diff.mx(data.input)
  result.mx[raw.number,] <- statistic.mx(data.mx)
}

colnames(result.mx) <- c("S", "Var(S)", "Z of S", "p-value", "trend")
write.csv(result.mx, paste("output/",filename,"_output.csv",sep=""))

