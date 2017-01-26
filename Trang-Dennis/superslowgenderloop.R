#about gender 

length = nrow(tangram)
tangram <- tangramfilter
tangram$factor_gender <- 0
for (i in 1:length){
  check <- tangram[i,]
 if (tolower(strtrim(check$Factor1,3)) %in% gen |tolower(strtrim(check$Factor2,3)) %in% gen|tolower(strtrim(check$Factor3,3)) %in% gen){
   tangram[i,]$factor_gender = 1
   } else {
   tangram[i,]$factor_gender = 0
   }
}
  
  
  
  
  if (tolower(strtrim(check$Factor2,3)) %in% gen){
    tangram$factor_gender = 1
  } else {
    tangram$factor_gender = 0
  }
  if (tolower(strtrim(check$Factor2,3)) %in% gen){
    tangram$factor_gender = 1
  } else {
    tangram$factor_gender = 0
  }
  
  
if (tolower(strtrim(check$Level1,1)) == "m" | tolower(strtrim(check$Level1,1)) == "h"){
  tangram[i,]$gender = "M"
}
if (tolower(strtrim(check$Level1,1)) == "f" | tolower(strtrim(check$Level1,2)) == "mu"){
  tangram[i,]$gender = "F"
}
  



