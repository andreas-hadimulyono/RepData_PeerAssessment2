calculateDamage <- function(dataframe){
    length <- nrow(dataframe)
    
    propertyDmg <- numeric(length)
    cropDmg <- numeric(length)
    
    for(i in 1:length){
        if (!is.na(dataframe[i,"PROPDMG"])){
            if (dataframe[i,"PROPDMGEXP"] == 'B'){
                propertyDmg[i] = dataframe[i,"PROPDMG"] * 1000000000
            } else if(dataframe[i,"PROPDMGEXP"] == 'h' | dataframe[i,"PROPDMGEXP"] == 'H') {
                propertyDmg[i] = dataframe[i,"PROPDMG"] * 100
            } else if(dataframe[i,"PROPDMGEXP"] == 'm' | dataframe[i,"PROPDMGEXP"] == 'M') {
                propertyDmg[i] = dataframe[i,"PROPDMG"] * 1000000
            } else if(dataframe[i,"PROPDMGEXP"] == 'K') {
                propertyDmg[i] = dataframe[i,"PROPDMG"] * 1000
            } else {
                propertyDmg[i] = 0
            }            
        } else {
            propertyDmg[i] = 0
        }
        
        if (!is.na(dataframe[i,"CROPDMG"])){
            if (dataframe[i,"CROPDMGEXP"] == 'B'){
                cropDmg[i] = dataframe[i,"CROPDMG"] * 1000000000
            } else if(dataframe[i,"CROPDMGEXP"] == 'h' | dataframe[i,"CROPDMGEXP"] == 'H') {
                cropDmg[i] = dataframe[i,"CROPDMG"] * 100
            } else if(dataframe[i,"CROPDMGEXP"] == 'm' | dataframe[i,"CROPDMGEXP"] == 'M') {
                cropDmg[i] = dataframe[i,"CROPDMG"] * 1000000
            } else if(dataframe[i,"CROPDMGEXP"] == 'K') {
                cropDmg[i] = dataframe[i,"CROPDMG"] * 1000
            } else {
                cropDmg[i] = 0
            }            
        } else {
            cropDmg[i] = 0
        }
    }
    
    totalDmg <- propertyDmg + cropDmg;
    
    
    return(totalDmg)
}