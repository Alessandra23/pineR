load("~/Documents/GitHub/pineR/Data/Weighted Data/hortlandData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/ballinageeData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/OakwoodData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Glendine1Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Glendine2Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Lackenrea1Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Lackenrea2Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/SummerhillData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/DeerparkData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/BallymacshaneboyData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/KilduffData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/RossnagadData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Ballyroan1Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/Ballyroan2Data.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/DonadeaData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/CloondaraData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/KnockavilleData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/KilurneyData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/DoonData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/ClonoghilData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/TigroneyData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/GurtnapishaData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/RickardstownData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/LongfordpassData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/CashelduffData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/WoodfordData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/BallybrittasData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/CorracloonData.Rda")
load("~/Documents/GitHub/pineR/Data/Weighted Data/CorrakyleData.Rda")


hortlandData$temp %>% ggplot(aes(y = temp)) + geom_boxplot() + theme_bw()


temps <- list(hortlandData$temp,
        ballinageeData$temp ,
        OakwoodData$temp,
        Glendine1Data$temp,
        Glendine2Data$temp,
        Lackenrea1Data$temp,
        Lackenrea2Data$temp,
        SummerhillData$temp,
        DeerparkData$temp,
        BallymacshaneboyData$temp,
        KilduffData$temp,
        RossnagadData$temp,
        Ballyroan1Data$temp,
        Ballyroan2Data$temp,
        DonadeaData$temp,
        CloondaraData$temp,
        KnockavilleData$temp,
        KilurneyData$temp,
        DoonData$temp,
        ClonoghilData$temp,
        TigroneyData$temp,
        GurtnapishaData$temp,
        RickardstownData$temp,
        LongfordpassData$temp,
        CashelduffData$temp,
        WoodfordData$temp,
        BallybrittasData$temp,
        CorracloonData$temp,
        CorrakyleData$temp
)
nn <-  lapply(temps, length)
names <- lapply(1:length(nn), function(i){rep(unique(summaryData$Site)[i], nn[[i]])})
MeanTemp <- do.call(rbind, Map(data.frame, Site=names, Temperature=temps))

MeanTemp %>% ggplot(aes(y=Site, x=Temperature)) + geom_boxplot(fill = "#E69F00", color = "black") + theme_bw()
