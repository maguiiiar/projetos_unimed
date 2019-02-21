require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

fev15uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "075523")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

abril17uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01397033","0530342","0566848","0567347","0294278")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

setanov17uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "47223","0953751","01147871","01274690","01116448","01012455",
  "01279602","01309586","0378451","01373006","01360937","01039477",
  "01360948","0929298","01392055","01309608","0286864","01032765",
  "01101782","0725326","01373009","01327330","052279","01345891",
  "057766","0823296","0186268","01309635","01309644","0860636",
  "041920","0668061","01069005","01309563","0349606","01099136",
  "01125904","0530054","01125928","01182776","01125670","01125647",
  "0305185","021679","01263498","01125621","0244108","0244109",
  "01125623","01125660","0700981","01255323","01125665","01125666",
  "01243005","01125658","011258275","01125612","01125881")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

jan18uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01125816","01125831")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

abrmai18uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "0962918","0962933","0962928","0962936","0962942","0962917",
  "072878","0562494","0125903","0125905","01372426")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

ago18uni <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01397033")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

benef.cias <- fread("C:/Users/mrrezende/Documents/benef_cias.txt",
                    colClasses = c("Beneficiario Codigo" = "character"))

dez16colab<- inner_join(despesas.final,benef.cias,
                        by = "Beneficiario Codigo") %>% group_by(
                          Competencia) %>% summarise(Total = sum(valor))

algar.cias <- fread("C:/Users/mrrezende/Documents/algar_cias.txt",
                    colClasses = c("Beneficiario Codigo" = "character"))

abr15alg <- inner_join(despesas.final,algar.cias,
                       by = "Beneficiario Codigo") %>% group_by(
                         Competencia) %>% summarise(Total = sum(valor))

junaago15alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "0106400","01074949","01127155",
  "01204071","038630","01094264","01099699")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

outadez15alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01026235","01099261","01238488",
  "01230605","01103389","01189033")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

fevmar16alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01090111","023653","0403979","0997873")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

maiaago16alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "0350209","01046636","0310527",
  "01161272","0220727","0726108")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

novdez16alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "0178147","01168070","01122656")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

janamar17alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01200828","01090943","057068","01019976")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

abrmai17alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01148147","0561937","0793707","01127632","01129183",
  "0732648","0666229","01123250","01200651","01015031")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

agoset17alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01014729","0607758","01015709")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

novdez17alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01095743","01061095","01074979","0235218")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

janamar18alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01140505","0176907","01180417"," 01244469","01031451",
  "030168","01014026","01134021","01200968")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

abrajun18alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01135805","0962931","01011283","01134081","01108087",
  "0174108","01154563","01216215","01166386","01067573",
  "01103420","0317995","01103113")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

julset18alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "0994105","054258","01136835","01103021",
  "0970425","01137639","01151159")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

outadez18alg <- despesas.final %>% filter(`Beneficiario Codigo` %in% c(
  "01215940","0830595","0573795","0203904","01105172",
  "01178952","0841099","01017660","01017664","0583962",
  "01115656","01115035","01227479")) %>% group_by(
    Competencia) %>% summarise(Total = sum(valor))

fwrite(setanov17uni, file = "C:/Users/mrrezende/Documents/SerieUNI.csv",
       sep = "|")
