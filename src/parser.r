#!/usr/bin/Rscript 
TABLE_HEADER_VOMIT <- "#Patient_id,SEX,Age.at.Dx,AHD,PRIOR.MAL,PRIOR.CHEMO,PRIOR.XRT,Infection,ITD,D835,Ras.Stat,Chemo.Simplest,WBC,ABS.BLST,BM.BLAST,BM.MONOCYTES,BM.PROM,PB.BLAST,PB.MONO,PB.PROM,HGB,PLT,LDH,ALBUMIN,BILIRUBIN,CREATININE,FIBRINOGEN,CD13,CD33,CD34,CD7,CD10,CD20,HLA.DR,CD19,ACTB,AIFM1,AKT1,AKT1_2_3.pS473,AKT1_2_3.pT308,ARC,ASH2L,ASNS,ATF3,ATG7,BAD,BAD.pS112,BAD.pS136,BAD.pS155,BAK1,BAX,BCL2,BCL2L1,BCL2L11,BECN1,BID,BIRC2,BIRC5,BMI1,BRAF,CASP3,CASP3.cl175,CASP7.cl198,CASP8,CASP9,CASP9.cl315,CASP9.cl330,CAV1,CBL,CCNB1,CCND1,CCND3,CCNE1,CCNE2,CD44,CD74,CDK1,CDK2,CDK4,CDKN1A,CDKN2A,CLPP,COPS5,CREB1,CREB1.pS133,CTNNA1,CTNNB1,CTNNB1.pS33_37_41,CTSG,DIABLO,DLX1,DUSP6,EGFR,EGFR.pY992,EGLN1,EIF2AK2,EIF2AK2.pT451,EIF2S1,EIF2S1.pS51.,EIF4E,ELK1.pS383,ERBB2,ERBB2.pY1248,ERBB3,ERG,Fli1,FN1,FOXO1.pT24_FOXO3.pT32,FOXO3,FOXO3.S318_321,GAB2,GAB2.pY452,GAPDH,GATA1,GATA3,GRP78,GSKA_B,GSKA_B.pS21_9,H3histon,H3K27Me3,H3K4Me2,H3K4Me3,HDAC1,HDAC2,HDAC3,HIF1A,HNRNPK,HSP90AA1_B1,HSPA1A_L,HSPB1,IGF1R,IGFBP2,INPP5D,INPPL1,IRS1.pS1101,ITGA2,ITGAL,ITGB3,JMJD6,JUNB,JUN.pS73,KDR,KIT,LCK,LEF1,LGALS3,LSD1,LYN,MAP2K1,MAP2K1_2.pS217_221,MAPK1,MAPK1_3.pT202Y204,MAPK14,MAPK14.pT180Y182,MAPK9,MAPT,MCL1,MDM2,MDM4,MET.pY1230_1234_1235,MSI2,MTOR,MTOR.pS2448,MYC,NCL,NF2,NF2.pS518,NOTCH1.cl1744,NOTCH3,NPM1,NPM1.3542,NR4A1,NRP1,ODC1,PA2G4,PA2G4.pS65,PA2G4.pT37_46,PA2G4.pT70,PARK7,PARP1,PARP1.cl214,PDK1,PDK1.pS241,PIK3CA,PIK3R1_2,PIM1,PIM2,PLAC1,PPARA,PPARG,PPP2R2A_B_C_D,PRKAA1_2,PRKAA1_2.pT172,PRKCA,PRKCA.pS657,PRKCB.I,PRKCB.II,PRKCD.pS645,PRKCD.pS664,PRKCD.pT507,CDKN1B,CDKN1B.pS10,PTEN,PTEN.pS380T382T383,PTGS2,PTK2,PTPN11,RAC1_2_3,RB1,RB1.pS807_811,RELA,RPS6,RPS6KB1,RPS6KB1.pT389,RPS6.pS235_236,RPS6.pS240_244,SFN,SIRT1,SMAD1,SMAD2,SMAD2.pS245,SMAD2.pS465,SMAD3,SMAD4,SMAD5,SMAD5.pS463,SMAD6,SOCS2,SPI1,SPP1,SQSTM0,SRC,SRC.pY416,SRC.pY527,SSBP2,STAT1,STAT1.pY701,STAT3,STAT3.pS727,STAT3.pY705,STAT5A_B,STAT5A_B.pY694,STAT6.pY641,STK11,STMN1,TAZ,TAZ.pS89,TCF4,TGM2,TNK1,TP53,TP53.pS15,TRIM24,TRIM62,TSC2,VASP,VHL,WTAP,XIAP,XPO1,YAP1,YAP1p,YWHAE,YWHAZ,ZNF296,ZNF346"

rawInput <- readLines(file("stdin"))
rawInput <- gsub('\r', '', rawInput)
rawInput <- gsub('\t', ',', rawInput)

x <- TABLE_HEADER_VOMIT
for(row in rawInput){
    x <- paste(x, row, sep="\n")
}

con <- textConnection(x)
data <- read.csv(con)
close(con)

print(data)
