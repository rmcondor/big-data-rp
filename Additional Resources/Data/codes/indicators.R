## Creación de indicadores

# Se define el espacio de trabajo seleccionando el directorio con la ubicación de la base de datos
setwd("M:/Mi unidad/Master Program/Cursos/Elective Courses/Big data/Research Proposal/Data")

# Se lee la base de datos de la ENDIREH 2021, como un archivo de datos de R
load("raw/bd_endireh_2021_rdata/bd_endireh_2021.RData")

library(ggplot2)
library(dplyr)
library(haven)
library(survey) # Librería para el cálculo de precisiones estadísticas
library(openxlsx) # Librería para importar/exportar archivos en formato .xlsx

# Seleccionamos las variables necesarias para la construcción de los indicadores, a partir de la base de
# datos del cuestionario principal de la mujer elegida “TB_SEC_IVaVD"
P7_6<-paste0("P7_6_",1:18); P7_8<-paste0("P7_8_",1:18) # Escolar
P8_9<-paste0("P8_9_",1:19); P8_11<-paste0("P8_11_",1:19) # Laboral
P8_8<-paste0("P8_8_",1:9) # Discriminación en los últimos 12 meses
P9_1<-paste0("P9_1_",1:16); P9_3<-paste0("P9_3_",1:16) # Comunitario
P11_1<-paste0("P11_1_",1:20) # Familiar
P14_1<-paste0("P14_1_",1:38); P14_1[c(23,24,35:38)]<-paste0(P14_1[c(23,24,35:38)],"AB") # Pareja
P14_3<-paste0("P14_3_",1:38); P14_3[c(23,24,35:38)]<-paste0(P14_3[c(23,24,35:38)],"AB") # Pareja
variables<-c("ID_VIV", "ID_PER", "UPM", "VIV_SEL", "HOGAR", "N_REN", "DOMINIO", "NOM_ENT",
 "UPM_DIS","EST_DIS","FAC_MUJ","CVE_ENT","T_INSTRUM","P7_1","P7_2",P7_6,P7_8,
 "P8_1","P8_2","P8_3_1_1","P8_3_1_2","P8_3_2_1","P8_3_2_2","P8_3_2_3",
 "P8_4","P8_5",P8_9,P8_11,P8_8,P9_1,P9_3,P11_1,"P13_C_1",P14_1,P14_3
)
muj<-TB_SEC_IVaVD[,variables]



# Violencia total ---------------------------------------------------------


#* Prevalencia de violencia total contra las mujeres de 15 años y más a lo largo de la vida *#
# Construcción de variables #
# Mujeres de 15 años y más que experimentaron al menos un acto de violencia
# a lo largo de la vida
muj$vtot_lv_con <- ifelse(
  (muj$P7_6_1%in%'1' | muj$P7_6_2%in%'1' | muj$P7_6_3%in%'1' |
     muj$P7_6_4%in%'1' | muj$P7_6_5%in%'1' | muj$P7_6_6%in%'1' |
     muj$P7_6_7%in%'1' | muj$P7_6_8%in%'1' | muj$P7_6_9%in%'1' |
     muj$P7_6_10%in%'1' | muj$P7_6_11%in%'1' | muj$P7_6_12%in%'1' |
     muj$P7_6_13%in%'1' | muj$P7_6_14%in%'1' | muj$P7_6_15%in%'1' |
     muj$P7_6_16%in%'1' | muj$P7_6_17%in%'1' | muj$P7_6_18%in%'1' |
     muj$P8_3_1_1%in%'1' | muj$P8_3_1_2%in%'1' | muj$P8_3_2_1%in%'1' |
     muj$P8_3_2_2%in%'1' | muj$P8_3_2_3%in%'1' | muj$P8_8_1%in%'1' |
     muj$P8_8_2%in%'1' | muj$P8_8_3%in%'1' | muj$P8_8_4%in%'1' |
     muj$P8_8_5%in%'1' | muj$P8_8_6%in%'1' | muj$P8_8_7%in%'1' |
     muj$P8_8_8%in%'1' | muj$P8_8_9%in%'1' | muj$P8_9_1%in%'1' |
     muj$P8_9_2%in%'1' | muj$P8_9_3%in%'1' | muj$P8_9_4%in%'1' |
     muj$P8_9_5%in%'1' | muj$P8_9_6%in%'1' | muj$P8_9_7%in%'1' |
     muj$P8_9_8%in%'1' | muj$P8_9_9%in%'1' | muj$P8_9_10%in%'1' |
     muj$P8_9_11%in%'1' | muj$P8_9_12%in%'1' | muj$P8_9_13%in%'1' |
     muj$P8_9_14%in%'1' | muj$P8_9_15%in%'1' | muj$P8_9_16%in%'1' |
     muj$P8_9_17%in%'1' | muj$P8_9_18%in%'1' | muj$P8_9_19%in%'1' |
     muj$P9_1_1%in%'1' | muj$P9_1_2%in%'1' | muj$P9_1_3%in%'1' |
     muj$P9_1_4%in%'1' | muj$P9_1_5%in%'1' | muj$P9_1_6%in%'1' |
     muj$P9_1_7%in%'1' | muj$P9_1_8%in%'1' | muj$P9_1_9%in%'1' |
     muj$P9_1_10%in%'1' | muj$P9_1_11%in%'1' | muj$P9_1_12%in%'1' |
     muj$P9_1_13%in%'1' | muj$P9_1_14%in%'1' | muj$P9_1_15%in%'1' |
     muj$P9_1_16%in%'1' |
     muj$P11_1_1%in%c( '1','2','3') | muj$P11_1_2%in%c( '1','2','3') |
     muj$P11_1_3%in%c( '1','2','3') | muj$P11_1_4%in%c( '1','2','3') |
     muj$P11_1_5%in%c( '1','2','3') | muj$P11_1_6%in%c( '1','2','3') |
     muj$P11_1_7%in%c( '1','2','3') | muj$P11_1_8%in%c( '1','2','3') |
     muj$P11_1_9%in%c( '1','2','3') | muj$P11_1_10%in%c( '1','2','3') |
     muj$P11_1_11%in%c( '1','2','3') | muj$P11_1_12%in%c( '1','2','3') |
     muj$P11_1_13%in%c( '1','2','3') | muj$P11_1_14%in%c( '1','2','3') |
     muj$P11_1_15%in%c( '1','2','3') | muj$P11_1_16%in%c( '1','2','3') |
     muj$P11_1_17%in%c( '1','2','3') | muj$P11_1_18%in%c( '1','2','3') |
     muj$P11_1_19%in%c( '1','2','3') | muj$P11_1_20%in%c( '1','2','3') |
     muj$P14_1_1%in%c( '1','2','3') | muj$P14_1_2%in%c( '1','2','3') |
     muj$P14_1_3%in%c( '1','2','3') | muj$P14_1_4%in%c( '1','2','3') |
     muj$P14_1_5%in%c( '1','2','3') | muj$P14_1_6%in%c( '1','2','3') |
     muj$P14_1_7%in%c( '1','2','3') | muj$P14_1_8%in%c( '1','2','3') |
     muj$P14_1_9%in%c( '1','2','3') | muj$P14_1_10%in%c( '1','2','3') |
     muj$P14_1_11%in%c( '1','2','3') | muj$P14_1_12%in%c( '1','2','3') |
     muj$P14_1_13%in%c( '1','2','3') | muj$P14_1_14%in%c( '1','2','3') |
     muj$P14_1_15%in%c( '1','2','3') | muj$P14_1_16%in%c( '1','2','3') |
     muj$P14_1_17%in%c( '1','2','3') | muj$P14_1_18%in%c( '1','2','3') |
     muj$P14_1_19%in%c( '1','2','3') | muj$P14_1_20%in%c( '1','2','3') |
     muj$P14_1_21%in%c( '1','2','3') | muj$P14_1_22%in%c( '1','2','3') |
     muj$P14_1_23AB%in%c( '1','2','3') | muj$P14_1_24AB%in%c( '1','2','3') |
     muj$P14_1_25%in%c( '1','2','3') | muj$P14_1_26%in%c( '1','2','3') |
     muj$P14_1_27%in%c( '1','2','3') | muj$P14_1_28%in%c( '1','2','3') |
     muj$P14_1_29%in%c( '1','2','3') | muj$P14_1_30%in%c( '1','2','3') |
     muj$P14_1_31%in%c( '1','2','3') | muj$P14_1_32%in%c( '1','2','3') |
     muj$P14_1_33%in%c( '1','2','3') | muj$P14_1_34%in%c( '1','2','3') |
     muj$P14_1_35AB%in%c( '1','2','3') | muj$P14_1_36AB%in%c( '1','2','3') |
     muj$P14_1_37AB%in%c( '1','2','3') | muj$P14_1_38AB%in%c( '1','2','3')),1,0)


#* Prevalencia de violencia total contra las mujeres de 15 años y más en los últimos 12 meses *#
# Construcción de variables #
# Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
# en los últimos 12 meses
muj$vtot_12m_con <- ifelse(
  (muj$P7_8_1%in%c( '1','2','3') | muj$P7_8_2%in% c( '1','2','3') |
     muj$P7_8_3%in%c( '1','2','3') | muj$P7_8_4%in% c( '1','2','3') |
     muj$P7_8_5%in%c( '1','2','3') | muj$P7_8_6%in% c( '1','2','3') |
     muj$P7_8_7%in%c( '1','2','3') | muj$P7_8_8%in% c( '1','2','3') |
     muj$P7_8_9%in%c( '1','2','3') | muj$P7_8_10%in% c( '1','2','3') |
     muj$P7_8_11%in%c( '1','2','3') | muj$P7_8_12%in% c( '1','2','3') |
     muj$P7_8_13%in%c( '1','2','3') | muj$P7_8_14%in% c( '1','2','3') |
     muj$P7_8_15%in%c( '1','2','3') | muj$P7_8_16%in% c( '1','2','3') |
     muj$P7_8_17 %in%c( '1','2','3') | muj$P7_8_18%in% c( '1','2','3') |
     muj$P8_8_1%in%'1'| muj$P8_8_2%in% '1'| muj$P8_8_3%in% '1'|
     muj$P8_8_4%in%'1'| muj$P8_8_5%in% '1'| muj$P8_8_6%in% '1'|
     muj$P8_8_7%in%'1'| muj$P8_8_8%in% '1'| muj$P8_8_9%in% '1'|
     muj$P8_11_1%in%c( '1','2','3') | muj$P8_11_2%in% c( '1','2','3') |
     muj$P8_11_3%in%c( '1','2','3') | muj$P8_11_4%in% c( '1','2','3') |
     muj$P8_11_5%in%c( '1','2','3') | muj$P8_11_6%in% c( '1','2','3') |
     muj$P8_11_7%in%c( '1','2','3') | muj$P8_11_8%in% c( '1','2','3') |
     muj$P8_11_9%in%c( '1','2','3') | muj$P8_11_10%in% c( '1','2','3') |
     muj$P8_11_11%in%c( '1','2','3') | muj$P8_11_12%in% c( '1','2','3') |
     muj$P8_11_13%in%c( '1','2','3') | muj$P8_11_14%in% c( '1','2','3') |
     muj$P8_11_15%in%c( '1','2','3') | muj$P8_11_16%in% c( '1','2','3') |
     muj$P8_11_17%in%c( '1','2','3') | muj$P8_11_18%in% c( '1','2','3') |
     muj$P8_11_19%in%c( '1','2','3') | muj$P9_3_1%in% c( '1','2','3') |
     muj$P9_3_2%in%c( '1','2','3') | muj$P9_3_3%in% c( '1','2','3') |
     muj$P9_3_4%in%c( '1','2','3') | muj$P9_3_5%in% c( '1','2','3') |
     muj$P9_3_6%in%c( '1','2','3') | muj$P9_3_7%in% c( '1','2','3') |
     muj$P9_3_8%in%c( '1','2','3') | muj$P9_3_9%in% c( '1','2','3') |
     muj$P9_3_10%in%c( '1','2','3') | muj$P9_3_11%in% c( '1','2','3') |
     muj$P9_3_12%in%c( '1','2','3') | muj$P9_3_13%in% c( '1','2','3') |
     muj$P9_3_14%in%c( '1','2','3') | muj$P9_3_15%in% c( '1','2','3') |
     muj$P9_3_16%in%c( '1','2','3') | muj$P11_1_1%in% c( '1','2','3') |
     muj$P11_1_2%in%c( '1','2','3') | muj$P11_1_3%in% c( '1','2','3') |
     muj$P11_1_4%in%c( '1','2','3') | muj$P11_1_5%in% c( '1','2','3') |
     muj$P11_1_6%in%c( '1','2','3') | muj$P11_1_7%in% c( '1','2','3') |
     muj$P11_1_8%in%c( '1','2','3') | muj$P11_1_9%in% c( '1','2','3') |
     muj$P11_1_10%in%c( '1','2','3') | muj$P11_1_11%in% c( '1','2','3') |
     muj$P11_1_12%in%c( '1','2','3') | muj$P11_1_13%in% c( '1','2','3') |
     muj$P11_1_14%in%c( '1','2','3') | muj$P11_1_15%in% c( '1','2','3') |
     muj$P11_1_16%in%c( '1','2','3') | muj$P11_1_17%in% c( '1','2','3') |
     muj$P11_1_18%in%c( '1','2','3') | muj$P11_1_19%in% c( '1','2','3') |
     muj$P11_1_20%in%c( '1','2','3') | muj$P14_3_1%in% c( '1','2','3') |
     muj$P14_3_2%in%c( '1','2','3') | muj$P14_3_3%in% c( '1','2','3') |
     muj$P14_3_4%in%c( '1','2','3') | muj$P14_3_5%in% c( '1','2','3') |
     muj$P14_3_6%in%c( '1','2','3') | muj$P14_3_7%in% c( '1','2','3') |
     muj$P14_3_8%in%c( '1','2','3') | muj$P14_3_9%in% c( '1','2','3') |
     muj$P14_3_10%in%c( '1','2','3') | muj$P14_3_11%in% c( '1','2','3') |
     muj$P14_3_12%in%c( '1','2','3') | muj$P14_3_13%in% c( '1','2','3') |
     muj$P14_3_14%in%c( '1','2','3') | muj$P14_3_15%in% c( '1','2','3') |
     muj$P14_3_16%in%c( '1','2','3') | muj$P14_3_17%in% c( '1','2','3') |
     muj$P14_3_18%in%c( '1','2','3') | muj$P14_3_19%in% c( '1','2','3') |
     muj$P14_3_20%in%c( '1','2','3') | muj$P14_3_21%in% c( '1','2','3') |
     muj$P14_3_22%in%c( '1','2','3') | muj$P14_3_23AB%in% c( '1','2','3') |
     muj$P14_3_24AB%in%c( '1','2','3') | muj$P14_3_25%in% c( '1','2','3') |
     muj$P14_3_26%in%c( '1','2','3') | muj$P14_3_27%in% c( '1','2','3') |
     muj$P14_3_28%in%c( '1','2','3') | muj$P14_3_29%in% c( '1','2','3') |
     muj$P14_3_30%in%c( '1','2','3') | muj$P14_3_31%in% c( '1','2','3') |
     muj$P14_3_32%in%c( '1','2','3') | muj$P14_3_33%in% c( '1','2','3') |
     muj$P14_3_34%in%c( '1','2','3') | muj$P14_3_35AB %in% c( '1','2','3') |
     muj$P14_3_36AB%in%c( '1','2','3') | muj$P14_3_37AB%in% c( '1','2','3') |
     muj$P14_3_38AB%in%c( '1','2','3')), 1, 0)


# Violencia psicológica ---------------------------------------------------


#* Prevalencia de violencia psicológica contra las mujeres de 15 años y más a lo largo de la vida *#
# Construcción de variables
# Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
# psicológica a lo largo de la vida
muj$vpsi_lv_con <- ifelse(
  (muj$P7_6_4%in%'1' | muj$P7_6_9%in%'1' | muj$P7_6_13%in%'1' |
     muj$P7_6_16%in%'1' | muj$P7_6_18%in%'1' | muj$P8_9_2 %in%'1' |
     muj$P8_9_7%in%'1' | muj$P8_9_11%in%'1' | muj$P8_9_12%in%'1' |
     muj$P8_9_17%in%'1' | muj$P8_9_18%in%'1' | muj$P9_1_2%in%'1' |
     muj$P9_1_3%in%'1' | muj$P9_1_11%in%'1' | muj$P9_1_15%in%'1' |
     muj$P11_1_1%in%c( '1','2','3') | muj$P11_1_6%in%c( '1','2','3') |
     muj$P11_1_7%in%c( '1','2','3') | muj$P11_1_12%in%c( '1','2','3') |
     muj$P11_1_14%in%c( '1','2','3') | muj$P11_1_17%in%c( '1','2','3') |
     muj$P11_1_20%in%c( '1','2','3') | muj$P14_1_10%in%c( '1','2','3') |
     muj$P14_1_11%in%c( '1','2','3') | muj$P14_1_12%in%c( '1','2','3') |
     muj$P14_1_13%in%c( '1','2','3') | muj$P14_1_14%in%c( '1','2','3') |
     muj$P14_1_15%in%c( '1','2','3') | muj$P14_1_16%in%c( '1','2','3') |
     muj$P14_1_17%in%c( '1','2','3') | muj$P14_1_18%in%c( '1','2','3') |
     muj$P14_1_19%in%c( '1','2','3') | muj$P14_1_20%in%c( '1','2','3') |
     muj$P14_1_21%in%c( '1','2','3') | muj$P14_1_22%in%c( '1','2','3') |
     muj$P14_1_23AB%in%c( '1','2','3') | muj$P14_1_24AB%in%c( '1','2','3') |
     muj$P14_1_31%in%c( '1','2','3')),1,0)

# En pareja
muj$vparpsi_lv_con <- ifelse(
  (muj$P14_1_10%in%c( '1','2','3') |
     muj$P14_1_11%in%c( '1','2','3') | muj$P14_1_12%in%c( '1','2','3') |
     muj$P14_1_13%in%c( '1','2','3') | muj$P14_1_14%in%c( '1','2','3') |
     muj$P14_1_15%in%c( '1','2','3') | muj$P14_1_16%in%c( '1','2','3') |
     muj$P14_1_17%in%c( '1','2','3') | muj$P14_1_18%in%c( '1','2','3') |
     muj$P14_1_19%in%c( '1','2','3') | muj$P14_1_20%in%c( '1','2','3') |
     muj$P14_1_21%in%c( '1','2','3') | muj$P14_1_22%in%c( '1','2','3') |
     muj$P14_1_23AB%in%c( '1','2','3') | muj$P14_1_24AB%in%c( '1','2','3') |
     muj$P14_1_31%in%c( '1','2','3')),1,0)

#* Prevalencia de violencia psicológica contra las mujeres de 15 años y más en los últimos 12 meses *#
# Construcción de variables
# Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
# psicológica en los últimos 12 meses
muj$vpsi_12m_con <- ifelse(
  (muj$P7_8_4%in%c('1', '2','3') | muj$P7_8_9%in% c('1', '2','3') |
     muj$P7_8_13%in%c('1', '2','3') | muj$P7_8_16%in% c('1', '2','3') |
     muj$P7_8_18%in%c('1', '2','3') | muj$P8_11_2%in% c('1', '2','3') |
     muj$P8_11_7%in%c('1', '2','3') | muj$P8_11_11%in% c('1', '2','3') |
     muj$P8_11_12%in%c('1', '2','3') | muj$P8_11_17%in% c('1', '2','3') |
     muj$P8_11_18%in%c('1', '2','3') | muj$P9_3_2%in% c('1', '2','3') |
     muj$P9_3_3%in%c('1', '2','3') | muj$P9_3_11%in% c('1', '2','3') |
     muj$P9_3_15%in%c('1', '2','3') | muj$P11_1_1%in% c( '1','2','3') |
     muj$P11_1_6%in%c( '1','2','3') | muj$P11_1_7%in% c( '1','2','3') |
     muj$P11_1_12%in%c( '1','2','3') | muj$P11_1_14%in% c( '1','2','3') |
     muj$P11_1_17%in%c( '1','2','3') | muj$P11_1_20%in% c( '1','2','3') |
     muj$P14_3_10%in%c( '1','2','3') | muj$P14_3_11%in% c( '1','2','3') |
     muj$P14_3_12%in%c( '1','2','3') | muj$P14_3_13%in% c( '1','2','3') |
     muj$P14_3_14%in%c( '1','2','3') | muj$P14_3_15%in% c( '1','2','3') |
     muj$P14_3_16%in%c( '1','2','3') | muj$P14_3_17%in% c( '1','2','3') |
     muj$P14_3_18%in%c( '1','2','3') | muj$P14_3_19%in% c( '1','2','3') |
     muj$P14_3_20%in%c( '1','2','3') | muj$P14_3_21%in% c( '1','2','3') |
     muj$P14_3_22%in%c( '1','2','3') | muj$P14_3_23AB%in% c( '1','2','3') |
     muj$P14_3_24AB%in%c( '1','2','3') | muj$P14_3_31%in% c( '1','2','3')), 1, 0)

# En pareja
muj$vparpsi_12m_con <- ifelse(
  (muj$P14_3_10%in%c( '1','2','3') | muj$P14_3_11%in% c( '1','2','3') |
     muj$P14_3_12%in%c( '1','2','3') | muj$P14_3_13%in% c( '1','2','3') |
     muj$P14_3_14%in%c( '1','2','3') | muj$P14_3_15%in% c( '1','2','3') |
     muj$P14_3_16%in%c( '1','2','3') | muj$P14_3_17%in% c( '1','2','3') |
     muj$P14_3_18%in%c( '1','2','3') | muj$P14_3_19%in% c( '1','2','3') |
     muj$P14_3_20%in%c( '1','2','3') | muj$P14_3_21%in% c( '1','2','3') |
     muj$P14_3_22%in%c( '1','2','3') | muj$P14_3_23AB%in% c( '1','2','3') |
     muj$P14_3_24AB%in%c( '1','2','3') | muj$P14_3_31%in% c( '1','2','3')), 1, 0)

# Violencia física --------------------------------------------------------


#* Prevalencia de violencia física contra las mujeres de 15 años y más a lo largo de la vida *#
# Construcción de variables
# Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
# física a lo largo de la vida
muj$vfis_lv_con <- ifelse(
  (muj$P7_6_1%in%'1' | muj$P7_6_2%in% '1' | muj$P7_6_6%in%'1' |
     muj$P8_9_8%in%'1' | muj$P8_9_9%in% '1' | muj$P8_9_19%in%'1' |
     muj$P9_1_4%in%'1' | muj$P9_1_6%in% '1' | muj$P9_1_12%in%'1' |
     muj$P11_1_5%in%c( '1','2','3') | muj$P11_1_10%in%c( '1','2','3') |
     muj$P11_1_11%in%c( '1','2','3') | muj$P14_1_1%in%c( '1','2','3') |
     muj$P14_1_2%in%c( '1','2','3') | muj$P14_1_3%in%c( '1','2','3') |
     muj$P14_1_4%in%c( '1','2','3') | muj$P14_1_5%in%c( '1','2','3') |
     muj$P14_1_6%in%c( '1','2','3') | muj$P14_1_7%in%c( '1','2','3') |
     muj$P14_1_8%in%c( '1','2','3') | muj$P14_1_9%in%c( '1','2','3')),1,0)


# En pareja
muj$vparfis_lv_con <- ifelse(
  (muj$P14_1_1%in%c( '1','2','3') |
     muj$P14_1_2%in%c( '1','2','3') | muj$P14_1_3%in%c( '1','2','3') |
     muj$P14_1_4%in%c( '1','2','3') | muj$P14_1_5%in%c( '1','2','3') |
     muj$P14_1_6%in%c( '1','2','3') | muj$P14_1_7%in%c( '1','2','3') |
     muj$P14_1_8%in%c( '1','2','3') | muj$P14_1_9%in%c( '1','2','3')),1,0)

#* Prevalencia de violencia física contra las mujeres de 15 años y más en los últimos 12 meses *#
# Construcción de variables
# Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
# física en los últimos 12 meses
muj$vfis_12m_con <- ifelse(
  (muj$P7_8_1%in%c('1', '2','3') | muj$P7_8_2%in%c('1', '2','3') |
     muj$P7_8_6%in%c('1', '2','3') | muj$P8_11_8%in%c('1', '2','3') |
     muj$P8_11_9%in%c('1', '2','3') | muj$P8_11_19%in%c('1', '2','3') |
     muj$P9_3_4%in%c('1', '2','3') | muj$P9_3_6%in%c('1', '2','3') |
     muj$P9_3_12%in%c('1', '2','3') | muj$P11_1_5%in%c('1', '2','3') |
     muj$P11_1_10%in%c('1', '2','3') | muj$P11_1_11%in%c('1', '2','3') |
     muj$P14_3_1%in%c('1', '2','3') | muj$P14_3_2%in%c('1', '2','3') |
     muj$P14_3_3%in%c('1', '2','3') | muj$P14_3_4%in%c('1', '2','3') |
     muj$P14_3_5%in%c('1', '2','3') | muj$P14_3_6%in%c('1', '2','3') |
     muj$P14_3_7%in%c('1', '2','3') | muj$P14_3_8%in%c('1', '2','3') |
     muj$P14_3_9%in%c('1', '2','3')),1,0)

# En pareja
muj$vparfis_12m_con <- ifelse(
  (muj$P14_3_1%in%c('1', '2','3') | muj$P14_3_2%in%c('1', '2','3') |
     muj$P14_3_3%in%c('1', '2','3') | muj$P14_3_4%in%c('1', '2','3') |
     muj$P14_3_5%in%c('1', '2','3') | muj$P14_3_6%in%c('1', '2','3') |
     muj$P14_3_7%in%c('1', '2','3') | muj$P14_3_8%in%c('1', '2','3') |
     muj$P14_3_9%in%c('1', '2','3')),1,0)


# Violencia económica y/o patrimonial -------------------------------------

#* Prevalencia de violencia económica y/o patrimonial contra las mujeres de 15 años y más a lo largo dela vida *#
  # Construcción de variables
  # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
  # económica a lo largo de la vida
  muj$veco_lv_con <- ifelse(
    (muj$P8_3_1_1%in%'1' | muj$P8_3_1_2%in% '1' | muj$P8_3_2_1%in% '1' |
       muj$P8_3_2_2%in%'1' | muj$P8_3_2_3%in% '1' | muj$P8_8_1%in% '1' |
       muj$P8_8_2%in%'1' | muj$P8_8_3%in% '1' | muj$P8_8_4%in% '1' |
       muj$P8_8_5%in%'1' | muj$P8_8_6%in% '1' | muj$P8_8_7%in% '1' |
       muj$P8_8_8%in%'1' | muj$P8_8_9%in% '1' |
       muj$P11_1_8%in%c( '1','2','3') | muj$P11_1_9%in%c( '1','2','3') |
       muj$P11_1_15%in%c( '1','2','3') | muj$P11_1_16%in%c( '1','2','3') |
       muj$P14_1_32%in%c( '1','2','3') | muj$P14_1_33%in%c( '1','2','3') |
       muj$P14_1_34%in%c( '1','2','3') | muj$P14_1_35AB%in%c( '1','2','3') |
       muj$P14_1_36AB%in%c( '1','2','3') | muj$P14_1_37AB%in%c( '1','2','3') |
       muj$P14_1_38AB%in%c( '1','2','3')), 1, 0)

# En pareja
muj$vpareco_lv_con <- ifelse(
  (muj$P14_1_32%in%c( '1','2','3') | muj$P14_1_33%in%c( '1','2','3') |
     muj$P14_1_34%in%c( '1','2','3') | muj$P14_1_35AB%in%c( '1','2','3') |
     muj$P14_1_36AB%in%c( '1','2','3') | muj$P14_1_37AB%in%c( '1','2','3') |
     muj$P14_1_38AB%in%c( '1','2','3')), 1, 0)


  #* Prevalencia de violencia económica contra las mujeres de 15 años y más en los últimos 12 meses *#
  # Construcción de variables
  # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
  # económica en los últimos 12 meses
  muj$veco_12m_con <- ifelse(
    (muj$P8_8_1%in%'1' | muj$P8_8_2%in% '1' | muj$P8_8_3%in% '1' |
       muj$P8_8_4%in%'1' | muj$P8_8_5%in% '1' | muj$P8_8_6%in% '1' |
       muj$P8_8_7%in%'1' | muj$P8_8_8%in% '1' | muj$P8_8_9%in% '1' |
       muj$P11_1_8%in%c( '1','2','3') | muj$P11_1_9%in% c( '1','2','3') |
       muj$P11_1_15%in%c( '1','2','3') | muj$P11_1_16%in% c( '1','2','3') |
       muj$P14_3_32%in%c( '1','2','3') | muj$P14_3_33%in% c( '1','2','3') |
       muj$P14_3_34%in%c( '1','2','3') | muj$P14_3_35AB%in% c( '1','2','3') |
       muj$P14_3_36AB%in%c( '1','2','3') | muj$P14_3_37AB%in% c( '1','2','3') |
       muj$P14_3_38AB%in%c( '1','2','3')), 1, 0)

  # En pareja
  muj$vpareco_12m_con <- ifelse(
    (  muj$P14_3_32%in%c( '1','2','3') | muj$P14_3_33%in% c( '1','2','3') |
       muj$P14_3_34%in%c( '1','2','3') | muj$P14_3_35AB%in% c( '1','2','3') |
       muj$P14_3_36AB%in%c( '1','2','3') | muj$P14_3_37AB%in% c( '1','2','3') |
       muj$P14_3_38AB%in%c( '1','2','3')), 1, 0)


# Violencia sexual --------------------------------------------------------

  #* Prevalencia de violencia sexual contra las mujeres de 15 años y más a lo largo de la vida *#
  # Construcción de variables
  # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
  # sexual a lo largo de la vida
  muj$vsex_lv_con <- ifelse(
    (muj$P7_6_3%in%'1' | muj$P7_6_5%in% '1' | muj$P7_6_7%in% '1' |
       muj$P7_6_8%in%'1' | muj$P7_6_10%in% '1' | muj$P7_6_11%in% '1' |
       muj$P7_6_12%in%'1' | muj$P7_6_14%in% '1' | muj$P7_6_15%in% '1' |
       muj$P7_6_17%in%'1' | muj$P8_9_1%in% '1' | muj$P8_9_3%in% '1' |
       muj$P8_9_4%in%'1' | muj$P8_9_5%in% '1' | muj$P8_9_6%in% '1' |
       muj$P8_9_10%in%'1' | muj$P8_9_13%in% '1' | muj$P8_9_14%in% '1' |
       muj$P8_9_15%in%'1' | muj$P8_9_16%in% '1' | muj$P9_1_1%in% '1' |
       muj$P9_1_5%in%'1' | muj$P9_1_7%in% '1' | muj$P9_1_8%in% '1' |
       muj$P9_1_9%in%'1' | muj$P9_1_10%in% '1' | muj$P9_1_13%in% '1' |
       muj$P9_1_14%in%'1' | muj$P9_1_16%in% '1' |
       muj$P11_1_2%in%c( '1','2','3') | muj$P11_1_3%in%c( '1','2','3') |
       muj$P11_1_4%in%c( '1','2','3') | muj$P11_1_13%in%c( '1','2','3') |
       muj$P11_1_18%in%c( '1','2','3') | muj$P11_1_19%in%c( '1','2','3') |
       muj$P14_1_25%in%c( '1','2','3') | muj$P14_1_26%in%c( '1','2','3') |
       muj$P14_1_27%in%c( '1','2','3') | muj$P14_1_28%in%c( '1','2','3') |
       muj$P14_1_29%in%c( '1','2','3') | muj$P14_1_30%in%c( '1','2','3')), 1,0)

  # En pareja
  muj$vparsex_lv_con <- ifelse(
    (muj$P14_1_25%in%c( '1','2','3') | muj$P14_1_26%in%c( '1','2','3') |
       muj$P14_1_27%in%c( '1','2','3') | muj$P14_1_28%in%c( '1','2','3') |
       muj$P14_1_29%in%c( '1','2','3') | muj$P14_1_30%in%c( '1','2','3')), 1,0)

  #* Prevalencia de violencia sexual contra las mujeres de 15 años y más en los últimos 12 meses *#
  #* # Construcción de variables
  # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
  # sexual en los últimos 12 meses
  muj$vsex_12m_con <- ifelse(
    (muj$P7_8_3%in%c('1','2','3') | muj$P7_8_5%in%c('1','2','3') |
       muj$P7_8_7%in%c('1','2','3') | muj$P7_8_8%in%c('1','2','3') |
       muj$P7_8_10%in%c('1','2','3') | muj$P7_8_11%in%c('1','2','3') |
       muj$P7_8_12%in%c('1','2','3') | muj$P7_8_14%in%c('1','2','3') |
       muj$P7_8_15%in%c('1','2','3') | muj$P7_8_17%in%c('1','2','3') |
       muj$P8_11_1%in%c('1','2','3') | muj$P8_11_3%in%c('1','2','3') |
       muj$P8_11_4%in%c('1','2','3') | muj$P8_11_5%in%c('1','2','3') |
       muj$P8_11_6%in%c('1','2','3') | muj$P8_11_10%in%c('1','2','3') |
       muj$P8_11_13%in%c('1','2','3') | muj$P8_11_14%in%c('1','2','3') |
       muj$P8_11_15%in%c('1','2','3') | muj$P8_11_16%in%c('1','2','3') |
       muj$P9_3_1%in%c('1','2','3') | muj$P9_3_5%in%c('1','2','3') |
       muj$P9_3_7%in%c('1','2','3') | muj$P9_3_8%in%c('1','2','3') |
       muj$P9_3_9%in%c('1','2','3') | muj$P9_3_10%in%c('1','2','3') |
       muj$P9_3_13%in%c('1','2','3') | muj$P9_3_14%in%c('1','2','3') |
       muj$P9_3_16%in%c('1','2','3') | muj$P11_1_2%in%c('1','2','3') |
       muj$P11_1_3%in%c('1','2','3') | muj$P11_1_4%in%c('1','2','3') |
       muj$P11_1_13%in%c('1','2','3') | muj$P11_1_18%in%c('1','2','3') |
       muj$P11_1_19%in%c('1','2','3') | muj$P14_3_25%in%c('1','2','3') |
       muj$P14_3_26%in%c('1','2','3') | muj$P14_3_27%in%c('1','2','3') |
       muj$P14_3_28%in%c('1','2','3') | muj$P14_3_29%in%c('1','2','3') |
       muj$P14_3_30%in%c('1','2','3')),1,0)

# En pareja
  muj$vparsex_12m_con <- ifelse(
    (muj$P14_3_25%in%c('1','2','3') |
       muj$P14_3_26%in%c('1','2','3') | muj$P14_3_27%in%c('1','2','3') |
       muj$P14_3_28%in%c('1','2','3') | muj$P14_3_29%in%c('1','2','3') |
       muj$P14_3_30%in%c('1','2','3')),1,0)

  # 1.2.18 Prevalencia de violencia total contra las mujeres de 15 años y más en el ámbito de pareja a lo largo de su relación actual o última
  #* Prevalencia de violencia total contra las mujeres de 15 años y más en el ámbito de pareja a lo largo de su relación actual o última *#
    # Construcción de variables
    # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
    # a lo largo de su relación de pareja (actual o última)

  muj$vpartot_lr_con <-ifelse(
    (muj$P14_1_1%in%c('1','2','3') | muj$P14_1_2%in%c('1','2','3') |
       muj$P14_1_3%in%c('1','2','3') | muj$P14_1_4%in%c('1','2','3') |
       muj$P14_1_5%in%c('1','2','3') | muj$P14_1_6%in%c('1','2','3') |
       muj$P14_1_7%in%c('1','2','3') | muj$P14_1_8%in%c('1','2','3') |
       muj$P14_1_9%in%c('1','2','3') | muj$P14_1_10%in%c('1','2','3') |
       muj$P14_1_11%in%c('1','2','3') | muj$P14_1_12%in%c('1','2','3') |
       muj$P14_1_13%in%c('1','2','3') | muj$P14_1_14%in%c('1','2','3') |
       muj$P14_1_15%in%c('1','2','3') | muj$P14_1_16%in%c('1','2','3') |
       muj$P14_1_17%in%c('1','2','3') | muj$P14_1_18%in%c('1','2','3') |
       muj$P14_1_19%in%c('1','2','3') | muj$P14_1_20%in%c('1','2','3') |
       muj$P14_1_21%in%c('1','2','3') | muj$P14_1_22%in%c('1','2','3') |
       muj$P14_1_23AB%in%c('1','2','3') | muj$P14_1_24AB%in%c('1','2','3') |
       muj$P14_1_25%in%c('1','2','3') | muj$P14_1_26%in%c('1','2','3') |
       muj$P14_1_27%in%c('1','2','3') | muj$P14_1_28%in%c('1','2','3') |
       muj$P14_1_29%in%c('1','2','3') | muj$P14_1_30%in%c('1','2','3') |
       muj$P14_1_31%in%c('1','2','3') | muj$P14_1_32%in%c('1','2','3') |
       muj$P14_1_33%in%c('1','2','3') | muj$P14_1_34%in%c('1','2','3') |
       muj$P14_1_35AB%in%c('1','2','3') | muj$P14_1_36AB%in%c('1','2','3') |
       muj$P14_1_37AB%in%c('1','2','3') | muj$P14_1_38AB%in%c('1','2','3')),1,0)

  #* 1.2.19 Prevalencia de violencia total contra las mujeres de 15 años y más en el ámbito de pareja (actualo última) en los últimos 12 meses *#
  #* # Construcción de variables
  # Mujeres de 15 años y más que experimentaron una o más situaciones de violencia
  # en los últimos 12 meses en el ámbito de pareja (actual o última)
  muj$vpartot_12m_con <- ifelse(
    (muj$P14_3_1%in%c('1','2','3') | muj$P14_3_2%in%c('1','2','3') |
       muj$P14_3_3%in%c('1','2','3') | muj$P14_3_4%in%c('1','2','3') |
       muj$P14_3_5%in%c('1','2','3') | muj$P14_3_6%in%c('1','2','3') |
       muj$P14_3_7%in%c('1','2','3') | muj$P14_3_8%in%c('1','2','3') |
       muj$P14_3_9%in%c('1','2','3') | muj$P14_3_10%in%c('1','2','3') |
       muj$P14_3_11%in%c('1','2','3') | muj$P14_3_12%in%c('1','2','3') |
       muj$P14_3_13%in%c('1','2','3') | muj$P14_3_14%in%c('1','2','3') |
       muj$P14_3_15%in%c('1','2','3') | muj$P14_3_16%in%c('1','2','3') |
       muj$P14_3_17%in%c('1','2','3') | muj$P14_3_18%in%c('1','2','3') |
       muj$P14_3_19%in%c('1','2','3') | muj$P14_3_20%in%c('1','2','3') |
       muj$P14_3_21%in%c('1','2','3') | muj$P14_3_22%in%c('1','2','3') |
       muj$P14_3_23AB%in%c('1','2','3') | muj$P14_3_24AB%in%c('1','2','3') |
       muj$P14_3_25%in%c('1','2','3') | muj$P14_3_26%in%c('1','2','3') |
       muj$P14_3_27%in%c('1','2','3') | muj$P14_3_28%in%c('1','2','3') |
       muj$P14_3_29%in%c('1','2','3') | muj$P14_3_30%in%c('1','2','3') |
       muj$P14_3_31%in%c('1','2','3') | muj$P14_3_32%in%c('1','2','3') |
       muj$P14_3_33%in%c('1','2','3') | muj$P14_3_34%in%c('1','2','3') |
       muj$P14_3_35AB%in%c('1','2','3') | muj$P14_3_36AB%in%c('1','2','3') |
       muj$P14_3_37AB%in%c('1','2','3') | muj$P14_3_38AB%in%c('1','2','3')) ,1,0)


# write.csv(muj, "clean/TB_SEC_IVaVD.csv", row.names = FALSE)
write_dta(muj, "clean/TB_SEC_IVaVD.dta")


