#===============================================================================
#===============================================================================
# JE3 CALCULOS DESDE CERO
# CORTE 2025-06-06
#===============================================================================
#===============================================================================

#UBICAR LA CARPETA UDEA
setwd("D:/FUENTES_INFORMACION/JOVENES_U/Inscritos9_JE3/UDEA/BOGOTA")
getwd()

# LIBRERIAS A USAR
library(sqldf)
library(expss)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(eeptools)
library(openxlsx)
options(scipen=999)

#===============================================================================
# CARGUE FUENTES ORIGINALES PARA EL CALCULO
#===============================================================================
load("JE3_INSUMO_BOGOTA.RData")

# Los dataframes que contiene son:

# BASE_OFERTA_FDL (contiene por ID_OFERTA_PROGRAMA y N_REG_OFERTA los cupos disponibles CUPOS_ASIGNADOS de acuerdo al TIPO_OFERTA y FUENTE)
# JE3 (base de inscritos BOGOTA persona unica (anonimizado) , contiene el cruce con fuentes externas para el calculo de habilitación, criterios de puntuación, ordenamiento, elegibles y listas de espera)
# JE3_PER_OFERTA (base de aspirantes con el detalle de la oferta inscrita, contiene la PRIORIDAD y la marca de UAESP) 

#===========================================================================================================================================
# CALCULO HABILITADOS 
# 5.1.1.1. Requisitos de participación para jóvenes graduados o con bachillerato validado en Bogotá.
#===========================================================================================================================================

JE3$CORTE_EDAD <- as.integer(age_calc(as.Date(JE3$FECHA_NACIMIENTO, format = "%Y-%m-%d"), enddate = as.Date("2025-05-13"), units = "years", precise = TRUE))
cro(JE3$CORTE_EDAD)

#------------------------------
# CRITERIO A
# a) Ser Bachiller egresado de un colegio ubicado en la ciudad de Bogotá y autorizado por la Secretaría de Educación del Distrito, 
# o haber obtenido el título de Bachiller mediante la prueba de validación del ICFES presentada en Bogotá.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                     WHEN  SIMAT_GM_Divipola_MUNICIPIO is null 
                          AND MEN_GM_Divipola_MUNICIPIO is null 
                          AND SABER11_PERIODO is null  
                          THEN '01_SIN_GM_SIN_SABER11'

                     WHEN RA_ICFES_VALIDACION=='N' AND SIMAT_GM_Divipola_MUNICIPIO is not null AND SIMAT_GM_Divipola_MUNICIPIO!='11001' THEN '02_SIMAT_GM_NOBOGOTA'
                     WHEN RA_ICFES_VALIDACION=='N' AND MEN_GM_Divipola_MUNICIPIO  is not null AND MEN_GM_Divipola_MUNICIPIO!='11001' THEN '02_MEN_GM_NOBOGOTA' 
                     
                     WHEN RA_ICFES_VALIDACION=='S' AND SABER11_REGISTRO_SNP like 'VG%' AND SABER11_PUNTAJE_GLOBAL < 30 THEN '05_VALIDANTE_MENOR30_PUNTOS'
                     WHEN RA_ICFES_VALIDACION=='S' AND SABER11_REGISTRO_SNP like 'VG%' AND SABER11_MUNICIPIO_CITACION !='BOGOTÁ D.C.'  THEN '05_VALIDANTE_NO_BOGOTA'
                     
                     WHEN RA_ICFES_VALIDACION=='DV' AND SABER11_REGISTRO_SNP like 'AC%' THEN '06_SABER11_NO_ES_VALIDANTE'
                     
                     ELSE 'HABILITADO' END HABILITADO_A
                     FROM JE3")

cro(JE3$HABILITADO_A)

#------------------------------
# CRITERIO B
# b) Haber presentado la prueba Saber 11° o pruebas ICFES.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                     WHEN SABER11_PERIODO is null THEN 'NO_HABILITADO' 
                     ELSE 'HABILITADO' END HABILITADO_B
                     FROM JE3")
cro(JE3$HABILITADO_B)

#------------------------------
# CRITERIO C
# c) Tener hasta 28 años.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                     WHEN CORTE_EDAD > 28 THEN 'NO_HABILITADO' ELSE 'HABILITADO' END HABILITADO_C
                     FROM JE3")
cro(JE3$HABILITADO_C)

#------------------------------
# CRITERIO D
# d) Si en el último año a partir del cierre de la convocatoria, el aspirante se encontraba cursando un programa técnico profesional 
# o tecnológico, podrá participar, si cuenta con estado “Graduado” en el sistema de información SNIES1. Este requisito no aplica para 
# los egresados o estudiantes del SENA que hayan finalizado su etapa lectiva y se encuentren en la etapa productiva.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                     
                      -- MATRICULA TYT 2025_1 (SIN SENA)
                     WHEN TYT_MATRICULA_2025_1 IS NOT NULL 
                          AND TYT_GRADUACION_MATRICULA_2025_1 like '%NO_GRADUADO%' 
                          AND SENA_Registra_Certificado is null THEN '1_TYT_20251_SINGRADO_SINSENA'
                     
                     -- MATRICULA TYT 2024_2(SIN SENA)
                     WHEN TYT_MATRICULA_2024_2 IS NOT NULL 
                          AND TYT_GRADUACION_MATRICULA_2024_2 like '%NO_GRADUADO%'  
                          AND SENA_Registra_Certificado is null THEN '2_TYT_20242_SINGRADO_SINSENA'
                     
                    -- MATRICULA TYT 2025_1 Y CONTRATO SENA MENOR AL INICIO DE LA CONVOCATORIA
                     WHEN TYT_MATRICULA_2025_1 IS NOT NULL 
                          AND TYT_GRADUACION_MATRICULA_2025_1 like '%NO_GRADUADO%' 
                          AND SENA_Registra_Certificado =='NO' 
                          AND (SENA_Fecha_Fin_Contrato < '2025-05-05' OR SENA_Fecha_Fin_Contrato is null) THEN '3_TYT_20251_SINGRADO_SENA_EXP'
                    
                    -- MATRICULA TYT 2024_2 Y CONTRATO SENA MENOR AL INICIO DE LA CONVOCATORIA
                     WHEN TYT_MATRICULA_2024_2 IS NOT NULL 
                          AND TYT_GRADUACION_MATRICULA_2024_2 like '%NO_GRADUADO%'  
                          AND SENA_Registra_Certificado =='NO'
                          AND (SENA_Fecha_Fin_Contrato < '2025-05-05' OR SENA_Fecha_Fin_Contrato is null) THEN '4_TYT_20242_SINGRADO_SENA_EXP'
                    
                     ELSE 'HABILITADO' END HABILITADO_D 
             FROM JE3")

cro(JE3$HABILITADO_D)

#------------------------------
# CRITERIO E
# e) No encontrarse cursando un programa de nivel profesional universitario en el último año.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                    -- EGRESADO SUPERIOR
                    WHEN MEN_ULTIMA_MATRICULA_SUPERIOR IS NOT NULL THEN 'NO_HABILITADO'
                     
                     ELSE 'HABILITADO' END HABILITADO_E 
             FROM JE3")

cro(JE3$HABILITADO_E)

#------------------------------
# CRITERIO F
# f) No ser egresado(a) de un programa de educación superior en el nivel profesional universitario.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                    -- EGRESADO SUPERIOR
                    WHEN GRADUADO_Universitario IS NOT NULL OR GRADUADO_EspecializacionUniversitaria IS NOT NULL THEN 'NO_HABILITADO'
                     
                     ELSE 'HABILITADO' END HABILITADO_F 
             FROM JE3")

cro(JE3$HABILITADO_F)

#------------------------------
# CRITERIO G
#g) Inscribirse a la convocatoria a través del sistema de inscripciones SICORE de Atenea y en las fechas indicadas en este lineamiento.
# NO APLICA DADO QUE TODOS PROVIENEN DEL SISTEMA DE INFORMACION SICORE


#------------------------------
# CRITERIO H
# h) No haber sido beneficiario(a) del Programa “Jóvenes a la E” (antes Jóvenes a la U) en sus convocatorias previas, o de otras estrategias 
# que adelanten Atenea o la Secretaría de Educación del Distrito para el acceso y la permanencia en educación superior, a saber: 
# Fondo Educación Superior para Todos (FEST), Fondo Alianza Ciudad Educadora, Fondo para la Reparación de las Víctimas del Conflicto Armado, 
# Fondo Ciudad Bolívar, Fondo Técnica y Tecnológica, Fondo Universidades Públicas (Universidad Nacional de Colombia, Universidad Nacional Abierta 
# y a Distancia- UNAD, Universidad Distrital Francisco José de Caldas, Universidad Pedagógica Nacional y Escuela Tecnológica Instituto Técnico Central) 
# y becas Universidad Libre y América. No obstante, podrán participar los beneficiarios de Jóvenes a la E de carreras técnicas profesionales y 
# tecnológicas que con corte al 31 de diciembre de 2024 se encuentren reportados como “graduados” por parte de las Instituciones de Educación 
# Superior en donde adelantaron sus estudios.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                    -- JU
                    WHEN RESTRICCION_JU =='S' THEN 'ES_BENEF_JU'
                    -- UTC
                    WHEN UTC_ESTADO_JE is not null and UTC_ESTADO_JE =='Bloqueado' THEN 'ES_BENEF_UTC'
                    -- FONDOS
                    WHEN RA_FONDOS_ATENEA =='S' OR RA_FONDOS_SED =='S' THEN 'ES_FONDOS'
                    
                     ELSE 'HABILITADO' END HABILITADO_H
             FROM JE3")

cro(JE3$HABILITADO_H)

#------------------------------
# CRITERIO I - RNEC
# Nota 1: Para la verificación de requisitos mínimos de participación la Agencia Atenea realizará validaciones de identidad de las y los aspirantes, 
# por lo cual cualquier inconsistencia que sea reportada por la Registraduría Nacional del Estado Civil será causal de inhabilitación.
#------------------------------
JE3 <- sqldf("SELECT *,
                     CASE 
                    -- RNEC
                    WHEN RA_INHABILITADO_RENEC is not null AND RA_INHABILITADO_RENEC NOT IN ('HABILITADO') THEN 'NO_HABILITADO'
                    
                     ELSE 'HABILITADO' END HABILITADO_I
             FROM JE3")

cro(JE3$HABILITADO_I)

#=================================================================================================================================
# CALCULO HABILITACION SECUENCIAS - BOGOTA
# La persona se inhabilita por la primera regla con inclumplimiento
#=================================================================================================================================
JE3 <- sqldf("SELECT *,
         
         CASE 
                     WHEN HABILITADO_A !='HABILITADO'  THEN 'INHABILITA_REQUISITO_A'
                     WHEN HABILITADO_B !='HABILITADO'  THEN 'INHABILITA_REQUISITO_B'
                     WHEN HABILITADO_C !='HABILITADO'  THEN 'INHABILITA_REQUISITO_C'
                     WHEN HABILITADO_D !='HABILITADO'  THEN 'INHABILITA_REQUISITO_D'
                     WHEN HABILITADO_E !='HABILITADO'  THEN 'INHABILITA_REQUISITO_E'
                     WHEN HABILITADO_F !='HABILITADO'  THEN 'INHABILITA_REQUISITO_F'
                     WHEN HABILITADO_H !='HABILITADO'  THEN 'INHABILITA_REQUISITO_H'
                     WHEN HABILITADO_I !='HABILITADO'  THEN 'INHABILITA_REQUISITO_I'
              ELSE 'HABILITADO' END HABILITADO
             
             FROM JE3")

cro(JE3$HABILITADO)


#===========================================================================================================================================
#         CALCULO CRITERIOS DE PUNTUACION
# 5.2. Fase 2: Mecanismo de selección de elegibles y asignación de cupos
#===========================================================================================================================================

#-------------------------------------------------------------------------------
# CRITERIO 1 : Vulnerabilidad estructural (Acumulable hasta 15 puntos)
#-------------------------------------------------------------------------------

JE3 <- sqldf("SELECT *,
                     CASE WHEN SEXO IN ('MUJER') THEN 7 else 0 END C1_1, 
                     CASE WHEN SEXO IN ('MUJER') AND INFO_HIJOS IS NOT NULL AND INFO_HIJOS !='SINHIJOS' AND ATENEA_PUNTUA_HIJOS == 'S'  THEN 5 else 0 END C1_2,
                     CASE WHEN IDENTIDAD_GENERO IN ('HOMBRE TRANSGENERO', 'MUJER TRANSGENERO') THEN 10 else 0 END C1_3,
                     CASE WHEN ETNIA != 'NINGUNO' AND ATENEA_PUNTUA_ETNIA =='S' THEN 15 else 0 END C1_4,
                     CASE WHEN RA_VICTIMAS =='S'                        THEN 10 ELSE 0 END C1_5,  
                     CASE WHEN RA_VIOLENCIA_GENERO =='S'                 THEN 10 ELSE 0 END C1_6,
                     CASE WHEN RA_REINCORPORADOS_REINSERTADOS =='S'     THEN 10 ELSE 0 END C1_7,  
                     CASE WHEN RA_DISCAPACIDAD_MINSALUD =='S'           THEN 10 ELSE 0 END C1_8,
                     CASE WHEN RA_MINDEFENSA_LEY1699 =='S'              THEN 10 ELSE 0 END C1_9,
                     
                     --Egresados colegios oficiales Localidad Sumapaz o en zonas rurales a más de 3 km
                     CASE WHEN SIMAT_GM_CODIGO_DANE_SEDE is not null AND  SIMAT_GM_CODIGO_DANE_SEDE in (211850001473,211850000833,211001076346,211850000094,211850000876,211001027485,211850001121,211850001171) THEN 15
                          WHEN MEN_GM_CODIGO_DANE_SEDE is not null AND  MEN_GM_CODIGO_DANE_SEDE in (211850001473,211850000833,211001076346,211850000094,211850000876,211001027485,211850001121,211850001171) THEN 15
                                                                                                        
                      ELSE 0 END C1_10
                     
                     FROM JE3")

# SE TOTALIZAN EL CALCULO DE LAS VARIABLES QUE CONFORMAN EL CRITERIO
JE3$C1_TOTAL <- JE3$C1_1 + JE3$C1_2 + JE3$C1_3 + JE3$C1_4 +  JE3$C1_5 + JE3$C1_6 + JE3$C1_7 + JE3$C1_8 + JE3$C1_9 + JE3$C1_10 

# Se calcula el criterio teniendo en cuenta que es Acumulable hasta 15 puntos
JE3 <- sqldf("SELECT *,
                   CASE 
                        WHEN C1_TOTAL > 15 THEN 15 ELSE C1_TOTAL END CRITERIO_1_VUL_ESTRUCTURAL
                   FROM JE3")

cro(JE3$CRITERIO_1_VUL_ESTRUCTURAL)
cro(JE3$C1_TOTAL,JE3$CRITERIO_1_VUL_ESTRUCTURAL)

#-------------------------------------------------------------------------------
# CRITERIO 2 : Vulnerabilidad económica (No acumulable)
#-------------------------------------------------------------------------------

# CALCULO PERSONA CON SISBEN
JE3 <- sqldf("SELECT *,
                     CASE WHEN SISBEN4_Grupo  =='A'  THEN 40  
                          WHEN SISBEN4_Grupo  =='B'  THEN 30
                          WHEN SISBEN4_Grupo  =='C'  THEN 20
                          ELSE 0 END C2_1
                     FROM JE3")
cro(JE3$SISBEN4_Grupo, JE3$C2_1)

# CALCULO PERSONA SIN SISBEN
JE3 <- sqldf("SELECT *,
                     CASE 
                          --GRADUADOS SIMAT (OFICIAL-RURAL)
                          WHEN C2_1==0 AND SISBEN4_Grupo =='NA' AND SIMAT_GM_SECTOR =='OFICIAL' AND SIMAT_GM_ZONA == 'RURAL'  THEN 40
                          --GRADUADOS MEN (OFICIAL-RURAL)
                          WHEN C2_1==0 AND SISBEN4_Grupo =='NA' AND MEN_GM_SECTOR =='OFICIAL' AND MEN_GM_ZONA == 'RURAL'  THEN 40
                          
                          -- ETNIA INDIGENA
                            WHEN C2_1==0 AND SISBEN4_Grupo =='NA' AND ETNIA =='INDIGENA' AND ATENEA_PUNTUA_ETNIA =='S'  THEN 30
                            
                          --GRADUADOS SIMAT
                          WHEN C2_1==0 AND SISBEN4_Grupo =='NA' AND SIMAT_GM_SECTOR =='OFICIAL' THEN 15
                          --GRADUADOS MEN
                          WHEN C2_1==0 AND SISBEN4_Grupo =='NA' AND MEN_GM_SECTOR =='OFICIAL' THEN 15
                          
                          ELSE 0 END C2_2
                     FROM JE3")
cro(JE3$C2_2)

# LA SUMATORIA DE LOS CALCULOS DEFINE EL CRITERIO 2
JE3$CRITERIO_2_VUL_ECONOMICA <- JE3$C2_1 + JE3$C2_2
cro(JE3$CRITERIO_2_VUL_ECONOMICA)


#-------------------------------------------------------------------------------
# CRITERIO 3 : Mérito académico
#-------------------------------------------------------------------------------
# ESTAS VARIABLES DEBEN SER NUMERICAS
str(JE3$SABER11_PERIODO)
str(JE3$SABER11_PERCENTIL_NACIONAL_GLOBAL)
str(JE3$SABER11_PUESTO)

JE3 <- sqldf("SELECT *,
                     CASE 
                     
                     --POR PERCENTIL
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL == 100 THEN 40
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 95 and 99 THEN 35
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 90 and 94 THEN 30
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 85 and 89 THEN 25
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 75 and 84 THEN 20
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 65 and 74 THEN 10
                     WHEN SABER11_PERIODO >= 20161 AND SABER11_PERCENTIL_NACIONAL_GLOBAL between 50 and 64 THEN 5
                     
                     --POR PUESTO
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 1 and 10 THEN 40
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 11 and 50 THEN 35
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 51 and 100 THEN 30
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 101 and 150 THEN 25
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 151 and 250 THEN 20
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 251 and 350 THEN 15
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 351 and 450 THEN 10
                     WHEN SABER11_PERIODO < 20161 AND SABER11_PUESTO between 451 and 499 THEN 5
                   
                     ELSE 0 END CRITERIO_3_MERITO_ACADEMICO
                     FROM JE3")
cro(JE3$CRITERIO_3_MERITO_ACADEMICO)

#-------------------------------------------------------------------------------
# CRITERIO 4 : Fomento a trayectorias (No acumulable Hasta 5 puntos)
#-------------------------------------------------------------------------------

JE3 <- sqldf("SELECT *,
                     CASE WHEN RA_UTC =='S'  THEN 5
                          WHEN RESTRICCION_JU =='NA'  THEN 5
                          WHEN RA_CONSEJERO_JUVENTUD_ELECTO =='S'  THEN 5
                          WHEN RA_DEPORTISTA_IDRD  =='S'  THEN 5
                          WHEN RA_PARCEROS  =='S'  THEN 5
                          WHEN RA_JOVENES_OPORTUNIDADES  =='S'  THEN 5
                          WHEN RA_RETO_SDIS  =='S'  THEN 5
                          
                          ELSE 0 END CRITERIO_4_TRAYECTORIA
                     FROM JE3")
cro(JE3$CRITERIO_4_TRAYECTORIA)


#-------------------------------------------------------------------------------
# SUMATORIA CRITERIOS PARA PUNTAJE_GLOBAL
#-------------------------------------------------------------------------------
JE3$CRITERIO_1_VUL_ESTRUCTURAL <- as.double(JE3$CRITERIO_1_VUL_ESTRUCTURAL)
JE3$CRITERIO_2_VUL_ECONOMICA <- as.double(JE3$CRITERIO_2_VUL_ECONOMICA)
JE3$CRITERIO_3_MERITO_ACADEMICO <- as.double(JE3$CRITERIO_3_MERITO_ACADEMICO)
JE3$CRITERIO_4_TRAYECTORIA <- as.double(JE3$CRITERIO_4_TRAYECTORIA)

JE3$PUNTAJE_GLOBAL <- JE3$CRITERIO_1_VUL_ESTRUCTURAL + JE3$CRITERIO_2_VUL_ECONOMICA + JE3$CRITERIO_3_MERITO_ACADEMICO + JE3$CRITERIO_4_TRAYECTORIA


#-------------------------------------------------------------------------------
# ORDENAMIENTO
# 5.2.2. Actividad 3: Aplicación de criterios de desempate
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# punto e) Cuando no se logre el desempate por los criterios anteriores, se asignará el cupo de forma aleatoria.
# SEMILLA 2025-05-13 (Cierre de convocatoria)
#-------------------------------------------------------------------------------
set.seed(20250513)
JE3$SEMILLA <-  JE3$ID_PERSONA[sample(length(JE3$ID_PERSONA))]
sqldf("select SEMILLA FROM JE3 GROUP BY SEMILLA HAVING COUNT(1)>1")

#d)Se privilegiará el ingreso de egresados de colegios oficiales.
#----------------------------------
JE3 <- sqldf("select *,
               case --GRADUADO
                    when SIMAT_GM_SECTOR is not null and SIMAT_GM_SECTOR=='OFICIAL' then 1
                    when SIMAT_GM_SECTOR is not null then 2
                    when MEN_GM_SECTOR is not null and MEN_GM_SECTOR=='OFICIAL' then 1
                    when MEN_GM_SECTOR is not null then 2
                    
               else 3 end ORDENAMIENTO_D
               
              FROM JE3 ")
cro(JE3$ORDENAMIENTO_D)

#----------------------------------------
#ORDENAMIENTO FINAL
# 5.2.2. Actividad 3: Aplicación de criterios de desempate
#----------------------------------------
JE3 <- sqldf("select 
               ROW_NUMBER() OVER(ORDER BY PUNTAJE_GLOBAL desc,
                                          SABER11_PUNTAJE_GLOBAL desc,
                                          SABER11_PUESTO asc,
                                          SISBEN4_Grupo asc,
                                          SISBEN4_Nivel asc,
                                          CRITERIO_1_VUL_ESTRUCTURAL desc,
                                          ORDENAMIENTO_D asc,  
                                          SEMILLA asc ) AS JE3_LLAVE_PER,
               *
              FROM JE3")


#-------------------------------------------------------------------------------
# PREPARACION INFORMACION PERSONA-OFERTA
#-------------------------------------------------------------------------------

# PEGAR VARIABLES DE PERSONA UNICA
dim(JE3_PER_OFERTA)
JE3_PER_OFERTA <- merge(x=JE3_PER_OFERTA, y=JE3[,c("ID_PERSONA","codloc_SICORE","LOCALIDAD_SICORE","GEO_ANALISIS","codloc","LOCALIDAD","UAESP","JE3_LLAVE_PER","HABILITADO")], by="ID_PERSONA", all = FALSE )
dim(JE3_PER_OFERTA)

#-------------------------------------------------------------------------------
# CONSTRUCCION DATAFRAME DE HABILITADOS POR PERSONA PARA
# 1. CALCULO INHABILITAD POR IES (5.1.3.1. Requisitos y costos adicionales establecidos por las IES)
# 2. CALCULO DE ELEGIBLES
#-------------------------------------------------------------------------------

TMP_ELEGIBLE<- JE3_PER_OFERTA[JE3_PER_OFERTA$HABILITADO=="HABILITADO" 
                                     , c("JE3_LLAVE_PER","ID_OFERTA_PROGRAMA","PRIORIDAD","CODIGO_SNIES_IES", "NOMBRE_INSTITUCION_SUPERIOR","CODIGO_SNIES_PROGRAMA","NOMBRE_PROGRAMA","NIVEL_FORMACION","ID_PERSONA","ID_PERSONA_OFERTA","codloc_SICORE","LOCALIDAD_SICORE", "codloc","LOCALIDAD","UAESP", "HABILITADO")]

#-----------------------------------------------------------
# VALIDACIONES POR CONDICIONES ESPECIFICAS IES Y PROGRAMAS
#-----------------------------------------------------------

TMP_ELEGIBLE <- merge(x=TMP_ELEGIBLE, y=JE3[,c("ID_PERSONA", "SABER11_REGISTRO_SNP","SABER11_PERIODO","SABER11_PUNTAJE_GLOBAL")], by="ID_PERSONA", all = FALSE)

TMP_ELEGIBLE$HABILITADO_IES <- NA


# UNIVERSIDAD DE LOS ANDES (REQUISITO DE ADMISIÓN)
# 370 puntos en la prueba Saber 11 para Ingenierías y Economía y 350 puntos en los demás programas

# INGENIERIAS y ECONOMIA
TMP_ELEGIBLE[TMP_ELEGIBLE$CODIGO_SNIES_IES==1813 & TMP_ELEGIBLE$CODIGO_SNIES_PROGRAMA %in% c(91142,1539,1541,4690,1535) &  !is.na(TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL) & TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL >=370, "HABILITADO_IES"] <- "1-HABILITADO"
# Otros Programas
TMP_ELEGIBLE[TMP_ELEGIBLE$CODIGO_SNIES_IES==1813 & TMP_ELEGIBLE$CODIGO_SNIES_PROGRAMA %in% c(104693,106784,106785,106783) &  !is.na(TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL) & TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL >=350, "HABILITADO_IES"] <- "1-HABILITADO"
TMP_ELEGIBLE[TMP_ELEGIBLE$CODIGO_SNIES_IES==1813 & is.na(TMP_ELEGIBLE$HABILITADO_IES), "HABILITADO_IES"] <-"2-INHABILITA_CRITERIO_IES" 

#-------------------------------------------------------------------------------
# MARCACION VACIOS COMO HABILITADO
TMP_ELEGIBLE[is.na(TMP_ELEGIBLE$HABILITADO_IES), "HABILITADO_IES"] <- "0-NOAPLICA"
cro(TMP_ELEGIBLE$HABILITADO_IES)

# PEGUE RESULTADO HABILITADO IES
JE3_PER_OFERTA <- merge(x=JE3_PER_OFERTA, y=TMP_ELEGIBLE[,c("ID_PERSONA_OFERTA","HABILITADO_IES")],  by="ID_PERSONA_OFERTA", all.x = TRUE )


# EXCLUIR REGISTROS QUE NO CUMPLEN LOS CRITERIOS DE LAS IES
TMP_ELEGIBLE <- TMP_ELEGIBLE[TMP_ELEGIBLE$HABILITADO_IES != "2-INHABILITA_CRITERIO_IES",]
cro(TMP_ELEGIBLE$HABILITADO_IES)

# CREACION VARIABLE PARA EL CALCULO DE ELEGIBLES
TMP_ELEGIBLE$ELEGIBLE <- NA
TMP_ELEGIBLE$ASIGNACION <- NA
TMP_ELEGIBLE$N_REG_OFERTA <- NA

#-------------------------------------------------------------------------------
#ELEGIBLES 
#-------------------------------------------------------------------------------

TMP_ELEGIBLE<- TMP_ELEGIBLE[order(TMP_ELEGIBLE$JE3_LLAVE_PER, TMP_ELEGIBLE$PRIORIDAD),] 
BASE_OFERTA_FDL$CUPOS_CONTROL<-BASE_OFERTA_FDL$CUPOS_ASIGNADOS


for (aspirante in unique(TMP_ELEGIBLE$JE3_LLAVE_PER)) { # RECORRER POR PERSONA ordenada por la LLAVE
  print(aspirante)
  
  # DATAFRAME DE LA PERSONA
  PERSONA_HABILITADA <- TMP_ELEGIBLE[TMP_ELEGIBLE$JE3_LLAVE_PER==aspirante,]
  #LOCALIDAD DE LA PERSONA
  PERSONA_LOCALIDAD <- unique(PERSONA_HABILITADA$codloc)
  
  #SE ORDENA EL DATAFRAME DE LAS OFERTAS DE LA PERSONA POR PRIORIDAD
  PERSONA_HABILITADA<- PERSONA_HABILITADA[order(PERSONA_HABILITADA$PRIORIDAD),] 
  
  # SE RECORRE LAS OFERTAS APLICADAS POR LA PERSONA
  for (prioridad in PERSONA_HABILITADA$PRIORIDAD) {
    
    # DATAFRAME DE LA PERSONA_OFERTA X PRIORIDAD
    PERSONA_OFERTA <- PERSONA_HABILITADA[PERSONA_HABILITADA$PRIORIDAD==prioridad,]
    
    #DATOS OFERTA A EVALUAR
    EVALUAR_OFERTA <- BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA,]
    
    #---------------------------------------------------------------------------
    #--------------------------------DEFINIR LA OFERTA-----------------------
    # SE PRIORIZA OFERTA DE LOCALIDAD CUANDO ES IGUAL A LA LOCALIDAD DE LA PERSONA
    # EN CASO QUE NO EXISTA OFERTA POR LOCALIDAD SE PRIORIZA LA OFERTA SIN LOCALIDAD
    #---------------------------------------------------------------------------
    # 1. LA OFERTA CUENTA CON CUPOS
    EVALUAR_OFERTA <- EVALUAR_OFERTA[EVALUAR_OFERTA$CUPOS_CONTROL >0,]
    
    # La oferta tienen registros donde hay cupos
    if(nrow(EVALUAR_OFERTA) > 0){
      
      # 2. Hay Cupos en la localidad de la persona
      if(!is.na(PERSONA_LOCALIDAD)){
        # Crear Dataframe unicamente con las ofertas que le aplican por localidad
        EVALUAR_OFERTA_LOC <- EVALUAR_OFERTA[!is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$codloc==PERSONA_LOCALIDAD,]  
        # SI hay cupos en la oferta para la localidad (priorizo localidad sobre Atenea)
        if(nrow(EVALUAR_OFERTA_LOC) > 0){
          EVALUAR_OFERTA <- EVALUAR_OFERTA_LOC[EVALUAR_OFERTA_LOC$N_REG_OFERTA == min(EVALUAR_OFERTA_LOC$N_REG_OFERTA), ]  
        }else { #3. Se evalua la oferta Sin Localidad
          EVALUAR_OFERTA <- EVALUAR_OFERTA[is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$N_REG_OFERTA == min(EVALUAR_OFERTA$N_REG_OFERTA), ]
        }
      }else{#4. Se evalua la oferta Sin Localidad
        EVALUAR_OFERTA <- EVALUAR_OFERTA[is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$N_REG_OFERTA == min(EVALUAR_OFERTA$N_REG_OFERTA), ]
      }
    }else
    {
      #print("NO HAY CUPOS EN OFERTA")
    }
    
    
    #---------------------------------------------------------------------------
    #-------------------------RECORRIDO CUANDO HAY OFERTA-----------------------
    #---------------------------------------------------------------------------
    if(nrow(EVALUAR_OFERTA) > 0){
      
    #---------------------------------------------------------------------------
    ###---------------------------UAESP-----------------------------------------
    #---------------------------------------------------------------------------
    #PREGUNTAMOS SI LA OFERRA ES GENERAL (si alcanza para el costo de la corte da un cupo)  Y CUPOS disponibles
    if(!is.na(PERSONA_OFERTA$UAESP) & EVALUAR_OFERTA$TIPO_OFERTA=="GENERAL" & EVALUAR_OFERTA$CUPOS_CONTROL > 0 & is.na(PERSONA_OFERTA$ELEGIBLE)) {
      
      #COMO CUMPLE HACEMOS LAS MARCACIONES CORRESPONDIENTES
      #MARCAMOS DAFRAME PERSONA
      PERSONA_HABILITADA[PERSONA_HABILITADA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL
      PERSONA_HABILITADA[is.na(PERSONA_HABILITADA$ELEGIBLE) & PERSONA_HABILITADA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- "-"
      
      #MARCAMOS DATAFRAME DE ELEGIBLES
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ASIGNACION"] <- "UAESP"
      TMP_ELEGIBLE[is.na(TMP_ELEGIBLE$ELEGIBLE) & TMP_ELEGIBLE$ID_PERSONA == PERSONA_OFERTA$ID_PERSONA & TMP_ELEGIBLE$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA, "ELEGIBLE"] <- "-"
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "N_REG_OFERTA"] <- EVALUAR_OFERTA$N_REG_OFERTA
      
      #AFECTAMOS CUPO
      nuevo_cupo<- EVALUAR_OFERTA$CUPOS_CONTROL - 1
      BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA & BASE_OFERTA_FDL$N_REG_OFERTA==EVALUAR_OFERTA$N_REG_OFERTA, "CUPOS_CONTROL" ] <- nuevo_cupo

    }
    #---------------------------------------------------------------------------
    ###--------------------GENERAL Y RECOMPOSICION------------------------------
    #---------------------------------------------------------------------------
    #PREGUNTAMOS POR CUPOS disponibles en recomposicion
    else if(EVALUAR_OFERTA$CUPOS_CONTROL > 0 & is.na(PERSONA_OFERTA$ELEGIBLE)) {
      
      #COMO CUMPLE HACEMOS LAS MARCACIONES CORRESPONDIENTES
      #MARCAMOS DAFRAME PERSONA
      PERSONA_HABILITADA[PERSONA_HABILITADA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL
      PERSONA_HABILITADA[is.na(PERSONA_HABILITADA$ELEGIBLE) & PERSONA_HABILITADA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- "-"
      
      #MARCAMOS DATAFRAME DE ELEGIBLES
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ASIGNACION"] <- paste(EVALUAR_OFERTA$TIPO_OFERTA, EVALUAR_OFERTA$FUENTE, sep = "-" ) 
      TMP_ELEGIBLE[is.na(TMP_ELEGIBLE$ELEGIBLE) & TMP_ELEGIBLE$ID_PERSONA == PERSONA_OFERTA$ID_PERSONA & TMP_ELEGIBLE$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA, "ELEGIBLE"] <- "-"
      TMP_ELEGIBLE[TMP_ELEGIBLE$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "N_REG_OFERTA"] <- EVALUAR_OFERTA$N_REG_OFERTA
      
      #AFECTAMOS CUPO
      nuevo_cupo<- EVALUAR_OFERTA$CUPOS_CONTROL - 1
      BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA & BASE_OFERTA_FDL$N_REG_OFERTA==EVALUAR_OFERTA$N_REG_OFERTA, "CUPOS_CONTROL" ] <- nuevo_cupo
      
    }
    
    } # SIN OFERTA A EVALUAR    
  }# CIERRE RECORRIDO OFERTAS DE LA PERSONA
}# CIERRE PERSONA

cro(TMP_ELEGIBLE$ASIGNACION)

#------------------------------------------------------------
# PEGAR RESULTADO
#------------------------------------------------------------
dim(JE3_PER_OFERTA)
JE3_PER_OFERTA<- merge(x=JE3_PER_OFERTA, y=TMP_ELEGIBLE[,c("ID_PERSONA_OFERTA", "ELEGIBLE", "ASIGNACION", "N_REG_OFERTA")], by="ID_PERSONA_OFERTA", all.x = TRUE )
dim(JE3_PER_OFERTA)

#-------------------------------------------------------------------------------
# CONSTRUCCCION ESTADO ELEGIBLE
#-------------------------------------------------------------------------------
JE3_PER_OFERTA$ESTADO_ELEGIBLE<- NA
JE3_PER_OFERTA[JE3_PER_OFERTA$HABILITADO!="HABILITADO","ESTADO_ELEGIBLE"]<-"NO_ELEGIBLE"
JE3_PER_OFERTA[is.na(JE3_PER_OFERTA$ESTADO_ELEGIBLE) & !is.na(JE3_PER_OFERTA$HABILITADO_IES) & JE3_PER_OFERTA$HABILITADO_IES=="2-INHABILITA_CRITERIO_IES","ESTADO_ELEGIBLE"]<-"NO_ELEGIBLE"
JE3_PER_OFERTA[is.na(JE3_PER_OFERTA$ESTADO_ELEGIBLE) & JE3_PER_OFERTA$PRIORIDAD > 3  ,"ESTADO_ELEGIBLE"]<-"NO_ELEGIBLE"
JE3_PER_OFERTA[!is.na(JE3_PER_OFERTA$ASIGNACION) & is.na(JE3_PER_OFERTA$ESTADO_ELEGIBLE),"ESTADO_ELEGIBLE"]<-"ELEGIBLE"
JE3_PER_OFERTA[!is.na(JE3_PER_OFERTA$ELEGIBLE) & JE3_PER_OFERTA$ELEGIBLE=="-" ,"ESTADO_ELEGIBLE"]<-"INACTIVO"
JE3_PER_OFERTA[is.na(JE3_PER_OFERTA$ESTADO_ELEGIBLE),"ESTADO_ELEGIBLE"]<-"DISPONIBLE"
cro(JE3_PER_OFERTA$ESTADO_ELEGIBLE)
# BORRAR VARIABLES Y DATAFRAME QUE NO CUMPLEN SU OBJETO PARA EL CALCULO Y NO SE USAN MAS
rm(aspirante,nuevo_cupo,PERSONA_LOCALIDAD,prioridad,EVALUAR_OFERTA,EVALUAR_OFERTA_LOC,PERSONA_HABILITADA,PERSONA_OFERTA)



#-------------------------------------------------------------------------------
# CALCULO LISTA DE ESPERA
#-------------------------------------------------------------------------------

# CALCULO CUPOS PARA LA LISTA DE ESPERA
BASE_OFERTA_FDL$CUPOS_ASIGNADOS_LP <- BASE_OFERTA_FDL$CUPOS_ASIGNADOS 

# CUANDO EL REGISTRO DE OFERTA SON 3 CUPOS O MENOS LA LISTA DE ESPERA ES EL DOBLE
# CUANDO EL REGISTRO DE OFERTA ES MAYOR A 3 CUPOS LA LISTA DE ESPERA ES IGUAL
BASE_OFERTA_FDL[BASE_OFERTA_FDL$CUPOS_ASIGNADOS_LP < 4 , "CUPOS_ASIGNADOS_LP" ] <- BASE_OFERTA_FDL[BASE_OFERTA_FDL$CUPOS_ASIGNADOS_LP < 4 , "CUPOS_ASIGNADOS_LP" ] * 2


TMP_LISTA<-JE3_PER_OFERTA[JE3_PER_OFERTA$ESTADO_ELEGIBLE=="DISPONIBLE",]
TMP_LISTA<- TMP_LISTA[order(TMP_LISTA$JE3_LLAVE_PER),]

BASE_OFERTA_FDL$CUPOS_CONTROL_LP<-BASE_OFERTA_FDL$CUPOS_ASIGNADOS_LP


for (aspirante in unique(TMP_LISTA$JE3_LLAVE_PER)) { # RECORRER POR PERSONA ordenada por la LLAVE
  print(aspirante)
  
  # DATAFRAME DE LA PERSONA
  PERSONA_HABILITADA <- TMP_LISTA[TMP_LISTA$JE3_LLAVE_PER==aspirante,]
  #LOCALIDAD DE LA PERSONA
  PERSONA_LOCALIDAD <- unique(PERSONA_HABILITADA$codloc)
  
  #SE ORDENA EL DATAFRAME DE LAS OFERTAS DE LA PERSONA POR PRIORIDAD
  PERSONA_HABILITADA<- PERSONA_HABILITADA[order(PERSONA_HABILITADA$PRIORIDAD),] 
  
  # SE RECORRE LAS OFERTAS APLICADAS POR LA PERSONA
  for (prioridad in PERSONA_HABILITADA$PRIORIDAD) {
    
    # DATAFRAME DE LA PERSONA_OFERTA X PRIORIDAD
    PERSONA_OFERTA <- PERSONA_HABILITADA[PERSONA_HABILITADA$PRIORIDAD==prioridad,]
    
    #DATOS OFERTA A EVALUAR
    EVALUAR_OFERTA <- BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA,]
    
    #---------------------------------------------------------------------------
    #--------------------------------DEFINIR LA OFERTA-----------------------
    # SE PRIORIZA OFERTA DE LOCALIDAD CUANDO ES IGUAL A LA LOCALIDAD DE LA PERSONA
    # EN CASO QUE NO EXISTA OFERTA POR LOCALIDAD SE PRIORIZA LA OFERTA SIN LOCALIDAD
    #---------------------------------------------------------------------------
    # 1. OFERTA CON CUPOS
    EVALUAR_OFERTA <- EVALUAR_OFERTA[EVALUAR_OFERTA$CUPOS_CONTROL_LP >0,]
    
    if(nrow(EVALUAR_OFERTA) > 0){
      
      # 2. Hay Cupos en la localidad de la persona
      if(!is.na(PERSONA_LOCALIDAD)){
        # Crear Dataframe unicamente con las ofertas que le aplican por localidad
        EVALUAR_OFERTA_LOC <- EVALUAR_OFERTA[!is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$codloc==PERSONA_LOCALIDAD,]  
        
        # SI hay cupos en la oferta para la localidad (priorizo localidad sobre Atenea)
        if(nrow(EVALUAR_OFERTA_LOC) > 0){
          EVALUAR_OFERTA <- EVALUAR_OFERTA_LOC[EVALUAR_OFERTA_LOC$N_REG_OFERTA == min(EVALUAR_OFERTA_LOC$N_REG_OFERTA), ]  
        }else { #3. Se evalua la oferta Sin Localidad
          EVALUAR_OFERTA <- EVALUAR_OFERTA[is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$N_REG_OFERTA == min(EVALUAR_OFERTA$N_REG_OFERTA), ]
        }
      }else{ #4. Se evalua la oferta Sin Localidad
        EVALUAR_OFERTA <- EVALUAR_OFERTA[is.na(EVALUAR_OFERTA$codloc) & EVALUAR_OFERTA$N_REG_OFERTA == min(EVALUAR_OFERTA$N_REG_OFERTA), ]
      }
    }else
    {
      #print("NO HAY CUPOS EN OFERTA")
    }
    
    
    #---------------------------------------------------------------------------
    #-------------------------RECORRIDO CUANDO HAY OFERTA-----------------------
    #---------------------------------------------------------------------------
    if(nrow(EVALUAR_OFERTA) > 0){
      
      #---------------------------------------------------------------------------
      ###---------------------------UAESP-----------------------------------------
      #---------------------------------------------------------------------------
      #PREGUNTAMOS SI LA OFERRA ES GENERAL (si alcanza para el costo de la corte da un cupo)  Y CUPOS disponibles
      if(!is.na(PERSONA_OFERTA$UAESP) & EVALUAR_OFERTA$TIPO_OFERTA=="GENERAL" & EVALUAR_OFERTA$CUPOS_CONTROL_LP > 0 & is.na(PERSONA_OFERTA$ELEGIBLE)) {
        
        #COMO CUMPLE HACEMOS LAS MARCACIONES CORRESPONDIENTES
        #MARCAMOS DAFRAME PERSONA
        PERSONA_HABILITADA[PERSONA_HABILITADA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL_LP
        PERSONA_HABILITADA[is.na(PERSONA_HABILITADA$ELEGIBLE) & PERSONA_HABILITADA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- "-"
        
        #MARCAMOS DATAFRAME DE ELEGIBLES
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL_LP
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ASIGNACION"] <- "UAESP"
        TMP_LISTA[is.na(TMP_LISTA$ELEGIBLE) & TMP_LISTA$ID_PERSONA == PERSONA_OFERTA$ID_PERSONA & TMP_LISTA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA, "ELEGIBLE"] <- "-"
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "N_REG_OFERTA"] <- EVALUAR_OFERTA$N_REG_OFERTA
        
        #AFECTAMOS CUPO
        nuevo_cupo<- EVALUAR_OFERTA$CUPOS_CONTROL_LP - 1
        BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA & BASE_OFERTA_FDL$N_REG_OFERTA==EVALUAR_OFERTA$N_REG_OFERTA, "CUPOS_CONTROL_LP" ] <- nuevo_cupo
        
      }
      #---------------------------------------------------------------------------
      ###--------------------GENERAL Y RECOMPOSICION------------------------------
      #---------------------------------------------------------------------------
      #PREGUNTAMOS POR CUPOS disponibles en recomposicion
      else if(EVALUAR_OFERTA$CUPOS_CONTROL_LP > 0 & is.na(PERSONA_OFERTA$ELEGIBLE)) {
        
        #COMO CUMPLE HACEMOS LAS MARCACIONES CORRESPONDIENTES
        #MARCAMOS DAFRAME PERSONA
        PERSONA_HABILITADA[PERSONA_HABILITADA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL_LP
        PERSONA_HABILITADA[is.na(PERSONA_HABILITADA$ELEGIBLE) & PERSONA_HABILITADA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- "-"
        
        #MARCAMOS DATAFRAME DE ELEGIBLES
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ELEGIBLE"] <- EVALUAR_OFERTA$CUPOS_CONTROL_LP
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "ASIGNACION"] <- paste(EVALUAR_OFERTA$TIPO_OFERTA, EVALUAR_OFERTA$FUENTE, sep = "-" ) 
        TMP_LISTA[is.na(TMP_LISTA$ELEGIBLE) & TMP_LISTA$ID_PERSONA == PERSONA_OFERTA$ID_PERSONA & TMP_LISTA$ID_PERSONA_OFERTA != PERSONA_OFERTA$ID_PERSONA_OFERTA, "ELEGIBLE"] <- "-"
        TMP_LISTA[TMP_LISTA$ID_PERSONA_OFERTA == PERSONA_OFERTA$ID_PERSONA_OFERTA , "N_REG_OFERTA"] <- EVALUAR_OFERTA$N_REG_OFERTA
        
        #AFECTAMOS CUPO
        nuevo_cupo<- EVALUAR_OFERTA$CUPOS_CONTROL_LP - 1
        BASE_OFERTA_FDL[BASE_OFERTA_FDL$ID_OFERTA_PROGRAMA==PERSONA_OFERTA$ID_OFERTA_PROGRAMA & BASE_OFERTA_FDL$N_REG_OFERTA==EVALUAR_OFERTA$N_REG_OFERTA, "CUPOS_CONTROL_LP" ] <- nuevo_cupo
        
      }
      
    } # SIN OFERTA A EVALUAR    
  }# CIERRE RECORRIDO OFERTAS DE LA PERSONA
}# CIERRE PERSONA

cro(TMP_LISTA$ASIGNACION)

TMP_LISTA[!is.na(TMP_LISTA$ASIGNACION) & TMP_LISTA$ESTADO_ELEGIBLE=="DISPONIBLE","ESTADO_ELEGIBLE"]<-"LISTA_ESPERA"
TMP_LISTA[!is.na(TMP_LISTA$ELEGIBLE) & TMP_LISTA$ELEGIBLE=="-" ,"ESTADO_ELEGIBLE"]<-"INACTIVO_LISTA_ESPERA"
cro(TMP_LISTA$ESTADO_ELEGIBLE)

#------------------------------------------------------------
#AJUSTAR ESTADO PARA PEGAR EN JE1_PER_OFERTA
#------------------------------------------------------------
colnames(TMP_LISTA)[colnames(TMP_LISTA)=="ESTADO_ELEGIBLE"]<-"ESTADO_LISTA_ESPERA"
colnames(TMP_LISTA)[colnames(TMP_LISTA)=="ASIGNACION"]<-"ASIGNACION_LISTA_ESPERA"
colnames(TMP_LISTA)[colnames(TMP_LISTA)=="N_REG_OFERTA"]<-"N_REG_OFERTA_LISTA_ESPERA"
cro(TMP_LISTA$ESTADO_LISTA_ESPERA)

#------------------------------------------------------------
# PEGAR RESULTADO
#------------------------------------------------------------
JE3_PER_OFERTA<- merge(x=JE3_PER_OFERTA, y=TMP_LISTA[TMP_LISTA$ESTADO_LISTA_ESPERA!="DISPONIBLE",c("ID_PERSONA_OFERTA","ASIGNACION_LISTA_ESPERA","N_REG_OFERTA_LISTA_ESPERA", "ESTADO_LISTA_ESPERA" )], by="ID_PERSONA_OFERTA", all.x = TRUE )
JE3_PER_OFERTA[!is.na(JE3_PER_OFERTA$ESTADO_LISTA_ESPERA),"ESTADO_ELEGIBLE"]<-JE3_PER_OFERTA[!is.na(JE3_PER_OFERTA$ESTADO_LISTA_ESPERA),"ESTADO_LISTA_ESPERA"]

cro(JE3_PER_OFERTA$ESTADO_ELEGIBLE)

# BORRAR VARIABLES Y DATAFRAME QUE NO CUMPLEN SU OBJETO PARA EL CALCULO Y NO SE USAN MAS
rm(TMP_ELEGIBLE,TMP_LISTA,aspirante,nuevo_cupo,PERSONA_OFERTA,prioridad, PERSONA_HABILITADA,PERSONA_LOCALIDAD,EVALUAR_OFERTA,EVALUAR_OFERTA_LOC)


#===============================================================================
#MARCACION ESTADO PARA TIC
#===============================================================================
JE3_PER_OFERTA[JE3_PER_OFERTA$ESTADO_ELEGIBLE %in% c("ELEGIBLE","LISTA_ESPERA"),"RESULTADO"] <- JE3_PER_OFERTA[JE3_PER_OFERTA$ESTADO_ELEGIBLE %in% c("ELEGIBLE","LISTA_ESPERA"),"ESTADO_ELEGIBLE"]
JE3_PER_OFERTA[is.na(JE3_PER_OFERTA$RESULTADO),"RESULTADO"]<-"NO_ELEGIBLE"
cro(JE3_PER_OFERTA$RESULTADO)
cro(JE3_PER_OFERTA$ESTADO_ELEGIBLE, JE3_PER_OFERTA$RESULTADO)

#===============================================================================
# #PUESTO UNICAMENTE PARA ELEGIBLES, LISTA DE ESPERA y HABILITADOS (DISPONIBLES)
#===============================================================================
PUESTOS<-  JE3_PER_OFERTA[JE3_PER_OFERTA$ESTADO_ELEGIBLE %in% c("ELEGIBLE","LISTA_ESPERA","DISPONIBLE") ,
                           c( "ID_PERSONA_OFERTA","ID_OFERTA_PROGRAMA","N_REG_OFERTA","CODIGO_SNIES_PROGRAMA","JE3_LLAVE_PER","ESTADO_ELEGIBLE")]

#PARA EL CALCULO DEL PUESTO SE PRIORIZA 1 ELEGIBLES, 2 LISTA DE ESPERA, 3 EL RESTO
PUESTOS$ORDEN_ESTADO <- 3
PUESTOS[PUESTOS$ESTADO_ELEGIBLE=="ELEGIBLE", "ORDEN_ESTADO"] <-1
PUESTOS[PUESTOS$ESTADO_ELEGIBLE=="LISTA_ESPERA", "ORDEN_ESTADO"] <-2
cro(PUESTOS$ORDEN_ESTADO, PUESTOS$ESTADO_ELEGIBLE)

PUESTOS <- sqldf("select 
              *,
              ROW_NUMBER() OVER(PARTITION BY ID_OFERTA_PROGRAMA,CODIGO_SNIES_PROGRAMA ORDER BY ORDEN_ESTADO asc, JE3_LLAVE_PER asc) AS PUESTO
              FROM PUESTOS ")
dim(JE3_PER_OFERTA)
JE3_PER_OFERTA<- merge(x=JE3_PER_OFERTA, y=PUESTOS[,c("ID_PERSONA_OFERTA","PUESTO")], by="ID_PERSONA_OFERTA", all.x = TRUE)
dim(JE3_PER_OFERTA)
# BORRAR VARIABLES PARA CALCULO DEL PUESTO
rm(PUESTOS)
