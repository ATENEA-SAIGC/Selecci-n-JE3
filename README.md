# JE3 — ¿Cómo se eligen los beneficiarios? (Explicado para público no técnico)

> **Propósito:** Explicar, en términos sencillos, cómo funciona el código que calcula **quiénes quedan habilitados**, **cómo se puntúan**, **cómo se desempatan** y **cómo se asignan los cupos y las listas de espera** en la convocatoria **Jóvenes a la E (JE3) – Bogotá**.  
> **Tecnología usada:** R (consultas tipo SQL con `sqldf` y transformaciones con `dplyr`/`tidyr`).  

---

## 1) Visión general del proceso

1. **Cargue de insumos**: se abre el archivo con los datos de la convocatoria y la oferta educativa.
2. **Habilitación**: se verifica, **regla por regla**, si cada aspirante cumple requisitos mínimos (criterios **A** a **I**).
3. **Puntuación**: a quienes quedaron habilitados se les asigna un puntaje por **vulnerabilidad estructural**, **vulnerabilidad económica**, **mérito académico** y **trayectorias**.
4. **Ordenamiento y desempates**: se ordena a las personas por puntaje y, si hay empates, se aplican reglas de desempate y, en último caso, **sorteo con semilla fija** (reproducible).
5. **Asignación de cupos por oferta**: se recorre cada persona (en su orden), se miran sus opciones por **prioridad** y la existencia de cupos, con preferencia por la **misma localidad**.
6. **Estados de elegibilidad**: cada persona‑oferta queda como **ELEGIBLE**, **LISTA_ESPERA**, **DISPONIBLE**, **INACTIVO** o **NO_ELEGIBLE**, según corresponda.
7. **Lista de espera**: se calculan cupos para lista de espera y se repite una asignación controlada.
8. **Puestos**: se numera el **puesto** final dentro de cada programa/oferta.

Al final se obtiene, para cada aspirante y cada oferta a la que aplicó, un **estado** y, si aplica, su **puesto** y **fuente de asignación**.

### 1.1) Flujo de ejecución (resumen técnico y “por dónde pasa”)

![alt text](diagrama-flujo.png)

---

## 2) Detalle del Proceso 
### 2.1 ─ Datos de entrada (lo mínimo que hay que saber)

Al cargar los insumos se obtienen 3 tablas principales:

- **`BASE_OFERTA_FDL`**: cupos por programa/oferta, tipo de oferta y fuente de financiación (incluye localidad, si aplica).
- **`JE3`**: una fila por persona (base anonimizada) con resultados de cruces externos: educación previa, SISBÉN, pruebas Saber 11, etc.
- **`JE3_PER_OFERTA`**: las **opciones** a las que se inscribió cada persona, con su **prioridad** (1, 2, 3…) y marcas como **UAESP** cuando aplica.


---

### 2.2 ─ Habilitación: requisitos A–I (secuencial)

La habilitación es **secuencial**: la primera regla que no se cumple **inhabilita** a la persona.

#### 📌 Ejemplo de cálculo de edad
```r
JE3$CORTE_EDAD <- as.integer(
  age_calc(as.Date(JE3$FECHA_NACIMIENTO, "%Y-%m-%d"),
           enddate = as.Date("2025-05-13"),
           units = "years",
           precise = TRUE)
)
```
- Se calcula la **edad** al **13‑may‑2025** (cierre de convocatoria).
- Se usa para el **criterio C** (máximo 28 años).

1. Criterio **A** — Bachiller de Bogotá o validación en Bogotá
Se revisan fuentes de graduación (SIMAT/MEN) y validación ICFES.
    - Si **no** hay registro de grado ni Saber 11 → **no habilitado**.
    - Si validó en Bogotá pero el puntaje es **< 30** o no rindió en Bogotá → **no habilitado**.
    - Si todo está en regla → **HABILITADO_A**.

2. Criterio **B** — Presentó Saber 11/ICFES
    - Si no tiene periodo de Saber 11 → **NO_HABILITADO**.
    - Si sí → **HABILITADO_B**.

3. Criterio **C** — Edad ≤ 28 años
    - Edad > 28 → **NO_HABILITADO**; de lo contrario → **HABILITADO_C**.

4. Criterio **D** — Situación TYT/SENA en el último año
    - Se contemplan combinaciones de **matrícula reciente TYT** y si la persona **se graduó** o está en el **SENA**.
    - Ciertas combinaciones inhabilitan (p.ej., **matriculado, no graduado, sin SENA**).

5. Criterio **E** — No cursar carrera profesional en el último año
    - Si hay **matrícula reciente** en superior universitario → **NO_HABILITADO**.

6. Criterio **F** — No ser egresado profesional
    - Si es **graduado universitario** (o con especialización universitaria) → **NO_HABILITADO**.

7. Criterio **G** — Inscripción en SICORE
    - **No aplica** (todo el universo proviene de SICORE).

8. Criterio **H** — No haber sido beneficiario previo / otros fondos
    - Si aparece como beneficiario previo de **JU/UTC/fondos** → **NO_HABILITADO**.

9. Criterio **I** — Identidad (RNEC)
    - Si RNEC reporta inconsistencia → **NO_HABILITADO**.

#### Secuencia de Inhabilitación
```r
JE3 <- sqldf("SELECT *,
  CASE
    WHEN HABILITADO_A != 'HABILITADO' THEN 'INHABILITA_REQUISITO_A'
    WHEN HABILITADO_B != 'HABILITADO' THEN 'INHABILITA_REQUISITO_B'
    WHEN HABILITADO_C != 'HABILITADO' THEN 'INHABILITA_REQUISITO_C'
    WHEN HABILITADO_D != 'HABILITADO' THEN 'INHABILITA_REQUISITO_D'
    WHEN HABILITADO_E != 'HABILITADO' THEN 'INHABILITA_REQUISITO_E'
    WHEN HABILITADO_F != 'HABILITADO' THEN 'INHABILITA_REQUISITO_F'
    WHEN HABILITADO_H != 'HABILITADO' THEN 'INHABILITA_REQUISITO_H'
    WHEN HABILITADO_I != 'HABILITADO' THEN 'INHABILITA_REQUISITO_I'
    ELSE 'HABILITADO' END AS HABILITADO
  FROM JE3")
```
- La **primera** regla que falla define el motivo de inhabilitación.
- Si ninguna falla → **HABILITADO**.

---

### 2.3 ─ Puntuación (para habilitados)

Se suman 4 criterios para obtener el **`PUNTAJE_GLOBAL`**.

#### A. Vulnerabilidad estructural (hasta **15 pts**, **acumulable**)
- Suma puntos por: ser mujer, ser madre/padre (según reglas), identidad de género, pertenencia étnica, víctima, discapacidad, etc.
- Si la suma supera 15, se **cap** en **15**.

#### B. Vulnerabilidad económica (**no acumulable**)
- Con **SISBÉN IV**: A=40, B=30, C=20, otro=0.
- **Sin SISBÉN**: se miran señales alternativas (colegio oficial rural, pertenencia indígena con reglas, egreso de oficial), y se asigna **un solo bloque** (40/30/15/0) según corresponda.

#### C. Mérito académico (hasta **40 pts**)
- Para **periodos recientes (≥ 2016‑1)** se usa el **percentil nacional** del Saber 11 (100 → 40 pts; 95‑99 → 35; …).
- Para **periodos antiguos (< 2016‑1)** se usa el **puesto** (1‑10 → 40; 11‑50 → 35; …).

#### D. Trayectorias (hasta **5 pts**, **no acumulable**)
- Se otorgan **5 pts** si la persona participa en **alguna** trayectoria priorizada (UTC, Consejero de Juventud, IDRD, Parceros, etc.).

#### E. Suma total
```r
JE3$PUNTAJE_GLOBAL <-
  JE3$CRITERIO_1_VUL_ESTRUCTURAL +
  JE3$CRITERIO_2_VUL_ECONOMICA   +
  JE3$CRITERIO_3_MERITO_ACADEMICO+
  JE3$CRITERIO_4_TRAYECTORIA
```

---

### 2.4 ─ Ordenamiento y desempates

1. **Orden principal**: `PUNTAJE_GLOBAL` (desc).
2. **Desempates sucesivos**: puntaje global de Saber 11 (desc), puesto (asc), grupo/nivel SISBÉN (asc), vulnerabilidad estructural (desc), **colegio oficial** primero, y **sorteo aleatorio reproducible**.
3. **Semilla fija** del sorteo: `20250513` (fecha de cierre).

```r
set.seed(20250513)
JE3$SEMILLA <- JE3$ID_PERSONA[sample(length(JE3$ID_PERSONA))]

JE3 <- sqldf("SELECT
  ROW_NUMBER() OVER (ORDER BY
    PUNTAJE_GLOBAL DESC,
    SABER11_PUNTAJE_GLOBAL DESC,
    SABER11_PUESTO ASC,
    SISBEN4_Grupo ASC,
    SISBEN4_Nivel ASC,
    CRITERIO_1_VUL_ESTRUCTURAL DESC,
    ORDENAMIENTO_D ASC,
    SEMILLA ASC
  ) AS JE3_LLAVE_PER, * FROM JE3")
```
- **`JE3_LLAVE_PER`** es el **orden final** de las personas (llave de recorrido).

---

### 2.5 ─ Reglas específicas de IES/Programa (admisión mínima)

Para ciertos programas, hay **mínimos de SABER 11**. Ejemplo **Universidad de los Andes**:
- Ingenierías y Economía: **≥ 370** puntos.
- Otros programas: **≥ 350** puntos.

Quien no cumple queda **“2‑INHABILITA_CRITERIO_IES”** en esa opción; lo demás sigue igual.

---

### 2.6 ─ Asignación de cupos (persona → opciones por prioridad)

Se hace un **recorrido por persona** (según `JE3_LLAVE_PER`). Para cada persona:

1. Se toman sus **opciones** (`JE3_PER_OFERTA`) ordenadas por **`PRIORIDAD`**.
2. Para cada opción se consulta la **oferta** (`BASE_OFERTA_FDL`) y los **cupos disponibles** (`CUPOS_CONTROL`).
3. **Preferencia por localidad**: si hay cupo en la **misma localidad** de la persona, se usa; si no, se evalúa la oferta **sin localidad**.
4. Si hay cupo:
   - Si la opción es **UAESP + GENERAL**, se asigna con esa marca.
   - Si es **GENERAL** o **RECOMPOSICIÓN**, se asigna con la fuente correspondiente.
   - Se **descuenta** el cupo en `CUPOS_CONTROL`.
   - Se marca la opción elegida como **ELEGIBLE** y el resto de opciones de la persona como **“-”** (no elegidas).
5. Si no hay cupo en esa prioridad, se intenta con la siguiente prioridad.

**Resultado por persona‑oferta** (`JE3_PER_OFERTA`):
- `ELEGIBLE`, `ASIGNACION`, `N_REG_OFERTA` y `ESTADO_ELEGIBLE` (ver siguiente sección).

---

### 2.7 ─ Estados finales de cada persona‑oferta

- **ELEGIBLE**: recibió cupo en la asignación principal.
- **LISTA_ESPERA**: recibió cupo de lista de espera (ver sección 9).
- **DISPONIBLE**: habilitado, pero **sin cupo** en la pasada principal.
- **INACTIVO**: opciones descendentes de alguien ya asignado en una anterior (se marcan con “‑”).
- **NO_ELEGIBLE**: no cumple habilitación o reglas específicas (IES), o la **prioridad > 3**.

Además se crea un campo **`RESULTADO`** para TIC:
- `RESULTADO` = **ELEGIBLE** / **LISTA_ESPERA**; en otro caso → **NO_ELEGIBLE**.

---

### 2.8 ─ Lista de espera (cálculo y asignación)

1. Se parte de los **DISPONIBLES** tras la primera asignación.
2. Se definen **cupos de lista de espera** (`CUPOS_ASIGNADOS_LP`):
   - Si la oferta tiene **≤ 3** cupos, la lista de espera es el **doble**.
   - Si es **> 3**, la lista de espera es **igual** a los cupos.
3. Se repite un **recorrido similar** al de la asignación principal, consumiendo `CUPOS_CONTROL_LP`.
4. El estado resultante en estas asignaciones pasa a **LISTA_ESPERA** (o **INACTIVO_LISTA_ESPERA** cuando aplica).

---

### 2.9 ─ Puestos dentro de cada programa/oferta

Consolida **ELEGIBLES**, **LISTA_ESPERA** y **DISPONIBLES** y asigna **`PUESTO`** por programa/oferta:
- Se priorizan: (1) ELEGIBLES, (2) LISTA_ESPERA, (3) resto.
- Luego el orden dentro de cada bloque usa `JE3_LLAVE_PER` (el orden global).

---

## 3) Snippets clave (con explicación paso a paso)

### 3.1 Semilla y desempates reproducibles
```r
set.seed(20250513)                
JE3$SEMILLA <- JE3$ID_PERSONA[        
  sample(length(JE3$ID_PERSONA))
]
```
Este “sorteo” **solo se usa** si persisten empates tras aplicar todas las reglas.
- El set.seet() fija la semilla (fecha de cierre) para que el sorteo sea replicable
- Luego hay un vector de “sorteo” que reordena los IDs para romper empates al final

### 3.2 Preferencia por localidad (elección de registro de oferta)
Pseudocódigo equivalente a lo que hace el script en R:
```text
1) Filtra la oferta con CUPOS_CONTROL > 0
2) Si la persona tiene localidad:
   2a) busca un registro de oferta en esa misma localidad
   2b) si existe, usa el de menor N_REG_OFERTA
   2c) si no existe, usa el registro “sin localidad” de menor N_REG_OFERTA
3) Si la persona no tiene localidad: usa el registro “sin localidad” de menor N_REG_OFERTA
```
- `N_REG_OFERTA` funciona como **desempatador** interno dentro de una misma oferta.

## 4) Reglas mínimas de admisión por IES (ej. Uniandes)
```r
# Ingenierías y Economía (>= 370)
TMP_ELEGIBLE[
  TMP_ELEGIBLE$CODIGO_SNIES_IES==1813 &
  TMP_ELEGIBLE$CODIGO_SNIES_PROGRAMA %in% c(91142,1539,1541,4690,1535) &
  !is.na(TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL) &
  TMP_ELEGIBLE$SABER11_PUNTAJE_GLOBAL >= 370,
  "HABILITADO_IES"
] <- "1-HABILITADO"

# Otros (>= 350); lo demás: "2-INHABILITA_CRITERIO_IES"
```
- Si no cumple el puntaje mínimo de admisión de la IES **para ese programa**, esa opción queda **inhabilitada**, pero **otras opciones** del aspirante siguen vigentes.

---

## 5) Glosario

- **Habilitado**: cumple requisitos mínimos A–I.
- **Puntaje global**: suma de 4 criterios (máx. 100).
- **Prioridad**: orden de preferencias declarado por el aspirante (1 es la más alta).
- **UAESP**: marca especial que, con oferta **GENERAL**, puede activar una asignación directa.
- **Fuente**: origen del cupo (GENERAL, RECOMPOSICIÓN, etc.).
- **Lista de espera**: cupos adicionales para cubrir desistimientos/no matriculados.
- **Puesto**: orden final del aspirante dentro del programa/oferta.

---

## 6) Preguntas frecuentes (FAQ)

**¿Quién decide en un empate total?**  
El sistema usa una **semilla fija** para un sorteo final, por lo que el resultado es **reproducible**.

**¿Qué pasa si no hay cupo en mi prioridad 1?**  
El algoritmo intenta con tu **prioridad 2** (y así sucesivamente), respetando la preferencia por **localidad** cuando existe.

**¿Puedo quedar no habilitado para una IES y seguir compitiendo por otra?**  
Sí. Las **reglas de IES** aplican **por programa**. Puedes no cumplir en uno y, sin embargo, **sí** cumplir en otro.

**¿La lista de espera es igual en todos los casos?**  
No. Si una oferta tiene **≤ 3** cupos, la lista de espera es el **doble**. Si tiene **> 3**, la lista de espera es **igual**.

---

## 7) Licencia y contacto

Este material explica a alto nivel el proceso de selección en JE3 – Bogotá, con base en el script operativo en R.  
Para consultas remitirse a https://www.agenciaatenea.gov.co/atencion-y-servicios-la-ciudadania