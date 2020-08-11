# Análisis Numérico

Este repositorio contiene material y trabajos realizados para la materia Análisis Numérico de la *Facultad de Ciencias Económicas de Buenos Aires*. Los estudios fueron realizados con el lenguaje R.

## 1. Interpolación

En esta ocasión se analizaron dos métodos de *interpolación* distintos: Interpolación por método de **Newton**; Interpolación con **Splines**. Los datos utilizados pertenecen a una serie financiera del Merval, donde se buscó interpolar los valores de los cierres diarios a partir de los cierres semanales (viernes). Por otro lado se corroboró la influencia del *fenómeno de Runge*.

## 2. Integración y sumación

En este estudio, se trabajo con distintos métodos de *integración* y *sumación* numérica. El objetivo fue analizar cuál de los distintos métodos performaba mejor para los distintos casos. Con respecto a integración se utilizaron: la formula **Euler-McLaurin**; la formula de **Trapecios**; la fórmula de **Simpson** y su variación **Simpson 3/8**. Por otro lado, las sumaciones se realizaron con: el método de Lubbock, la formula Woolhouse; y se sumaran valores interpolados con **Newton** y **Splines**.

## 3. Ajustamiento
En el siguiente informe se analizó  la técnica de *ajustamiento* **Whittaker**. En la misma se utilizan dos medidas de bondad del ajuste: regularidad y fidelidad. Se realizo un primer acercamiento naive seguido de la segunda etapa en donde se tuvo en cuenta el *Coeficiente de Validación Cruzada* para optimizar cierto parámetro de la formula.

## Referencias

La carpeta *utils* contiene los scripts de los distintos métodos utilizados en los estudios. Los mismos fueron facilitados por la cátedra de Julio Eduardo Fabris de Análisis Numérico de la Facultad de Ciencias Económicas de Buenos Aires.


