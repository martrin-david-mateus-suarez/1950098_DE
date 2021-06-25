Examen Final
================

# Importar la base de datos en formato excel

``` r
library(readxl)
datos<- read_excel("C:/Users/usuario/Music/OneDrive/Documentos/Diseño.xlsx")
```

# Estandarización de variables

``` r
datost<- datos # crear una nueva base de datos o data frame
datost<- scale (datost, center =  TRUE, scale =  TRUE)
datost<- as.data.frame(datost)
```

# Normalidad Multivariante

H0= Normalidad multivariante H1= No Normalidad Multivariante Confianza=
95% Alfa= 5% = 0.05 P value &gt; Alfa: No se rechaza la H0 (Normalidad)
P value &lt; Alfa: Se rechaza la H0 (No Normalidas)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test          Statistic           p value Result
    ## 1 Mardia Skewness   55.7046974521321 0.485991122140327    YES
    ## 2 Mardia Kurtosis -0.633664894525895 0.526299539885756    YES
    ## 3             MVN               <NA>              <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test            Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk      Cilindro          0.7900  <0.001      NO    
    ## 2 Shapiro-Wilk   Velocidad(m/S)       0.8815  0.0031      NO    
    ## 3 Shapiro-Wilk    # de Personas       0.8539   8e-04      NO    
    ## 4 Shapiro-Wilk Aceleracion(0 a100)    0.9303  0.0499      NO    
    ## 5 Shapiro-Wilk       Modelo           0.9421  0.1035      YES   
    ## 6 Shapiro-Wilk      Peso(Kg)          0.9061  0.0119      NO    
    ## 
    ## $Descriptives
    ##                      n          Mean Std.Dev      Median       Min      Max
    ## Cilindro            30  6.478695e-17       1 -0.16273911 -1.383282 1.057804
    ## Velocidad(m/S)      30  2.655247e-16       1 -0.07426930 -1.188309 1.596790
    ## # de Personas       30  1.387779e-16       1 -0.03920244 -1.215276 2.312944
    ## Aceleracion(0 a100) 30 -1.753516e-16       1  0.05083965 -1.474350 2.338624
    ## Modelo              30 -5.307144e-14       1  0.35005030 -2.567036 1.516885
    ## Peso(Kg)            30 -4.587380e-18       1 -0.26183567 -2.618357 1.309178
    ##                           25th      75th       Skew   Kurtosis
    ## Cilindro            -1.0781466 1.0578042 -0.2338167 -1.5225818
    ## Velocidad(m/S)      -0.6312891 0.9005153  0.3602997 -1.2971296
    ## # de Personas       -1.2152755 1.1368706  0.2658120 -0.9461901
    ## Aceleracion(0 a100) -0.7117551 0.8134344  0.3840931 -0.6145733
    ## Modelo              -0.8167840 0.7876132 -0.5306083 -0.1588514
    ## Peso(Kg)            -0.6218597 1.0473427 -0.2750977 -0.4141914

como el P value es &lt; Alfa se rechaza la H0, por lo tanto no hay
Normalidad.

# Matriz de Correlaciones

H0= correlacion = 0 (no hay correlacion) H1= correlacion diferente de 0
(si hay correlacion)

cuando no se rechaza H0, no se aplica AFE. se rechaza H0, si para
aplicar AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                     Cilindro Velocidad(m/S) # de Personas Aceleracion(0 a100)
    ## Cilindro                1.00          -0.25          0.34               -0.38
    ## Velocidad(m/S)         -0.25           1.00          0.02                0.46
    ## # de Personas           0.34           0.02          1.00               -0.25
    ## Aceleracion(0 a100)    -0.38           0.46         -0.25                1.00
    ## Modelo                 -0.01          -0.19         -0.29                0.06
    ## Peso(Kg)                0.08          -0.25          0.13               -0.59
    ##                     Modelo Peso(Kg)
    ## Cilindro             -0.01     0.08
    ## Velocidad(m/S)       -0.19    -0.25
    ## # de Personas        -0.29     0.13
    ## Aceleracion(0 a100)   0.06    -0.59
    ## Modelo                1.00    -0.01
    ## Peso(Kg)             -0.01     1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                     Cilindro Velocidad(m/S) # de Personas Aceleracion(0 a100)
    ## Cilindro                0.00           1.00          0.79                0.52
    ## Velocidad(m/S)          0.19           0.00          1.00                0.15
    ## # de Personas           0.07           0.92          0.00                1.00
    ## Aceleracion(0 a100)     0.04           0.01          0.19                0.00
    ## Modelo                  0.94           0.33          0.12                0.76
    ## Peso(Kg)                0.66           0.18          0.48                0.00
    ##                     Modelo Peso(Kg)
    ## Cilindro              1.00     1.00
    ## Velocidad(m/S)        1.00     1.00
    ## # de Personas         1.00     1.00
    ## Aceleracion(0 a100)   1.00     0.01
    ## Modelo                0.00     1.00
    ## Peso(Kg)              0.96     0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) 
correlaciones$r
```

    ##                        Cilindro Velocidad(m/S) # de Personas
    ## Cilindro             1.00000000    -0.24694012    0.33988772
    ## Velocidad(m/S)      -0.24694012     1.00000000    0.01957758
    ## # de Personas        0.33988772     0.01957758    1.00000000
    ## Aceleracion(0 a100) -0.37659144     0.45798161   -0.24534987
    ## Modelo              -0.01473281    -0.18602012   -0.29338452
    ## Peso(Kg)             0.08265046    -0.25146147    0.13273185
    ##                     Aceleracion(0 a100)      Modelo    Peso(Kg)
    ## Cilindro                    -0.37659144 -0.01473281  0.08265046
    ## Velocidad(m/S)               0.45798161 -0.18602012 -0.25146147
    ## # de Personas               -0.24534987 -0.29338452  0.13273185
    ## Aceleracion(0 a100)          1.00000000  0.05829867 -0.58525290
    ## Modelo                       0.05829867  1.00000000 -0.01053513
    ## Peso(Kg)                    -0.58525290 -0.01053513  1.00000000

``` r
r <- as.matrix(correlaciones$r)
```

Alfa= 0,05 P value &gt; Alfa: no se rechaza H0 P value &lt; Alfa: se
rechaza H0

# Indicadores de Aplicabilidad del AFE

## Contrasre de Esfericidad de Bartlett

H0: las correlaciones teoricas entre cada par de variables es nulo. H1:
las correlaciones teoricas entre cada par de variables no es nulo.

P value &gt; Alfa: no se aplica al AFE (no se rechaza H0) P value &lt;
Alfa: no se aplica al AFE (se rechaza H0)

``` r
dim(datost)
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n= 30)
```

    ## $chisq
    ## [1] 30.57554
    ## 
    ## $p.value
    ## [1] 0.01000727
    ## 
    ## $df
    ## [1] 15

Como el P value es menor que Alfa, se rechaza la H0, por lo tanto, las
correlaciones teoricas entre cada par de variables es nulo, es decir si
es aplicable en el analisis factorialexploratorio (AFE)

## Medidad de Adecuacion muestral de KAISER, MEYER, Y OKLIN (KMO)

Estudian variable por variable si son o no aceptadas en el modelo para
hacer AFE. Se mantiene una variable en el modelo, si el KMO es igual o
mayor a 0,7. Se elimina una variable del modelo, si el KMO es menor es
menor que 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.58
    ## MSA for each item = 
    ##            Cilindro      Velocidad(m/S)       # de Personas Aceleracion(0 a100) 
    ##                0.58                0.64                0.59                0.58 
    ##              Modelo            Peso(Kg) 
    ##                0.49                0.55

# Determinacion del Numero de Factores a Extraer

``` r
fa.parallel(r, fm= "pa", n.obs= 30, ylabel="Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](Mateus_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con el metodo de ejes principales se tendria que extraeer 1.

## Metodo de Componentes Principales

``` r
fa.parallel(r, fm= "pc", n.obs= 30, ylabel= "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

![](Mateus_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

## Metodo de Maxima Verosimilitud

``` r
fa.parallel(r, fm= "ml", n.obs= 30, ylabel= "Eigenvalues")
```

![](Mateus_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con este metodo nos da que se recomienda extraer un factor.

# Metodo de extraccion de Factores

## Metodo de analisis de los componentes principales

``` r
acp<- principal(r, nfactors = 1, rotate = "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       PC1     h2   u2 com
    ## Cilindro             0.60 0.3577 0.64   1
    ## Velocidad(m/S)      -0.60 0.3647 0.64   1
    ## # de Personas        0.45 0.2022 0.80   1
    ## Aceleracion(0 a100) -0.88 0.7691 0.23   1
    ## Modelo              -0.08 0.0057 0.99   1
    ## Peso(Kg)             0.67 0.4526 0.55   1
    ## 
    ##                 PC1
    ## SS loadings    2.15
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.17 
    ## 
    ## Fit based upon off diagonal values = 0.62

PC1= Cargas Factoriales de cada variable h2= comunalidad

## Metodo de los ejes Principales o componentes principales iteradas (CPI)

``` r
cpi<- fa(r, nfactors= 1, fm= "pa", rotate = "none", n.obs = 30)
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

``` r
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       PA1     h2     u2 com
    ## Cilindro            -0.39 0.1536  0.846   1
    ## Velocidad(m/S)       0.44 0.1910  0.809   1
    ## # de Personas       -0.27 0.0734  0.927   1
    ## Aceleracion(0 a100)  1.04 1.0794 -0.079   1
    ## Modelo               0.04 0.0016  0.998   1
    ## Peso(Kg)            -0.52 0.2755  0.724   1
    ## 
    ##                 PA1
    ## SS loadings    1.77
    ## Proportion Var 0.30
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.17 with Chi Square of  30.58
    ## The degrees of freedom for the model are 9  and the objective function was  0.3 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.12 
    ## The df corrected root mean square of the residuals is  0.16 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  13.24  with prob <  0.15 
    ## The total number of observations was  30  with Likelihood Chi Square =  7.66  with prob <  0.57 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.151
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.186
    ## BIC =  -22.95
    ## Fit based upon off diagonal values = 0.81

## Metodo de Maxima Verosimilitud

``` r
mve<- fa(r, nfactors= 1, fm= "ml", rotate = "none", n.obs = 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       ML1     h2    u2 com
    ## Cilindro            -0.38 0.1425 0.858   1
    ## Velocidad(m/S)       0.46 0.2107 0.789   1
    ## # de Personas       -0.25 0.0606 0.939   1
    ## Aceleracion(0 a100)  1.00 0.9950 0.005   1
    ## Modelo               0.06 0.0034 0.997   1
    ## Peso(Kg)            -0.59 0.3438 0.656   1
    ## 
    ##                 ML1
    ## SS loadings    1.76
    ## Proportion Var 0.29
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.17 with Chi Square of  30.58
    ## The degrees of freedom for the model are 9  and the objective function was  0.3 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.12 
    ## The df corrected root mean square of the residuals is  0.16 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  13.64  with prob <  0.14 
    ## The total number of observations was  30  with Likelihood Chi Square =  7.54  with prob <  0.58 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.164
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.184
    ## BIC =  -23.07
    ## Fit based upon off diagonal values = 0.8
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   1.00
    ## Multiple R square of scores with factors          1.00
    ## Minimum correlation of possible factor scores     0.99

# Representacion Grafica de los Factores Extraidos

# Representacion Grafica DE los Facrores Extraidos

## ACP, CPI ,MVE

SOLO SE PUEDE GRAFICAR CUANDO HAY DOS FACTORES A EXTRAER.

# OBTENCION DE LOS PUNTOS FACTORIALES

## ACP

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(r, iterations= 100,graph= F)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 100 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.448521    3.287513      1.838991
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores = T)
acp1$scores
```

    ##               PC1
    ##  [1,]  0.68070805
    ##  [2,] -0.76879835
    ##  [3,] -0.14550982
    ##  [4,] -1.03052304
    ##  [5,] -1.25496220
    ##  [6,] -2.23602411
    ##  [7,]  0.14488730
    ##  [8,]  0.74832853
    ##  [9,]  1.72696478
    ## [10,]  0.92610097
    ## [11,] -0.07218109
    ## [12,] -0.75781961
    ## [13,] -1.74854259
    ## [14,] -1.26632985
    ## [15,] -0.11327973
    ## [16,]  0.86496026
    ## [17,]  0.72764082
    ## [18,] -0.47920174
    ## [19,]  0.66242547
    ## [20,] -1.74837350
    ## [21,]  0.67125880
    ## [22,]  0.92870589
    ## [23,] -0.10725297
    ## [24,]  1.65793784
    ## [25,]  0.16640641
    ## [26,]  0.78518345
    ## [27,] -0.37629856
    ## [28,]  0.43860410
    ## [29,]  0.28300727
    ## [30,]  0.69197721

## METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm= "pa", rotate = "none", n.obs = 30,scores = "regression")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

``` r
cpi1$scores
```

    ##               PA1
    ##  [1,] -0.61238464
    ##  [2,] -0.06266715
    ##  [3,]  0.94869825
    ##  [4,]  1.48027533
    ##  [5,]  0.72736918
    ##  [6,]  2.51701275
    ##  [7,]  0.04704587
    ##  [8,] -0.86907610
    ##  [9,] -1.47677025
    ## [10,] -0.57599520
    ## [11,] -0.14014504
    ## [12,]  0.80887846
    ## [13,]  1.69871022
    ## [14,]  0.76052267
    ## [15,] -0.12369428
    ## [16,] -0.65332345
    ## [17,] -0.62101682
    ## [18,] -0.08447167
    ## [19,]  0.25724745
    ## [20,]  1.66470876
    ## [21,] -1.65917617
    ## [22,]  0.21740857
    ## [23,] -0.67718729
    ## [24,] -1.38758298
    ## [25,] -0.78153728
    ## [26,]  0.10406711
    ## [27,]  0.92020426
    ## [28,]  0.13939110
    ## [29,] -0.86510862
    ## [30,] -1.70140302

``` r
puntfact_cpi<- cpi1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

``` r
mvel<- fa(datost[,2:7], nfactors = 1, fm= "ml", rotate = "none", n.obs = 30,scores = "regression")
mvel$scores
```

    ##               ML1
    ##  [1,] -0.71322960
    ##  [2,]  0.05958468
    ##  [3,]  0.80549646
    ##  [4,]  1.57188584
    ##  [5,]  0.81630661
    ##  [6,]  2.33294878
    ##  [7,]  0.04885842
    ##  [8,] -0.70910089
    ##  [9,] -1.47467128
    ## [10,] -0.71427515
    ## [11,]  0.05287801
    ## [12,]  0.81248858
    ## [13,]  1.57436135
    ## [14,]  0.81622489
    ## [15,]  0.05370364
    ## [16,] -0.71376785
    ## [17,] -0.71284880
    ## [18,]  0.05606993
    ## [19,]  0.04266907
    ## [20,]  1.57411764
    ## [21,] -1.46365196
    ## [22,]  0.04072328
    ## [23,] -0.70564893
    ## [24,] -1.47491859
    ## [25,] -0.70610897
    ## [26,]  0.04532776
    ## [27,]  0.80755501
    ## [28,]  0.04651449
    ## [29,] -0.70518392
    ## [30,] -1.46430849

``` r
puntfact_mve<- mvel$scores
puntfact_mve<- as.data.frame(puntfact_mve)
factor.scores(r, mve,method = "Trurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                               ML1
    ## Cilindro            -0.0021895828
    ## Velocidad(m/S)       0.0028925189
    ## # de Personas       -0.0013034732
    ## Aceleracion(0 a100)  0.9924105859
    ## Modelo               0.0002898431
    ## Peso(Kg)            -0.0044448770
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.9950255
