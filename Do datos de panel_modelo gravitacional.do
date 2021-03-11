clear all
cls
use "C:\Users\LENOVO\Documents\Carrera Economia\EC 07-01\ECO III\Trabajo Interciclo\data modelo gravitacional.dta" 

*************************** TRABAJO CON DATOS DE PANEL************************************
*** MODELO GRAVITACIONAL APLICADO PARA LAS EXPORTACIONES DE ECUADOR CON SUS PRINCIPALES SOCIOS COMERCIALES
*** PARA EL PERIODO 2005-2019

xtset id año
xtdes //panel balanceado y corto

** Generamos el logaritmos de las variables cuantitativas **
gen ln_e_ij = ln(e_ij )
gen ln_imp_ji=ln(imp_ji)
gen ln_pib_i = ln(pib_i )
gen ln_pib_j = ln(pib_j)
gen ln_pop_i = ln(pop_i)
gen ln_pop_j = ln(pop_j)
gen ln_pgdp_j = ln(pgdp_j)
gen ln_pgdp_i = ln(pgdp_i)
gen ln_exchange_ij = ln(exchange_ij) 

gen ln_dist_ij = ln(dist_ij ) // invariante en el tiempo

 *** The relative factor endowment*** // no significativo
gen ln_dlinder_ij =ln(dlinder_ij)
** Índice de desarrollo humano **
gen ln_idh_i = ln(idh_i )  // no significativo
gen ln_idh_j = ln(idh_j ) 

*** Political Risk Services International Country Risk Guide (PRS)** // se probó este índice pero no resultó significativo
gen ln_prs_i =ln(prs_i)
gen ln_prs_j =ln(prs_j)

*** Similarity index ***
gen sim_ij= ln(1-((pib_i)/(pib_i+pib_j))^2 - ((pib_j)/(pib_i+pib_j))^2) // no significativo


            **********************************************
			*************** DESCRPTIVOS ******************
			**********************************************
table id, contents (mean e_ij mean pib_j mean idh_j )

xtsum  
/* EXPORTACIONES: La variacion entre paises es mayor que la variacion entre un pais a lo largo del tiempo
DISTANCIA / IDIOMA:No existe variaciones de un país a lo largo del tiempo pero si entre paises
PIB Local (i): Se espera que exista cambios a lo largo del tiempo
PIB Extranjero (j): Se espera que exista mas variaciones entre paises que un paises a lo largo del tiempo
IDH Extranjero (j):Existe mas variaciones entre países que un país a lo largo del tiempo
*/

			*********************************************
			*************** GRÁFICAS ********************
			*********************************************

* Generamos gráficos comparativos entre paises para las exportaciones
 
by id: egen media_e_ij = mean(e_ij)
twoway scatter e_ij id , msymbol(circle_hollow) || connected media_e_ij id , msymbol(diamond) 
/* EEUU tiene una amplio porcentaje en la canasta de exportaciones de los 20 princiaples socios comerciales
No hay mucha variaciones dentro de los paises. entre 2010 y 2019 */
*Quitando EEUU tenemos:
twoway scatter e_ij id if id > 1 , msymbol(circle_hollow) || connected media_e_ij id if id > 1, msymbol(diamond) 

* Generamos gráficos comparativos entre paises para el tipo de cambio (Nada interesante)
by id: egen media_exchange_ij = mean(exchange_ij)
twoway scatter exchange_ij id, msymbol(circle_hollow) || connected media_exchange_ij id, msymbol(diamond) 

* Realizamos gráficos de dispersión
graph twoway (scatter e_ij pib_j) (qfit e_ij pib_j)
graph twoway (scatter e_ij pib_j) (qfit e_ij pib_j) if id > 1 //Sin USA
/*A prior, existe una relacion positiva ente el PIB y las exportaciones*/
	
			
		*********************************************
		*************** REGRESIONES******************
		*********************************************
* Global (variables independientes)
global Var ln_pib_i ln_pib_j ln_exchange_ij ln_idh_j
global Fij ln_dist_ij Idioma // invariantes en el tiempo	

		****************
		** Pooled MCO **
		****************
		
regress ln_e_ij $Var $Fij 
estimate store MCO_POOLED
predict errores, residuals

*Pooled con errores estandar robustos a la correlacion a lo largo del tiempo para un indiviudo dado 
*los errores estandar robustos por cluster requieren que n sea grande y que los errores entre individuos esten incorrelacionados
regress ln_e_ij $Var $Fij, vce(robust)
*estimate store MCO_R_POOLED

		*************************
		***** Autocorrelacion ***
		*************************
*Las perturbaciones estan correlaciones entre periodos

*Calculamos las autocorrelaciones de orden 1 a 6 con toda la muestra para ver posibles estructuras de Autocorrelación
* Autocorrelaciones de orden 1 a 20 y 40
forvalues j = 1/14 {
quietly corr errores L`j'.errores
display "Autocorrelacion al reazago `j' =" r(rho)
}

forvalues s = 2006/2019 {
quietly corr errores L.errores if año == `s'
display "Autocorrelación al rezago 1 en el año `s' = "r(rho)
}

* Confirmamos/rechazamos la presencia de autocorrelación de primer orden con el test de Wooldridge
xtserial ln_e_ij $Var $Fij // , output
*No hay evidencia de correlacion de primer orden en los errores*
****************************************************************

/*Dado el test anterior sabemos que no hay autocorrelacion de 1er orden, sin embargo
puede existir otras estructuras de autocorrelaciones por lo que hacemos que el stata
nos ayude con diferentes modelos para este posible problema*/

quietly xtreg ln_e_ij $Var $Fij, pa corr(independent) // Esquema: No Asume autocorrelacion
estimates store MCGF_POOLED1
**** 1 **** //tabla resumen
quietly xtreg ln_e_ij $Var $Fij, pa corr(independent) vce(robust) // Esquema: No Asume autocorrelacion
estimates store MCGF_POOLED2
xtreg ln_e_ij $Var $Fij, pa corr(unstructured) vce(robust) nolog // Esquema: No planteada, la maquina estima los coeicientes de autocorrelacion sin estructura
estimates store MCGF_POOLED3
xtreg ln_e_ij $Var $Fij, pa corr(nonstationary 1) vce(robust) nolog //Esquema: igual que el anterior pero restringe a 1 coef, autogresivo
estimates store MCGF_POOLED4

esttab MCO_POOLED MCGF_POOLED1 MCGF_POOLED2 MCGF_POOLED3 MCGF_POOLED4, b(4) se(4) stats(N r2 p) starlevels("*" 0.1 "**" 0.05 "***" 0.01 )mtitles

**Moldelos Pooled son significativos**
*******************************************
cls
			******************************************************
			** REGRESIONES EFECTOS FIJOS Y DE EFECTOS ALEATORIOS**
			******************************************************
			
*********************************
*Comando para regresiones fijos*
*********************************
***** 2 ****** // tabla resumen
xtreg ln_e_ij $Var, fe  
estimates store FE
/* MEJOR SIGNOS ESPERADOS
TEST F = Permite compara Efectos Fijos vs Pooled
Ho = No existe efectos indivduales fijos
H1:Almenos un alpha i es diferente de cero
Desición: Mejor un modelo de efectos Fijos Within sobre el Pooled*/

xtreg ln_e_ij $Var , fe vce(robust) // (los coeficienres pierden significancia)
estimates store RFE

***************************************
**** Estimación con Varibales Dummy****
***************************************
****** 3 ****** // tabla resumen
areg ln_e_ij $Var , absorb(id) vce(bootstrap, reps(200)) // rebustos a la autocorrelacion y heterocedsaticidad
estimates store MCVD
areg ln_e_ij $Var , absorb(id) vce(robust) // rebustos a la autocorrelacion y heterocedsaticidad
areg ln_e_ij $Var , absorb(id)  // rebustos a la autocorrelacion y heterocedsaticidad

***********************************
** Estimación efectos aleatorios **
***********************************
***** 4****** // tabla resumen
xtreg ln_e_ij $Var $Fij, re theta
estimates store RE

***Test Breusch and Pagan LM test for random effects**
xttest0 // Ho: No hay efectos Aleatorios (vs Pooled)-> Rechazo

xtreg ln_e_ij $Var $Fij, re vce(robust)
estimates store RRE
xtreg ln_e_ij $Var $Fij, re vce (bootstrap , reps (200))

******************
*** Between*****
******************
xtreg ln_e_ij $Var $Fij, be
est store EA_Between 

esttab FE RFE MCVD RE RRE EA_Between, b(4) se(4) stats(N r2_o r2_w r2_b sigma_u sigma_e rho p) starlevels("*" 0.1 "**" 0.05 "***" 0.01 )mtitles

***************************************************************************
*** Test de Hausman (no robusto a heterocedasticidad o autocorrleación) ***
***************************************************************************
hausman FE RE
*Preferimos efectos fijos sobre efectos aleatorios 

*******************************
****TEST DE HAUSMAN ROBUSTO****
*******************************
xtreg ln_e_ij $Var $Fij , re theta
scalar theta = e(theta)
global yandxforhausman ln_e_ij $Var $Fij
sort id

 foreach x of varlist $yandxforhausman {
 by id: egen mean`x' = mean (`x')
 generate md`x' = `x' - mean`x'
 generate red`x' = `x' - theta*mean`x' 
 }
quietly regress redln_e_ij redln_pib_i redln_pib_j redln_exchange_ij redln_idh_j redln_dist_ij redIdioma mdln_pib_i mdln_pib_j mdln_exchange_ij  mdln_idh_j mdln_dist_ij mdIdioma
test mdln_pib_i mdln_pib_j mdln_exchange_ij  mdln_idh_j mdln_dist_ij mdIdioma

*Ho: Efectos indivduales son  Aleatorios ->>> Rechazamos Ho Preferimos Efectos fijos

*********************************************************************************
*****TEST HETEROCEDASTICIDAD POR BLOQUES Y TEST DE CORRELACION CONTEMPORANEA*****
**********************************************************************************
** Pesaran test para correlación contemporánea entre los shocks
quietly xtreg ln_e_ij $Var $Fij, fe
xtcsd, pesaran abs
*Ho: No hay correlacion contemporanea ->>  aceptamos la Ho

** Test de Wald modificado para heterocedasticidad por bloques
xttest3
*Ho: Homocedasticidad por grupos
*Rho: Hay heterocedasticidad por grupos

********************************************************
** TEST DE EFECTOS TEMPORALES (TWO-WAY FIXED EFFECTS) **
********************************************************
*usando variables dicótomas para cada año
xi: xtreg ln_e_ij $Var $Fij i.año, fe
testparm  _Iaño_2006 - _Iaño_2019  
*Ho: Variables dicotómicas temporales no son conjuntamente significativas 
*No Rechazamos la Ho.

***************************************************************************************
*REGRESIONES EFECTOS FIJOS ROBUSTAS CON DIFERENTES ESTRUCTURAS PARA LA AUTOCORRELACIÓN
***************************************************************************************
regress ln_e_ij $Var $Fij i.id, noconstant 
estimates store MCVD2 
xtreg ln_e_ij $Var $Fij i.id, noconstant pa corr(unstructured) vce(robust)
estimates store MCVD3
xtreg ln_e_ij $Var $Fij i.id, noconstant pa corr(nonstationary 1) vce(robust)
estimates store MCVD4

esttab MCVD MCVD2 MCVD3 MCVD4 , b(4) se(4) stats(N r2 p) starlevels("*" 0.1 "**" 0.05 "***" 0.01 ) mtitles
cls
**********************************************************************************
** Regresión robusta a la heterocedasticidad entre individuos y autocorrelación **
**********************************************************************************
*Panel Corrected Standar Errors (PCSE).  
xtpcse ln_e_ij $Var $Fij
estimates store PCSE1
xtpcse ln_e_ij $Var $Fij , het // robusto a la hetero por bloques
estimates store PCSE2
*** 5 *** // tabla resumen
xtpcse ln_e_ij $Var $Fij , het correlation(psar1) // rebusto a la autocorrelacion - correcion Praiss-Winter
estimates store PCSE3

esttab PCSE1 PCSE2 PCSE3, b(4) se(4) stats(N r2 p) starlevels("*" 0.1 "**" 0.05 "***" 0.01 )mtitles

cls
*****************************************
** TABLA RESUMEN MODELOS SELECCIONADOS **
*****************************************
esttab MCGF_POOLED2 FE MCVD RE PCSE3, b(4) se(4) stats(N r2 r2_o r2_w r2_b p) starlevels("*" 0.1 "**" 0.05 "***" 0.01 )mtitles

*cls
********************************
****VARIABLES INSTRUMENTALES****
********************************
* Buscamos instrumentos para la variable endogena (PIB ECUADOR)
corr ln_pib_i ln_idh_i // creemos que el índice de desarrollo humanos puede ser un instrumento válido

** EFECTOS FIJOS
quietly xtivreg ln_e_ij ln_pib_j ln_exchange_ij ln_idh_j (ln_pib_i= ln_idh_i), fe
est store FE_VI
** EFECTOS ALEATORIOS
quietly xtivreg ln_e_ij ln_pib_j ln_exchange_ij ln_idh_j ln_dist_ij Idioma (ln_pib_i= ln_idh_i), re
est store RE_VI
** PRIMERAS DIFERENCIAS
quietly xtivreg ln_e_ij ln_pib_j ln_exchange_ij ln_idh_j ln_dist_ij Idioma (ln_pib_i= ln_idh_i), fd
est store FD_VI
** BETWEEN
quietly xtivreg ln_e_ij ln_pib_j ln_exchange_ij ln_idh_j ln_dist_ij Idioma (ln_pib_i= ln_idh_i), be
est store BE_VI

esttab FE_VI RE_VI FD_VI BE_VI, b(4) se(4) stats(N r2 r2_o r2_w r2_b) starlevels("*" 0.1 "**" 0.05 "***" 0.01 )mtitles























