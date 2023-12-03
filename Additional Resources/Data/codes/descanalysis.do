

cd "M:\Mi unidad\Master Program\Cursos\Elective Courses\Big data\Research Proposal\Data"


use "clean\TB_SEC_IVaVD.dta", clear

rename vpartot_lr_con vpar_lv_con_tot

mdesc v*

* Total
sum v*_12m_con [aweight=FAC_MUJ] //Coincide con informe oficial

* En pareja
fre T_INSTRUM P13_C_1 // Variables que determinan si estás o estuviste en una relación
sum vpar*_con [aweight=FAC_MUJ] if inlist(T_INSTRUM, "A1","A2","B1","B2") |  P13_C_1 != "3"
d vpar*_con, f

duplicates report ID_PER // ID-person



**# Figure 1: IPV types on percentage
set scheme tab2

preserve
	keep if inlist(T_INSTRUM, "A1","A2","B1","B2") |  P13_C_1 != "3"
	keep ID_PER FAC_MUJ vpar*_con

	rename (vpar*_12m_con) (vpar_12m_con_*)
	rename (vpar*_lv_con) (vpar_lv_con_*)

	reshape long vpar_12m_con_ vpar_lv_con_, i(ID_PER) j(vtype) string

	encode vtype, gen(viotype)
	recode viotype (5=0)
	lab define viotype 1 "Económica" 2 "Física" 3 "Psicológica" 4 "Sexual" 0 "Total", modify

	gen vpar_12m_con_perc = vpar_12m_con_*100
	gen vpar_lv_con_perc = vpar_lv_con_*100

	graph bar (mean) vpar_12m_con_perc vpar_lv_con_perc [aweight=FAC_MUJ], ///
		over(viotype, label(labsize(medium))) ///
		legend(order(1 "Últimos 12 meses" 2 "A lo largo de la relación") size(medsmall)) ///
		blabel(bar, format(%5.1f) size(medsmall))  ///
		ytitle("Porcentaje", size(medium)) ///
		ylabel(0(10)50)
		
	graph export "outputs/viotype.png", replace

restore


merge 1:1 ID_PER using "raw/bd_endireh_2021_dta/TB_SEC_IVaVD.dta", keepusing(P6_1_* P6_2_*)

labvars P6_2_1 "¿Hombres y mujeres tienen el mismo derecho a salir por las noches a divertirse?" P6_2_2 "¿Mujeres con hijos deberían trabajar aún si no tienen necesidad de hacerlo?" P6_2_3 "¿Las mujeres que se visten con escotes provocan que los hombres las molesten?" P6_2_4 "¿Mujeres casadas deben tener relaciones sexuales con su esposo cuando él quiera?", a

destring P6_2_1 P6_2_2 P6_2_3 P6_2_4, replace

lab define yesno2 1 "Sí" 2 "No"
lab values P6_2_1 P6_2_2 P6_2_3 P6_2_4 yesno2

**# Figure 2: Beliefs
set scheme cleanplots
fre P6_2_1
slideplot hbar P6_2_1, neg(2) pos(1) legend(rows(1)) title("¿Hombres y mujeres tienen el mismo derecho a salir por las noches a divertirse?", size(medium)) percent  yscale(off) blabel(bar, format(%9.0f) position(center) size(medsmall)) name(graph1, replace)

fre P6_2_2
slideplot hbar P6_2_2, neg(2) pos(1) legend(off) title("¿Mujeres con hijos deberían trabajar aún si no tienen necesidad de hacerlo?", size(medium)) percent  yscale(off) blabel(bar, format(%9.0f) position(center) size(medsmall)) name(graph2, replace)

fre P6_2_3
slideplot hbar P6_2_3, neg(2) pos(1) legend(off) title("¿Las mujeres que se visten con escotes provocan que los hombres las molesten?", size(medium)) percent  yscale(off) blabel(bar, format(%9.0f) position(center) size(medsmall)) name(graph3, replace)

fre P6_2_4
slideplot hbar P6_2_4, neg(2) pos(1) legend(off) title("¿Mujeres casadas deben tener relaciones sexuales con su esposo cuando él quiera?", size(medium)) percent  yscale(off) blabel(bar, format(%9.0f) position(center) size(medsmall)) name(graph4, replace)

grc1leg graph1 graph2 graph3 graph4, cols(1) imargin (0 0 0 0 0) ycommon xcommon legendfrom(graph1) title("", span)

graph export "outputs/beliefs2.png", replace

**# Figure 3: A correlation
fre P6_2_1 P6_2_2 P6_2_3 P6_2_4
recode P6_2_1 (1=1) (2=0), gen(P6_2_1_rec)
recode P6_2_2 (1=1) (2=0), gen(P6_2_2_rec)
recode P6_2_3 (1=0) (2=1), gen(P6_2_3_rec) //inverse
recode P6_2_4 (1=0) (2=1), gen(P6_2_4_rec) //inverse

d P6_2_*_rec, f
egen empod =  rowtotal(P6_2_*_rec), m

d vpar*_12m_con, f
foreach var of varlist vpar*_12m_con {
	
	di "`var'"
	ttest empod, by(`var')

}

cibar empod, over(vparsex_12m_con) ///
			barlabel(on) blfmt(%9.2f) ///
			graphopts( ///
			ylabel(0(1)4) ///
			legend(order(1 "No sufrió violencia" 2 "Sufrió violencia") pos(6) row(1)) ///
			title("") ytitle("") ///
			name("cibarempo", replace) ///
			)
