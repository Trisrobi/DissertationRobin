import delimited "C:\Users\robin\OneDrive\Desktop\EC331 code\EC331 submission\End code\dataginifinalexports.csv", colrange(2) numericcols(8 9) clear

encode country, generate(panel_id)
xtset panel_id year
gen treatment=0
gen lpop=log(workagepop)


eststo sumstats: quietly estpost sum gini educbasic rd fdiinflows, detail
esttab sumstats, cells("mean sd min max")
eststo clear
replace treatment=1 if year>2004 & eu_2004==1
egen mean=mean(gini), by(eu_2004 year)
tsline mean if eu_2004==1  || tsline mean if eu_2004==0, tline(2003)

xtreg gini lpop rd fdiinflows treatment , fe
*outreg2 using myfile1, replace drop(year*) addtext(Country FE, YES) tex(frag)
*shellout using `"myfile1.tex"'
xtreg gini lpop educbasic treatment, fe
*outreg2 using myfile2, replace drop(year*) addtext(Country FE, YES) tex(frag)
*shellout using `"myfile2.tex"'

*not significant with robust standard errors
xtreg gini lpop rd fdiinflows educbasic treatment, fe
*outreg2 using myfile3, replace drop(year*) addtext(Country FE, YES) tex(frag)
*shellout using `"myfile3.tex"'


xtdidregress( gini lpop rd fdiinflows)(treatment), group(eu_2004) time(year)
estat trendplots
estat ptrends
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
xtdidregress(gini lpop educbasic )(treatment), group(eu_2004) time(year)
estat trendplots
estat ptrends
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
xtdidregress(gini lpop rd fdiinflows educbasic)(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
estat trendplots
estat ptrends


*to add in appendix, shows that education and research and development expenditure are linked
xtreg rd educbasic, fe