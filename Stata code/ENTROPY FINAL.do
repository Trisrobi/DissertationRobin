import delimited "C:\Users\robin\OneDrive\Desktop\EC331 code\EC331 submission\End code\Rcode\dataentropyforexports.csv", colrange(2) numericcols(8 9) clear

encode country, generate(panel_id)
xtset panel_id year
gen treatment=0
gen lpop=log(workagepop)

eststo sumstats: quietly estpost sum lpop entropy educbasic rd fdiinflows, detail
esttab sumstats, cells("mean sd min max") 
eststo clear

egen mean=mean(entropy), by(eu_2004 year)
replace treatment=1 if year>2004 & eu_2004==1
tsline mean if eu_2004==1  || tsline mean if eu_2004==0, tline(2003)
*graph export "C:\Users\robin\OneDrive\Desktop\EC331 code\Graphs\Graph1.pdf", as(pdf) name("Graph") replace

*run multiple models, some with rd, some with gdp some with both
xtreg entropy lpop rd fdiinflows treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")
xtreg entropy lpop  educbasic treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")
xtreg entropy lpop rd fdiinflows educbasic treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")


xtdidregress(entropy lpop rd fdiinflows)(treatment), group(eu_2004) time(year)
estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
estat trendplots
estat ptrends

xtdidregress(entropy lpop educbasic )(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
estat trendplots
estat ptrends


xtdidregress(entropy lpop rd fdiinflows educbasic)(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
estat trendplots
estat ptrends
