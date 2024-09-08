import delimited "C:\Users\robin\OneDrive\Desktop\EC331 code\EC331 submission\End code\Rcode\dataginifinalimports.csv", colrange(2) numericcols(8 9) clear

encode country, generate(panel_id)
xtset panel_id year
gen treatment=0
gen lpop=log(workagepop)
replace treatment=1 if year>2004 & eu_2004==1
egen mean=mean(gini), by(eu_2004 year)
tsline mean if eu_2004==1  || tsline mean if eu_2004==0, tline(2003)

*graph export "C:\Users\robin\OneDrive\Desktop\EC331 code\Graphs\Gini Imports.pdf", as(pdf) name("Graph")


xtreg gini lpop rd fdiinflows treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")
xtreg gini lpop educbasic treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")
xtreg gini lpop rd fdiinflows educbasic treatment, fe
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop("_cons")

xtdidregress( gini lpop rd fdiinflows)(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
xtdidregress(gini lpop educbasic)(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star)" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)
xtdidregress(gini lpop rd fdiinflows educbasic)(treatment), group(eu_2004) time(year)
*estout, cells("b(fmt(5) star) p(fmt(5))" se(par)) style(tex) drop(*.year "_cons") indicate(year effects= 2007.year 2000.year 2001.year)

estat ptrends
estat trendplots