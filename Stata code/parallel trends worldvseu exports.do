
import delimited "C:\Users\robin\OneDrive\Desktop\EC331 code\EC331 submission\Data\dataginiworldexports.csv", colrange(2) clear
encode country, generate(panel_id)
xtset panel_id year
gen treatment=0
replace treatment=1 if year>2004 & eu_2004==1
egen mean=mean(gini), by(eu_2014 year)
tsline mean if eu_2014==1  &year>2005 || tsline mean if eu_2014==0 &year>2005