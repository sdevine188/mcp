# https://2012-2017.usaid.gov/sites/default/files/documents/1863/MCPGlobal%20Appendix2012.pdf
# https://www.oecd.org/sdd/42495745.pdf

# country values range from 90 to 10
# note that the mcp formula appears to take the min/max-standardized value and multiply by 4 then add 1, 
# instead of just multiplying by 5 (for positively correlated indicators)
# this is used to bound the index from 1 to 5, instead of from 0 to 5 that'd result if the formula just multiplied by 5
# this 1 to 5 bounding is just an aesthetic/convenience choice
# 
# indicator values are positively correlated with development (e.g. gdp)
# a) 90 = 1.0 * 4 + 1 = 5; 1.0 * 5 = 5
# b) 80 = .875 * 4 + 1 = 4.5; .875 * 5 = 4.375
# c) 40 = .375 * 4 + 1 = 2.5; .375 * 5 = 1.875
# d) 10 = 0.0 * 4 + 1 = 1; 0.0 * 5 = 0
# 
# indicator values are negatively correlated with development (e.g. debt)
# a) 90 = 5 - 1.0 * 4 = 1; 5 - 1.0 * 5 = 0
# b) 80 = 5 - .875 * 4 = 1.5; 5 - .875 * 5 = .625
# c) 40 = 5 - .375 * 4 = 3.5; 5 - .375 * 5 = 3.125
# d) 10 = 5 - 0.0 * 4 = 5; 5 - 0.0 * 5 = 5