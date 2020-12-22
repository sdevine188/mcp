import os
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
import win32clipboard

# https://www.youtube.com/watch?v=FFDDN1C1MEQ
# download drivers from https://www.seleniumhq.org/download/
# https://selenium-python.readthedocs.io/index.html
# http://allselenium.info/python-selenium-commands-cheat-sheet-frequently-used/
#https://seleniumhq.github.io/selenium/docs/api/py/webdriver/selenium.webdriver.common.keys.html
#https://stackoverflow.com/questions/10629815/how-to-switch-to-new-window-in-selenium-for-python


# set wd
os.chdir("C:/Users/Stephen/Desktop/Python/selenium")

# start driver
driver = webdriver.Chrome("selenium_drivers/chromedriver.exe")

# set wd
os.chdir("C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/energy_and_infrastructure/data/iea")

# set page load timeout
driver.set_page_load_timeout(10)

# loop through countries
#country_list = ["ALBANIA", "BOSNIAHERZ", "KOSOVO", "NORTHMACED", "SERBIA", "UKRAINE", "MOLDOVA", "BELARUS",
#                "ARMENIA", "AZERBAIJAN", "GEORGIA"]
#country_list = ["BULGARIA", "CROATIA", "CZECH", "ESTONIA", "HUNGARY", "KAZAKHSTAN",
#                "KYRGYZSTAN", "LATVIA", "LITHUANIA", "MONTENEGRO", "POLAND",
#                "ROMANIA", "SLOVAKIA", "SLOVENIA", "TAJIKISTAN", "TURKMENIST",
#                "UZBEKISTAN"]
country_list = ["USA", "RUSSIA", "AUSTRIA", "BELGIUM", "DENMARK", "FINLAND", "FRANCE", "GERMANY", "GREECE",
                "IRELAND", "ITALY", "LUXEMBOU", "NETHLAND", "PORTUGAL", "SPAIN", "SWEDEN", "UK"]

for current_country in country_list:
        
        # print current_country
        print("current country is " + current_country)
        
        # create country_table placeholder
        country_table = pd.DataFrame({"category" : [],
                                      "Coal" : [],
                                      "Crude oil" : [],
                                      "Oil products" : [],
                                      "Natural gas" : [],
                                      "Nuclear" : [],
                                      "Hydro" : [],
                                      "Wind, solar, etc." : [],
                                      "Biofuels and waste" : [],
                                      "Electricity" : [],
                                      "Heat" : [],
                                      "Total" : [],
                                      "country" : [],
                                      "year" : []})
                        
        # loop through years
        # note that 2018 appears to be the most recent year, with 2019 getting a lot of NAs
        year_list = list(range(2009, 2020))
        for current_year in year_list:
                
                # get current_year as string
                current_year = str(current_year)
                
                # print current_year
                print("current year is " + current_year)
        
                # get current_url
                current_url = "https://www.iea.org/data-and-statistics/data-tables?country=" + current_country + \
                                "&energy=Balances&year=" + current_year
                
                # navigate to google home page
                driver.get(current_url)
                
                # sleep
                time.sleep(4)
                
                # get html_table
                html_table = driver.find_element_by_xpath("//table").get_attribute("outerHTML")
                #html_table
                
                # get current_table as a list
                current_table = pd.read_html(html_table)
                #current_table
                #type(current_table)
                #current_table[0]
                #len(current_table[0])
                
                # convert current_table to dataframe
                current_table = pd.DataFrame(current_table[0]).iloc[2:, ].rename(columns = {"Unnamed: 0" : "category"}) \
                        .assign(country = current_country, year = current_year) 
                        
                #current_table
                #current_table.dtypes
                #current_table.describe()
                #current_table.shape
                
                # get rid of unicode comma-space character
                current_table = current_table.applymap(lambda x: str(x).replace("\u202f", ""))
        
                # add current_table to country_table
                country_table = country_table.append(current_table)
        
        # get country_file_name
        country_file_name = "iea_" + current_country.lower() + ".csv"
        
        # write country_table 
        country_table.to_csv(country_file_name, index = False)








