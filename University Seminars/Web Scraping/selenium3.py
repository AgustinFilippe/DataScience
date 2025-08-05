from selenium import webdriver
from selenium.webdriver.common.by import By
import time

driver = webdriver.Chrome()
driver.get("http://quotes.toscrape.com/")

q_pages=3
page=1
while page<=q_pages:

    quotes = driver.find_elements(By.CLASS_NAME, "quote")
    for quote in quotes:
        text = quote.find_element(By.CLASS_NAME, "text").text
        #Tarea: Guardar en un csv
        print(text)

    next_element=driver.find_element(By.CLASS_NAME,"next")
    link_next=next_element.find_element(By.TAG_NAME,"a")
    link_next.click()

    time.sleep(3)
    page=page+1

driver.quit()