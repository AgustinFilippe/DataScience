from selenium import webdriver
from selenium.webdriver.common.by import By
import time

driver = webdriver.Chrome()
driver.get("http://quotes.toscrape.com/")

q_pages=3

for _ in range(0,q_pages):
    next_element=driver.find_element(By.CLASS_NAME,"next")
    link_next=next_element.find_element(By.TAG_NAME,"a")
    link_next.click()
    time.sleep(3)

driver.quit()