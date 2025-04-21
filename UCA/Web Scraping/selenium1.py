from selenium import webdriver
import time

# Configurar el navegador
options_chrome = webdriver.ChromeOptions()
#options.add_argument("--headless")  # Ejecutar en segundo plano (sin abrir ventana)
driver = webdriver.Chrome(options=options_chrome)

driver.get("https://quotes.toscrape.com/scroll")

time.sleep(3)

driver.quit()