from selenium import webdriver
from selenium.webdriver.common.by import By
import time

# Configurar el navegador
options_chrome = webdriver.ChromeOptions()
#options.add_argument("--headless")  # Ejecutar en segundo plano (sin abrir ventana)
driver = webdriver.Chrome(options=options_chrome)

driver.maximize_window()
driver.get("https://www.infobae.com/")

#Busco la primera
titulo1=driver.find_element(By.CLASS_NAME, "three-elements-chain-big-card")

link=titulo1.find_element(By.TAG_NAME,"a")

href=link.get_attribute('href')
print(href)
#<div class="three-elements-chain-item three-elements-chain-big-card"><div id="fusion-static-enter:f0ffPYbAzgrD4Av" data-fusion-component="f0ffPYbAzgrD4Av" style="display: none;"></div><a href="/politica/2025/04/13/elecciones-santa-fe-2025-en-vivo-las-ultimas-noticias-sobre-los-comicios-en-la-provincia-minuto-a-minuto/" class="story-card-ctn" data-mrf-recirculation="Bloque articulo individual" data-mrf-layout="" data-mrf-layout-anchor="" data-mrf-link="https://www.infobae.com/politica/2025/04/13/elecciones-santa-fe-2025-en-vivo-las-ultimas-noticias-sobre-los-comicios-en-la-provincia-minuto-a-minuto/" cmp-ltrk="Bloque articulo individual" cmp-ltrk-idx="0" mrfobservableid="cb41ed7f-cb73-49d0-aea1-771f38ba40aa"><div class="story-card-info"><h2 class="story-card-hl headline-link" data-mrf-layout-title="">Pullaro se impone con claridad en las elecciones en Santa Fe, y el peronismo y LLA se disputan el segundo lugar</h2><h3 class="story-card-deck">El frente oficialista Unidos para Cambiar Santa Fe es el más elegido para convencionales constituyentes. El segundo puesto lo disputan voto a voto Juan Monteverde, uno de los candidatos del peronismo, y Nicolás Mayoraz, de La Libertad Avanza. También hay PASO para cargos municipales y comunales</h3></div><div class="story-card-details-ctn"><p class="story-card-author-ctn">Por <b class="story-card-author-name">Diamela Rodriguez</b></p><span class="story-card-separator">|</span><span class="story-card-live-ctn"><div class="story-card-ring blinker-live-white"><div class="story-card-dot blinker-live-red"></div></div> EN VIVO</span></div><div class="story-card-img-ctn story-card-img-ctn-169"><picture><source srcset="https://www.infobae.com/resizer/v2/TBLGYN5CWRHSZK4O6FH56MJKSI.jpg?auth=b0bf01aefbdde32bee5e3564958b02aae419309dbc1667ff81768f84fd9d64fd&amp;smart=true&amp;width=768&amp;height=432&amp;quality=85" media="(min-width: 768px)"><source srcset="https://www.infobae.com/resizer/v2/TBLGYN5CWRHSZK4O6FH56MJKSI.jpg?auth=b0bf01aefbdde32bee5e3564958b02aae419309dbc1667ff81768f84fd9d64fd&amp;smart=true&amp;width=577&amp;height=325&amp;quality=85" media="(min-width: 520px)"><source srcset="https://www.infobae.com/resizer/v2/TBLGYN5CWRHSZK4O6FH56MJKSI.jpg?auth=b0bf01aefbdde32bee5e3564958b02aae419309dbc1667ff81768f84fd9d64fd&amp;smart=true&amp;width=420&amp;height=236&amp;quality=85" media="(min-width: 350px)"><source srcset="https://www.infobae.com/resizer/v2/TBLGYN5CWRHSZK4O6FH56MJKSI.jpg?auth=b0bf01aefbdde32bee5e3564958b02aae419309dbc1667ff81768f84fd9d64fd&amp;smart=true&amp;width=350&amp;height=197&amp;quality=85" media="(min-width: 80px)"><img alt="Elecciones en Santa Fe: votó" class="global-image story-card-img" decoding="async" fetchpriority="high" height="9" loading="eager" src="https://www.infobae.com/resizer/v2/TBLGYN5CWRHSZK4O6FH56MJKSI.jpg?auth=b0bf01aefbdde32bee5e3564958b02aae419309dbc1667ff81768f84fd9d64fd&amp;smart=true&amp;width=350&amp;height=197&amp;quality=85" width="16" data-mrf-layout-img=""></picture></div></a><div id="fusion-static-exit:f0ffPYbAzgrD4Av" data-fusion-component="f0ffPYbAzgrD4Av" style="display: none;"></div></div>

driver.quit()