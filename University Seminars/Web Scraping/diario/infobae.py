from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
import warnings
import web_scraping as ws

class Web_Scraping_Infobae(ws.Web_Scraping):

    url_base="https://www.infobae.com/"

    def __init__(self):
        self.url_base="https://www.infobae.com/"
        super().__init__()

    def expandir_lista_noticias(self):
        boton_leer_mas = self.browser.find_element(By.CLASS_NAME, "feed-list-read-more")
        notas = self.browser.find_elements(By.CLASS_NAME, 'feed-list-card')

        buscar = True
        cant_notas = len(notas)

        while buscar:
            self.browser.execute_script("arguments[0].click();", boton_leer_mas)
            boton_leer_mas = self.browser.find_element(By.CLASS_NAME, "feed-list-read-more")
            notas = self.browser.find_elements(By.CLASS_NAME, 'feed-list-card')
            if cant_notas == len(notas):
                buscar = False
            else:
                cant_notas = len(notas)

    def formatear_fecha_hora_publicacion(self,fecha_publicacion):

        mes_txt_a_nro = {"Ene": 1, "Feb": 2, "Mar": 3, "Abr": 4, "May": 5, "Jun": 6, "Jul": 7, "Ago": 8, "Sep": 9,
                         "Oct": 10, "Nov": 11, "Dic": 12}

        fecha_aux = fecha_publicacion.replace("Publicado: ", "").split(",")
        dia = fecha_aux[0][0:2]

        mes_txt = fecha_aux[0][3:6]
        mes_nro = str(mes_txt_a_nro[mes_txt])
        anio = fecha_aux[1][1:5]
        fecha_formateada = anio + "-" + mes_nro + "-" + dia

        hora_txt = fecha_aux[1][6:].split(":")
        hora_txt2 = hora_txt[1].split(" ")
        hora = hora_txt[0]
        mins = hora_txt2[0]
        if hora_txt2[1] == 'p.m.': hora = str(int(hora) + 12)

        hora_formateada = hora + ":" + mins

        return fecha_formateada,hora_formateada


    def procesar_noticia_completa(self,link):
        print("link", link)
        print("Entro al link")

        options = webdriver.ChromeOptions()
        browser_aux = webdriver.Chrome(options=options)
        browser_aux.get(link)
        fecha_publicacion = browser_aux.find_element(By.CLASS_NAME, "sharebar-article-date")
        fecha_formateada,hora_formateada=self.formatear_fecha_hora_publicacion(fecha_publicacion.text)
        print("fecha_publicacion",fecha_formateada)
        print("hora_formateada", hora_formateada)

        textos = browser_aux.find_elements(By.CLASS_NAME, "paragraph")
        texto_completo = ""
        for texto in textos:
            texto_completo = texto_completo + texto.text

        seccion=browser_aux.find_element(By.CLASS_NAME, "article-section-tag").get_attribute('href').replace(self.url_base,"")
        sub_seccion=browser_aux.find_element(By.CLASS_NAME, "article-section-tag").text

        print(texto_completo)
        browser_aux.quit()
        return texto_completo,seccion,sub_seccion


    def procesar_noticia_listado(self, nota):

        col1 = nota.find_element(By.CLASS_NAME, 'col1')
        titulo = col1.find_element(By.CLASS_NAME, 'feed-list-card-headline-lean').text

        print("titulo: ", titulo)
        # Algunas notas no tienen copete
        try:
            copete = col1.find_element(By.CLASS_NAME, 'deck').text
            print("copete: ", copete)
        except:
            copete = "No informado"

        link = nota.get_attribute('href')

        return {"titulo":titulo,"copete":copete,"link":link}


    def scraping_seccion(self,seccion):

        self.browser.get(self.url_base+seccion)

        self.expandir_lista_noticias()
        listado_notas = self.browser.find_elements(By.CLASS_NAME, 'feed-list-card')

        for nota in listado_notas:
            elemento_nota=self.procesar_noticia_listado(nota)
            self.noticias.append(elemento_nota)

        for i in range(0,len(self.noticias)):
            texto_completo,seccion,sub_seccion=self.procesar_noticia_completa(self.noticias[i]["link"])
            self.noticias[i]["texto_completo"]=texto_completo
            self.noticias[i]["seccion"] = seccion
            self.noticias[i]["sub_seccion"] = sub_seccion
            print(self.noticias[i])


    #Threads!!!!!!!!!!!
    def scraping_secciones(self,secciones):
        for seccion in secciones:
            self.scraping_seccion(seccion)


    def __del__(self):
        self.browser.quit()


#browser.maximize_window()

if __name__ == '__main__':
    wsi=Web_Scraping_Infobae()
    wsi.scraping_secciones(["ultimas-noticias","sociedad"])
