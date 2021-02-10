import pyttsx3
import PyPDF2

book = open('C:/Projects/Learning/101.01 - Python Basics.pdf', 'rb')
pdfreader = PyPDF2.PdfFileReader(book)
pages = pdfreader.numPages
speaker = pyttsx3.init()

page = pdfreader.getPage(17)
text = page.extractText()

speaker.say('Mic testing. 1,2,3.')
speaker.say(text)
speaker.runAndWait()