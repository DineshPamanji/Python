from PDFtoText import pdfparser

from gtts import gTTS
import os
import sys

def create_audiobook(file):
    data = pdfparser(file)

    # Choose the language
    language = 'en'

    # Convert to speech
    speech = gTTS(text=data, lang=language, slow=False)

    # Save the converted file
    speech.save("C:/Projects/Python/PDFtoAudio/{}.mp3".format(str(os.path.basename(file).strip('.pdf'))))


if __name__ == '__main__':
    create_audiobook(sys.argv[1])