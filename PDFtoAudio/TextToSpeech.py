from gtts import gTTS
import os

# Read the file
# file = open("draft.txt", "r").read().replace("\n", " ")

# Choose the language
language = 'en'

# Trial text
text = "This is a test audio. Mic testing. 1, 2, 3."

# Pass the text
# speech = gTTS(text=str(file), lang=language, slow=False)
speech = gTTS(text=text, lang=language, slow=False)

# Save the converted file
speech.save("C:/Projects/Python/PDFtoAudio/Test_TTS.mp3")



