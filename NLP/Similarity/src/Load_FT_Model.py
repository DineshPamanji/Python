from gensim.models import FastText

path = 'C:\\Projects\\OrgBuilder\\Codes\\DataServices\\DataServices/'

# Load model
model = FastText.load(path + 'outputs/FastText_EMSI_MODEL.model')


# Test text
text = 'leasing_sales_manager'
text = 'lending_sales_manager'
text = 'sales_leasing_manager'

# Most similar
model.wv.most_similar(text, topn=5)

standard_text = 'sales_and_leasing_manager'
standard_text in model.wv.vocab