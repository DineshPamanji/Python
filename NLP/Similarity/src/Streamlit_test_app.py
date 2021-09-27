import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px


@st.cache(ttl=60*5, max_entries=20)
def load_data():
    data = pd.read_csv("https://api.covid19india.org/csv/latest/state_wise.csv")
    return data

data = load_data()

st.markdown('<style>description{color:blue;}</style>', unsafe_allow_html=True)
st.title('Covid-19 impact in India')
st.markdown("<description>This is a test description to look at the color and text. </description>",
                        unsafe_allow_html=True)
st.sidebar.title('Select the parameters to analyze COVID-19 scenario')

st.sidebar.checkbox("Show Analysis by State", True, key=1)
select = st.sidebar.selectbox('Select a State',data['State'])
#get the state selected in the selectbox
state_data = data[data['State'] == select]
select_status = st.sidebar.radio("Covid-19 patient's status", ('Confirmed',
'Active', 'Recovered', 'Deceased'))


def get_total_dataframe(dataset):
    total_dataframe = pd.DataFrame({
    'Status':['Confirmed', 'Active', 'Recovered', 'Deaths'],
    'Number of cases':(dataset.iloc[0]['Confirmed'],
    dataset.iloc[0]['Active'], dataset.iloc[0]['Recovered'],
    dataset.iloc[0]['Deaths'])})
    return total_dataframe

state_total = get_total_dataframe(state_data)

if st.sidebar.checkbox("Show Analysis by State", True, key=2):
    st.markdown("## **State level analysis**")
    st.markdown("### Overall Confirmed, Active, Recovered and " +
    "Deceased cases in %s yet" % (select))
    if not st.checkbox('Hide Graph', False, key=1):
        state_total_graph = px.bar(
        state_total,
        x='Status',
        y='Number of cases',
        labels={'Number of cases':'Number of cases in %s' % (select)},
        color='Status')
        st.plotly_chart(state_total_graph)
