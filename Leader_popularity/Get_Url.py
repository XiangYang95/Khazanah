# -*- coding: utf-8 -*-
"""
Created on Wed Jul 11 23:46:20 2018

@author: Lenovo
"""
from requests import get
from requests.exceptions import RequestException
from contextlib import closing
from bs4 import BeautifulSoup
import pandas as pd

def simple_get(url):
    """
    Attempts to get the content at `url` by making an HTTP GET request.
    If the content-type of response is some kind of HTML/XML, return the
    text content, otherwise return None.
    """
    try:
        with closing(get(url, stream=True)) as resp:
            if is_good_response(resp):
                return resp.content
            else:
                return None

    except RequestException as e:
        log_error('Error during requests to {0} : {1}'.format(url, str(e)))
        return None


def is_good_response(resp):
    """
    Returns True if the response seems to be HTML, False otherwise.
    """
    content_type = resp.headers['Content-Type'].lower()
    return (resp.status_code == 200 
            and content_type is not None 
            and content_type.find('html') > -1)


def log_error(e):
    """
    It is always a good idea to log errors. 
    This function just prints them, but you can
    make it do anything.
    """
    print(e)
    
xl = pd.read_csv('leader.csv')
data = pd.read_excel('leader.xlsx', sheet_name='Sheet1')
raw_html = simple_get('https://www.facebook.com/niknazminikahmad/')
html = BeautifulSoup(raw_html, 'html.parser')


listOfTab = [item['data-key'] for item in html.find_all('div', attrs={'data-key' : True})]

tab = html.find('div',attrs = {'data-key' : 'tab_community'})
nextURL = tab.a["href"]
nextURL = "https://www.facebook.com/" + nextURL

next_raw_html = simple_get(nextURL)
next_html = BeautifulSoup(next_raw_html, 'html.parser')

numberOfFollowers = next_html.select("div._3xom")[2].text


    
    
    
    
    
    
    
    