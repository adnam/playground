#! -*- coding: utf8 -*-
"""
To run this script:
    $ pip install feedparser beautifulsoup requests

Sample output:

    $ python tt-rss-reader.py
    Promos ending next week in Singapore:
    ================================================================================
    1. Movistar Guatemala USD From 18 Jan 2015 00:00 To 18 Jan 2015 23:59 (GMT-06:00)
    <ol>
    <li>Applies for all prepaid plans</li>
    </ol>
    * no images found in description
    ________________________________________________________________________________
    2. Movistar Mexico USD From 12 Jan 2015 00:00 To 18 Jan 2015 23:59 (GMT-06:00)
    <ol>
    <li>For refills of $5 or more, customer will receive additional 50 minutes for long distance calls and 100 SMS.<br /></li>
    <li>Promotional bonus applies for USA and Canada.<br /></li>
    <li>Applies for all prepaid plans</li>
    </ol>
    * no images found in description
    ________________________________________________________________________________
    3. Tigo Bolivia USD From 08 Dec 2014 00:00 To 18 Jan 2015 23:59 (GMT-04:00)
    <ol>
    <li>In order to be part of this promotion, senders will use WhatsApp.<br /></li>
    <li>Senders in Spain must be over 18 years old, with an address in Spain and during the period of this promotion send refills to Tigo Bolivia users. <br /></li>
    <li>After the refill is done, senders must send  a picture of the purchase ticket to  684340515 via WhatsApp.<br /><br /></li>
    <li>A weekly raffle of 2 Smartphones following these dates:<br />Week 1: from Dec 8th to 14th. <br />Week 2: from Dec 15th to 21st.<br />Week 3: from Dec 22nd to 28th<br />Week 4: from Dec 29th to January 4th.<br />Week 5: from January 5th to 11st.<br />Week 6: from January 12nd to 18th<br /><br /></li>
    <li>Date on purchase ticket must be within the week of participation only.<br /></li>
    <li>Award will be granted trough "Momento ganador/ Winner moment"<br /></li>
    <li>The first customer who send a WhatsApp during the winner moment will get the Smartphone. (every Winner moment will be chosen and registered under a Notary record)<br /></li>
    <li>Every customer (mobile number) can send a maximum of 5  WhatsApp per week, otherwise the number will be blocked and will not be allowed to participate again.<br /></li>
    <li>In order to receive the price, winner will need to show original purchase ticket.<br /></li>
    <li>Every new week customers can participate again with a new purchase ticket. If customer is designated as winner, s/he can not participate again.<br /></li>
    <li>Every ticket can be used once, if duplicity is detected, the customer will be disqualified.<br /><br /><br /><img alt="" src="http://www.imagesup.net/dm-814189418042.png" style="height: 350px; width: 233px;" /></li>
    </ol>
    * downloading image http://www.imagesup.net/dm-814189418042.png
    ________________________________________________________________________________
    

TODO: error checking
TODO: unit tests

"""
import os, re
from datetime import datetime, tzinfo, timedelta
from calendar import timegm
from email.utils import parsedate_tz
import feedparser
import requests
from BeautifulSoup import BeautifulSoup
from pprint import pprint as pp

RSS_URL = "https://fm.transfer-to.com/shop/promotions2.xml"
#RSS_URL = os.path.join(os.path.dirname(__file__), "promotions2.xml")

RE_DESCRIPTION_LIST = re.compile(r"^\s*- (.*)$", re.MULTILINE|re.DOTALL)

class Timezone(tzinfo):
    """Class to model a given timezone"""
    _offset = None
    
    def __init__(self, **kwargs):
        self._offset = timedelta(**kwargs)
    
    def utcoffset(self, dt):
        return self._offset
    dst = utcoffset

def next_week_start_end(timezone):
    """Get the start and end times of next week in a given timezone"""
    next_week = datetime.now(timezone) + timedelta(days=7)
    start = (next_week - timedelta(days = next_week.weekday()))\
            .replace(hour=0, minute=0, second=0, microsecond=0)
            
    end = start + timedelta(days = 7)
    return start, end

def get_promos_ending_next_week(rss_feed, timezone):
    """For a given RSS feed and timezone, find the promotions
    which end during the following week in that timezone and 
    return a generator.
    """
    start, end = next_week_start_end(timezone)
    gmt = Timezone(minutes=0)
    for item in rss_feed['items']:
        date_to_tuple = parsedate_tz(item['dateto'])
        if not date_to_tuple: # Problem parsing RFC2822 date
            continue
        date_to_tz = Timezone(minutes=date_to_tuple[9])
        date_to = datetime.fromtimestamp(timegm(date_to_tuple[0:9]), date_to_tz)
        if date_to >= start and date_to <= end:
            yield item

def get_description_items(description):
    """A description may contain many 'items' which
    are denoted by a line starting with '- '

    TODO: we could also perhaps strip HTML line-breaks from the end
          of each item.
    """
    item = ""
    for line in description.splitlines():
        if line.startswith("- "):
            if item: yield item[2:]
            item = line
        else: item += line
    if item:
        yield item[2:]

def format_description(description):
    """For a given HTML description, re-format it as a numbered HTML list
    This assumes that the input will not contain block-level HTML structures
    """
    output = ['<ol>']
    for item in get_description_items(description):
        output.append("<li>%s</li>" % item)
    output.append('</ol>')
    return "\n".join(output)

def find_images(html_str):
    """Returns a generator of all the URLs of images in an HTML snippet"""
    bf = BeautifulSoup(html_str)
    for img in bf.findAll('img'):
        yield img["src"]

def download_image(img_url):
    """Download a file from a given URL
    
    TODO: we should regard the Content-type of the HTTP resource.
    """
    #img_url = "http://127.0.0.1/assets/img/logo.png" # XXX
    local_filename = img_url.split('/')[-1]
    r = requests.get(img_url)
    with open(local_filename, 'wb') as f:
        pass

def parse_rss(rss_url):
    rss = feedparser.parse(rss_url)
    # Singapore Time is 8 hours ahead of Greenwich Mean Time.
    timezone = Timezone(minutes = 8 * 60)
    print "Promos ending next week in Singapore:"
    print "="*80
    for idx, item in enumerate(get_promos_ending_next_week(rss, timezone)):
        print "%d. %s" % (idx+1, item["title"])
        print format_description(item["description"])
        num = None
        for num, img_url in enumerate(find_images(item["description"])):
            print "* downloading image", img_url
            download_image(img_url)
        if num is None:
            print "* no images found in description"
        print "_"*80

def main():
    parse_rss(RSS_URL)

if __name__ == "__main__":
    main()
