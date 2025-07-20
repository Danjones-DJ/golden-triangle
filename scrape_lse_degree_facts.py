import requests
import lxml.html
import pandas as pd
import re
import time


# janitor duty 

def extract_degree_type(title):
    if not title:
        return None
    
    # Use word boundaries to match complete degree types only
    degree_pattern = r'\b(BSc|BA|BEng|MEng|MSc|MA|PhD|MPhil|LLB|LLM|MB|MBBS|MD|BDS|DDS|PharmD|MSCi|MPharm|DVM|JD)\b'
    match = re.search(degree_pattern, title, flags=re.IGNORECASE)
    
    if match:
        return match.group(1)  # Return JUST the degree type, not group(0)
    else:
        return None
    

 
# function
def lse_degree_facts(url):
    try:
        html = requests.get(url, timeout=15).text
        doc = lxml.html.fromstring(html)

        # 1. TITLE + TYPE
        degree_title_results = doc.xpath('//*[@id="main"]/div/div[1]/div[2]/div/h1/span/text()')
        
        if degree_title_results:
            full_title = degree_title_results[0]  # 'BSc Mathematics with Economics'
            degree_type = extract_degree_type(full_title)  # 'BSc'
            
            # Remove degree type from anywhere in title
            if degree_type:
                clean_title = re.sub(r'\b' + re.escape(degree_type) + r'\b', '', full_title).strip()
            else:
                clean_title = full_title
        else:
            clean_title = None
            degree_type = None

        # 2. GET ALL PARAGRAPHS FROM ENTRY REQUIREMENTS
        all_paragraphs = doc.xpath('//*[@id="entry-requirement__home"]//p')
        
        # 3. FIND A-LEVEL REQUIREMENTS (look for A-level grade patterns)
        a_level_grade_req = None
        a_level_subject_reqs = None
        
        for p in all_paragraphs:
            text = p.text_content().strip()
            
            # Look for A-level grades (A*AA, AAA, etc.) - should be P3 from debug
            if re.search(r'^[A*ABCDE]{3,4}', text):
                # Extract the grade part
                grade_match = re.search(r'^([A*ABCDE]{3,4})', text)
                a_level_grade_req = grade_match.group(1) if grade_match else None
                a_level_subject_reqs = text  # Full text
                break

        # 4. FIND IB REQUIREMENTS (look for "points overall")
        ib_grade_req = None
        ib_subject_req = None
        
        for p in all_paragraphs:
            text = p.text_content().strip()
            
            # Look for IB points - should be P10 from debug
            if 'points overall' in text:
                # Extract the points number
                points_match = re.search(r'^(\d{2})', text)
                ib_grade_req = points_match.group(1) if points_match else None
                ib_subject_req = text  # Full text
                break

        return [degree_type, clean_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]

    except Exception as e:
        print(f"Error scraping {url}: {e}")
        return [None, None, None, None, None, None]


# url = "https://www.lse.ac.uk/study-at-lse/undergraduate/bsc-mathematics-with-economics"
# # url = "https://www.lse.ac.uk/study-at-lse/undergraduate/bsc-data-science"
# result = lse_degree_facts(url)
# print(result)

# #
df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/lse_links_discuni.csv")

results = []
for index, row in df2.iterrows():  # Changed: iterate over rows to get both columns
    url = row['crseurl']
    kis_course_id = row['kiscourseid']  # Get the kiscourseid (lowercase)
    
    print(f"Scraping: {url}")
    facts = lse_degree_facts(url)
    
    # Include both kiscourseid and URL in results
    results.append([kis_course_id, url] + facts)
    time.sleep(1)

# Create dataframe with kiscourseid as first column
columns = ['kiscourseid', 'url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
final_df = pd.DataFrame(results, columns=columns)

# Save
final_df.to_csv('lse_degree_facts.csv', index=False)
print("Done!")

