import requests
import lxml.html
import pandas as pd
import re
import time


# janitor duty 

# def extract_degree_type(title):
#     if not title:
#         return None
    
#     # Capture degree type + any following characters (like "(Econ)")
#     degree_pattern = r'(BSc |BA |BEng |MEng |MSc |MA |PhD |MPhil |LLB |LLM |MB |MBBS |MD |BDS |DDS |MPharm |PharmD |DVM |JD ).*$'
#     match = re.search(degree_pattern, title, flags=re.IGNORECASE)
    
#     if match:
#         return match.group(0).strip()  # Return the entire matched portion
#     else:
#         return None

def extract_degree_type(title):
    if not title:
        return None
    
    # Use word boundaries to match complete degree types only
    degree_pattern = r'\b(BSc|BA|BEng|MEng|MSc|MA|PhD|MPhil|LLB|LLM|MB|MBBS|MD|BDS|DDS|PharmD|MSCi|MPharm|DVM|BFA|BASc|JD)\b'
    match = re.search(degree_pattern, title, flags=re.IGNORECASE)
    
    if match:
        return match.group(1)  # Return just the degree type, not everything after
    else:
        return None
    
    
# function

def ucl_degree_facts(url):
    try:
        html = requests.get(url, timeout=15).text
        doc = lxml.html.fromstring(html)

        # title + type

        degree_title_results = doc.xpath('//h1/text()')
        if degree_title_results:
            title = degree_title_results[0]  
            degree_type = extract_degree_type(title)  
            
            if degree_type:
                clean_title = title.replace(degree_type, '').strip() 
            else:
                clean_title = title
        else:
            clean_title = None
            degree_type = None
        
        
        
        # a level

        ## grades

        a_level_grade_results = doc.xpath('//*[@id="tab1-alevel"]/div/dl[1]/dd[1]/text()')
        a_level_grade_req = a_level_grade_results[0] if a_level_grade_results else None
        
        ## subjects

        a_level_subject_reqs = doc.xpath('normalize-space(string(//*[@id="tab1-alevel"]/div/dl[1]/dd[2]))')
    
        # ib

        ## grades

        ib_grade_results = doc.xpath('//*[@id="tab2-ibdiploma"]/div/dl[1]/dd[1]/text()')
        ib_grade_req = ib_grade_results[0] if ib_grade_results else None

        ## subjects

        ib_subjects_results = doc.xpath('//*[@id="tab2-ibdiploma"]/div/dl[1]/dd[2]/text()')
        ib_subject_req = ib_subjects_results[0] if ib_subjects_results else None

    # return

        return [degree_type, clean_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]

    except Exception as e:
        return [None, None, None, None, None, None]



# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/information-management-business-bsc"
# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/pharmacology-bsc"
# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/urban-planning-and-real-estate-bsc"
# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/engineering-foundation-year"
# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/human-sciences-and-evolution-msci"
# url = "https://www.ucl.ac.uk/prospective-students/undergraduate/degrees/bachelor-laws-ucl-and-bachelor-laws-hku-llb"

# result = ucl_degree_facts(url)
# print(result)

# mass links!


df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/ucl_links_discuni.csv")

results = []
for index, row in df2.iterrows():  # Changed: iterate over rows to get both columns
    url = row['crseurl']
    kis_course_id = row['kiscourseid']  # Get the kiscourseid (lowercase)
    
    print(f"Scraping: {url}")
    facts = ucl_degree_facts(url)
    
    # Include both kiscourseid and URL in results
    results.append([kis_course_id, url] + facts)
    time.sleep(1)

# Create dataframe with kiscourseid as first column
columns = ['kiscourseid', 'url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
final_df = pd.DataFrame(results, columns=columns)

# Save
final_df.to_csv('ucl_degree_facts.csv', index=False)
print("Done!")
