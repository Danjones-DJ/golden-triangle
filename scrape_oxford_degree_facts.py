import requests
import lxml.html
import pandas as pd
import re
import time





def oxford_degree_facts(url):
    try:
        html = requests.get(url, timeout=15).text
        doc = lxml.html.fromstring(html)

        # 1. TITLE (always BA at Oxford)
        degree_title_results = doc.xpath('//h1/text()')
        if degree_title_results:
            clean_title = degree_title_results[0]
            degree_type = 'BA'
        else:
            clean_title = None
            degree_type = None

        a_level_grade_req = None
        a_level_subject_reqs = None
        ib_grade_req = None
        ib_subject_req = None

        # 2. TRY TABLE FORMAT FIRST
        tables = doc.xpath('//table')
        table_text_found = False
        
        for table in tables:
            table_text = table.text_content().replace('\xa0', ' ')
            if 'A-levels:' in table_text and 'International Baccalaureate' in table_text:
                table_text_found = True
                
                # FIXED: Extract A-level part ONLY (use .*? instead of [^A]*?)
                alevel_match = re.search(r'A-levels:\s*(.*?)(?=Advanced Highers|International)', table_text)
                if alevel_match:
                    alevel_text = alevel_match.group(1).strip()
                    grade_match = re.search(r'([A*ABCDE]{3,5})', alevel_text)
                    if grade_match:
                        a_level_grade_req = grade_match.group(1)
                        a_level_subject_reqs = alevel_text

                
                # FIXED: Extract IB part ONLY (use .*? instead of [^A]*?)
                ib_match = re.search(r'International Baccalaureate \(IB\):\s*(.*?)(?=Advanced diploma|Any other|$)', table_text)
                if ib_match:
                    ib_text = ib_match.group(1).strip()
                    points_match = re.search(r'(\d{2})', ib_text)
                    if points_match:
                        ib_grade_req = points_match.group(1)
                        ib_subject_req = ib_text
                break

        # 3. FALL BACK TO PARAGRAPH FORMAT (if no table found)
        if not table_text_found:
            all_paragraphs = doc.xpath('//p[@class="audience-copy"]')
            
            for p in all_paragraphs:
                text = p.text_content().strip().replace('\xa0', ' ')
                
                if 'Entrance requirements:' in text and re.search(r'[A*ABCDE]{3,5}', text):
                    req_match = re.search(r'Entrance requirements:\s*([^.]*\.)', text)
                    if req_match:
                        alevel_text = req_match.group(1).strip()
                        grade_match = re.search(r'([A*ABCDE]{3,5})', alevel_text)
                        if grade_match:
                            a_level_grade_req = grade_match.group(1)
                            a_level_subject_reqs = alevel_text
                            break

        return [degree_type, clean_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]

    except Exception as e:
        print(f"Error scraping {url}: {e}")
        return [None, None, None, None, None, None]

url = "https://www.ox.ac.uk/admissions/undergraduate/courses/course-listing/computer-science-and-philosophy"
result = oxford_degree_facts(url)
print(result)






# df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/oxf_links_discuni.csv")

# results = []
# for index, row in df2.iterrows():  # Changed: iterate over rows to get both columns
#     url = row['crseurl']
#     kis_course_id = row['kiscourseid']  # Get the kiscourseid (lowercase)
    
#     print(f"Scraping: {url}")
#     facts = oxford_degree_facts(url)
    
#     # Include both kiscourseid and URL in results
#     results.append([kis_course_id, url] + facts)
#     time.sleep(1)

# # Create dataframe with kiscourseid as first column
# columns = ['kiscourseid', 'url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
# final_df = pd.DataFrame(results, columns=columns)

# # Save
# final_df.to_csv('oxford_degree_facts.csv', index=False)
# print("Done!")



import requests
import lxml.html
import re

def oxford_degree_facts(url):
    try:
        html = requests.get(url, timeout=15).text
        doc = lxml.html.fromstring(html)
        
        # 1. TITLE (always BA at Oxford)
        degree_title_results = doc.xpath('//h1/text()')
        if degree_title_results:
            clean_title = degree_title_results[0]
            degree_type = 'BA'
        else:
            clean_title = None
            degree_type = None
            
        a_level_grade_req = None
        a_level_subject_reqs = None
        ib_grade_req = None
        ib_subject_req = None
        
        # 2. TRY TABLE FORMAT FIRST
        tables = doc.xpath('//table')
        table_text_found = False
        
        for table in tables:
            table_text = table.text_content().replace('\xa0', ' ')
            if 'A-levels:' in table_text and 'International Baccalaureate' in table_text:
                table_text_found = True
                
                # FIXED: Extract A-level part ONLY (use .*? instead of [^A]*?)
                alevel_match = re.search(r'A-levels:\s*(.*?)(?=Advanced Highers|International)', table_text)
                if alevel_match:
                    alevel_text = alevel_match.group(1).strip()
                    grade_match = re.search(r'([A*ABCDE]{3,5})', alevel_text)
                    if grade_match:
                        a_level_grade_req = grade_match.group(1)
                        a_level_subject_reqs = alevel_text
                
                # FIXED: Extract IB part ONLY (use .*? instead of [^A]*?)
                ib_match = re.search(r'International Baccalaureate \(IB\):\s*(.*?)(?=Advanced diploma|Any other|$)', table_text)
                if ib_match:
                    ib_text = ib_match.group(1).strip()
                    points_match = re.search(r'(\d{2})', ib_text)
                    if points_match:
                        ib_grade_req = points_match.group(1)
                        ib_subject_req = ib_text
                break
                
        # 3. FALL BACK TO PARAGRAPH FORMAT (if no table found)
        if not table_text_found:
            all_paragraphs = doc.xpath('//p[@class="audience-copy"]')
            
            for p in all_paragraphs:
                text = p.text_content().strip().replace('\xa0', ' ')
                
                if 'Entrance requirements:' in text and re.search(r'[A*ABCDE]{3,5}', text):
                    req_match = re.search(r'Entrance requirements:\s*([^.]*\.)', text)
                    if req_match:
                        alevel_text = req_match.group(1).strip()
                        grade_match = re.search(r'([A*ABCDE]{3,5})', alevel_text)
                        if grade_match:
                            a_level_grade_req = grade_match.group(1)
                            a_level_subject_reqs = alevel_text
                            break
        
        # NEW SECTION: Get optional degree type
        optional_degree_type = None
        page_text = doc.text_content().replace('\xa0', ' ').replace('\n', ' ')
        
        # Look for duration patterns
        duration_patterns = [
            r'Course duration:\s*(\d+)\s*years?\s*\(([^)]+)\);\s*(\d+)\s*years?\s*\(([^)]+)\)',
            r'(\d+)\s*years?\s*\(([^)]+)\)\s*or\s*(\d+)\s*years?\s*\(([^)]+)\)',
            r'studied for\s*(\d+)\s*years?\s*\(([^)]+)\)\s*or\s*(\d+)\s*years?[^(]*\(([^)]+)\)'
        ]
        
        for pattern in duration_patterns:
            match = re.search(pattern, page_text, re.IGNORECASE)
            if match:
                years1, type1, years2, type2 = match.groups()
                
                # The longer duration is the optional degree type
                if int(years1) > int(years2):
                    optional_degree_type = type1.strip()
                else:
                    optional_degree_type = type2.strip()
                break
                            
        return [degree_type, optional_degree_type, clean_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]
        
    except Exception as e:
        print(f"Error scraping {url}: {e}")
        return [None, None, None, None, None, None, None]

# Test the function
url = "https://www.ox.ac.uk/admissions/undergraduate/courses/course-listing/computer-science-and-philosophy"
result = oxford_degree_facts(url)
print("Results:", result)

df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/oxf_links_discuni.csv")

results = []
for index, row in df2.iterrows():
    url = row['crseurl']
    kis_course_id = row['kiscourseid']
    
    print(f"Scraping: {url}")
    facts = oxford_degree_facts(url)
    
    # Include both kiscourseid and URL in results
    results.append([kis_course_id, url] + facts)
    time.sleep(1)

# Updated columns to match the new 7-column return format
columns = ['kiscourseid', 'url', 'degree_type', 'optional_degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
final_df = pd.DataFrame(results, columns=columns)

# Save
final_df.to_csv('oxford_degree_facts.csv', index=False)
print("Done!")