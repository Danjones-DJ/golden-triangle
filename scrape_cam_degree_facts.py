import requests
import lxml.html
import pandas as pd
import re
import time

def cambridge_degree_facts(url):
    try:
        html = requests.get(url, timeout=15).text
        doc = lxml.html.fromstring(html)

        # 1. TITLE (always BA at Cambridge)
        degree_title_results = doc.xpath('//h1/text()')
        if degree_title_results:
            full_title = degree_title_results[0].strip()
            clean_title = re.sub(r',\s*BA\s*\(Hons\)\s*$', '', full_title).strip()
            degree_type = 'BA'
        else:
            clean_title = None
            degree_type = None

        a_level_grade_req = None
        a_level_subject_reqs = None
        ib_grade_req = None
        ib_subject_req = None

        # 2. FIND THE ENTRY REQUIREMENTS SECTION
        requirement_selectors = [
            '//*[@id="entry-requirements"]',
            '//*[contains(@class, "field-entry-overview")]',
            '//*[contains(@class, "entry-requirements")]'
        ]
        
        requirements_text = None
        for selector in requirement_selectors:
            elements = doc.xpath(selector)
            if elements:
                requirements_text = elements[0].text_content().replace('\xa0', ' ')
                break

        if requirements_text:
            # REMOVED DEBUG PRINT - no more spam!
            
            # 3. EXTRACT A-LEVEL GRADE (basic)
            alevel_grade_match = re.search(r'A level:\s*([A*ABCDE]{3,4})', requirements_text)
            if alevel_grade_match:
                a_level_grade_req = alevel_grade_match.group(1)
            
            # 4. EXTRACT A-LEVEL SUBJECT REQUIREMENTS (handle multiple patterns)
            subject_requirements = None
            
            # Pattern 1: "you will need" (like Modern Languages)
            required_match = re.search(r'you will need[^:]*:\s*(.*?)(?=We also recommend|College entry|$)', requirements_text, re.DOTALL)
            if required_match:
                required_text = required_match.group(1)
                clean_required = re.sub(r'\s+', ' ', required_text).strip()
                clean_required = re.sub(r'[\t\n•]', '', clean_required).strip()
                subject_requirements = clean_required
            
            # Pattern 2: "We don't ask for any specific subjects" (like Land Economy)
            elif re.search(r"We don't ask for any specific subjects", requirements_text):
                subject_requirements = "No specific subjects required"
            
            # Pattern 3: Look for other subject requirement patterns
            elif re.search(r'specific subjects?.*?(?:required|needed)', requirements_text, re.IGNORECASE):
                subject_match = re.search(r'(specific subjects?.*?)(?=College entry|$)', requirements_text, re.DOTALL | re.IGNORECASE)
                if subject_match:
                    subject_text = subject_match.group(1)
                    clean_subject = re.sub(r'\s+', ' ', subject_text).strip()
                    subject_requirements = clean_subject[:100] + "..." if len(clean_subject) > 100 else clean_subject

            # Format A-level requirements
            if subject_requirements:
                a_level_subject_reqs = f"{a_level_grade_req} - {subject_requirements}"
            else:
                a_level_subject_reqs = a_level_grade_req

            # 5. EXTRACT IB (same logic)
            ib_grade_match = re.search(r'IB:\s*(\d{2})', requirements_text)
            if ib_grade_match:
                ib_grade_req = ib_grade_match.group(1)
                
                if subject_requirements:
                    ib_subject_req = f"{ib_grade_req} points - {subject_requirements}"
                else:
                    ib_subject_req = f"{ib_grade_req} points"

        return [degree_type, clean_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]

    except Exception as e:
        print(f"Error scraping {url}: {e}")
        return [None, None, None, None, None, None]
# # Test the scraper
# urls = [
#     "https://www.undergraduate.study.cam.ac.uk/courses/modern-medieval-languages-ba-hons",
#     "https://www.undergraduate.study.cam.ac.uk/courses/land-economy-ba-hons"
# ]

# for url in urls:
#     print(f"\n=== Testing {url.split('/')[-1]} ===")
#     result = cambridge_degree_facts(url)
#     print(f"Result: {result}")

# Mass scraping (uncomment when ready)
df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/cam_links_discuni.csv")

results = []
for index, row in df2.iterrows():  # Changed: iterate over rows to get both columns
    url = row['crseurl']
    kis_course_id = row['kiscourseid']  # Get the KISCOURSEID
    
    print(f"Scraping: {url}")
    facts = cambridge_degree_facts(url)
    
    # Include both KISCOURSEID and URL in results
    results.append([kis_course_id, url] + facts)
    time.sleep(1)

# Create dataframe with KISCOURSEID as first column
columns = ['kiscourseid', 'url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
final_df = pd.DataFrame(results, columns=columns)

# Save
final_df.to_csv('cambridge_degree_facts.csv', index=False)
print("Done!")