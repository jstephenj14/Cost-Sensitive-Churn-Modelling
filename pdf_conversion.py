from markdown import markdown
import pdfkit
import pandas as pd

input_filename = "C:/Users/jstep/PycharmProjects/Cost-Sensitive-Churn-Modelling/README.md"
output_filename = "README.pdf"

with open(input_filename, 'r') as f:
    html_text = markdown(f.read(), output_format='html4')
    print(html_text)

pdfkit.from_string(html_text, output_filename)

### Using Grip

# grip your_markdown.md
# Worked. Refer to https://superuser.com/questions/689056/how-can-i-convert-github-flavored-markdown-to-a-pdf