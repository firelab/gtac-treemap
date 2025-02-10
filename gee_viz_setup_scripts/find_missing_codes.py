import pdfplumber, json, random

outputJson = r'C:\Users\NicholasStorey\gtac-treemap\gee_viz_setup_scripts\forest_type_palette_lookup_UPDATED.json'
input_pdf = r"C:\Users\NicholasStorey\Downloads\forestcodes.pdf"

with pdfplumber.open(input_pdf) as pdf:
    tables = [page.extract_table() for page in pdf.pages if page.extract_table()]

newLookup = { 'code': [], 'names': [] }
for table in tables:
    del table[0]
    for row in table:
        if 'group' in row[3].lower():
            continue
        newLookup['code'].append(int(row[2]))
        newLookup['names'].append(row[3])

with open(r"C:\Users\NicholasStorey\gtac-treemap\gee_viz_setup_scripts\forest_type_palette_lookup.json", 'r') as file:
    ogLookup = json.load(file)
    in_new_not_og = sorted(set(newLookup['code']) - set(ogLookup['code']))

for newCode in in_new_not_og:
    ogLookup['code'].append(newCode)

    index = newLookup['code'].index(newCode)
    ogLookup['names'].append(newLookup['names'][index])

    ogLookup['palette'].append("#{:06x}".format(random.randint(0, 0xFFFFFF)).upper())

if len(ogLookup['palette']) != len(set(ogLookup['palette'])):
    print('there are dupes')
else:
    print('no dupes')

with open (outputJson, 'w') as file:
    json.dump(ogLookup, file)