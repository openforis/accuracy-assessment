import os, collections, csv

def getCeoFile(ceo_project_path, id):
  ceo_file = None
  for file in os.listdir(ceo_project_path):
    if file.endswith('_ceo.csv'):
      ceo_file = os.path.join(ceo_project_path, file)
  return ceo_file

def merge(id):

  home_path = os.path.expanduser('~')
  ceo_project_path = os.path.join(home_path, 'ceo_files', str(id))
  ceo_file = getCeoFile(ceo_project_path, str(id))
  area_rast_file = os.path.join(ceo_project_path, 'area_rast.csv')
  export_ceo_file = os.path.join(ceo_project_path, 'ceo_export.csv')
  collected_data_file = os.path.join(ceo_project_path, 'collectedData.csv')

  if (os.path.isfile(ceo_file) and os.path.isfile(area_rast_file) and os.path.isfile(export_ceo_file)):

    classes = collections.OrderedDict()
    with open(area_rast_file, 'r') as f:
      csv_reader = csv.reader(f, delimiter=',')
      headers = next(csv_reader) 
      for row in csv_reader:
        classes[row[2]] = row[0]
    #print(classes)

    plots = collections.OrderedDict()
    with open(export_ceo_file, 'r') as f:
      csv_reader = csv.reader(f, delimiter=',')
      headers = next(csv_reader)
      for i, row in enumerate(csv_reader):
        value = row[4]
        plots[i] = classes.get(value, '')
    #print(plots)

    with open(ceo_file,'r') as csvinput:
      with open(collected_data_file, 'w') as csvoutput:

        writer = csv.writer(csvoutput, lineterminator='\n')
        reader = csv.reader(csvinput)

        all = []
        row = next(reader)
        row.append('ref_code')
        all.append(row)

        for i, row in enumerate(reader):
          value = plots.get(i, '')
          row.append(value)
          all.append(row)

        writer.writerows(all)

    return collected_data_file
