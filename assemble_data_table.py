import csv
import sys

import openpyxl


filename = sys.argv [1]
workbook = openpyxl.load_workbook (filename=filename)

JOBS_TO_IGNORE = [
    'Dirigente superior:',
    'Dirigente intermédio:'
]

with open ('posto-trabalho-por-admin-cargo-genero-data.csv', 'w') as fd:
    writer = csv.writer (fd, quoting=csv.QUOTE_NONNUMERIC)
    writer.writerow ([
        'dia',
        'mês',
        'ano',
        'administração',
        'idade_idx',
        'idade_num',
        'idade',
        'género',
        'cargo',
        'postos de trabalho'
    ])
    for sheet_index in range (1, 25):
        el_sheet = workbook [f'Q.1.3.{sheet_index}']
        print (el_sheet.title)
        for admin_label_look_for, admin_label_to_write in zip (
                [
                    'ADMINISTRAÇÃO CENTRAL ',
                    'ADMINISTRAÇÃO REGIONAL DOS AÇORES',
                    'ADMINISTRAÇÃO REGIONAL DA MADEIRA',
                    'ADMINISTRAÇÃO LOCAL',
                    'FUNDOS DE SEGURANÇA SOCIAL',
                    ], [
                    'Administração Central',
                    'Administração Regional dos Açores',
                    'Administração Regional da Madeira',
                    'Administração Local',
                    'Fundos de Segurança Social'
                    ]):
            admin_row = 8
            while el_sheet.cell (row=admin_row, column=2).value != admin_label_look_for and \
                    el_sheet.cell (admin_row, column=3).value != admin_label_look_for and \
                    admin_row < el_sheet.max_row:
                admin_row += 1
            admin_row += 1
            job_categories = 1
            while el_sheet.cell (row=admin_row + job_categories, column=4).value is not None:
                job_categories += 1
            print (f'{admin_label_to_write} @{admin_row} {job_categories}#')
            for age_index, age_number in enumerate ([24, 25, 35, 45, 55, 65]):
                age_label = el_sheet.cell (row=6, column=age_index * 3 + 5).value
                for gender_index, gender_label in enumerate (['m', 'f']):
                    for job_index in range (job_categories):
                        job_label = el_sheet.cell (row=job_index + admin_row, column=4).value
                        if job_label not in JOBS_TO_IGNORE:
                            number_jobs = el_sheet.cell (row=job_index + admin_row, column=5 + age_index * 3 + gender_index).value
                            row = [
                                31 if sheet_index % 2 == 1 else 1,
                                12 if sheet_index % 2 == 1 else 6,
                                sheet_index // 2 + 2011,
                                admin_label_to_write,
                                age_index,
                                age_number,
                                age_label,
                                gender_label,
                                job_label,
                                number_jobs
                            ]
                            writer.writerow (row)
