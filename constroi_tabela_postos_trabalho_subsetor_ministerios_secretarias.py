import csv
import sys

import openpyxl


nome_ficheiro = sys.argv [1]
folha_calculo = openpyxl.load_workbook (
    filename=nome_ficheiro
)

NOMES_ADMINISTRACOES = [
    'Administração Central',
    'Administração Regional dos Açores',
    'Administração Regional da Madeira',
    'Administração Local',
    'Fundos de Segurança Social',
]
LINHA_INICIAL_ADMINISTRACOES = [
    11,
    38,
    52,
    68,
    76
]
LINHA_FINAL_ADMINISTRACOES = [
    34,
    51,
    67,
    74,
    79,
]
MINISTERIOS_SECRETARIAS_IGNORAR = [
    'Estado',
    'Serviços e Fundos Autónomos',
    'Estado e Serviços e Fundos Autónomos',
    'Órgãos do Governo Regional dos Açores',
    'Serviços e Fundos Autónomos da AR dos Açores',
    'Órgãos do Governo Regional da Madeira',
    'Serviços e Fundos Autónomos da AR da Madeira',
    'dos quais: Sector Empresarial Local - Entidades Reclassif. (ii)',
]

with open ('posto-trabalho-por-subsetor-ministerios-secretarias.csv', 'w') as fd:
    writer = csv.writer (
        fd,
        quoting=csv.QUOTE_NONNUMERIC
    )
    writer.writerow ([
        'tempo',
        'administração',
        'ministério_secretaria',
        'faixa_etária',
        'sexo',
        'postos de trabalho'
    ])
    indice_quadro = 1
    while True:
        nome_folha = f'Q.1.1.{indice_quadro}'
        if nome_folha not in folha_calculo.sheetnames:
            print (f'Folha {nome_folha} não existe na folha de cálculo {nome_ficheiro}.')
            break
        print (nome_folha)
        tempo = 2011.5 + (indice_quadro - 1) / 2
        a_folha = folha_calculo [nome_folha]
        for nome_administracao, linha_inicial_administracao, linha_final_administracao in zip (
                NOMES_ADMINISTRACOES, LINHA_INICIAL_ADMINISTRACOES, LINHA_FINAL_ADMINISTRACOES):
            for linha_administracao in range (linha_inicial_administracao, linha_final_administracao + 1):
                nome_ministerio_secretaria = a_folha.cell (row=linha_administracao, column=2).value
                if nome_ministerio_secretaria in MINISTERIOS_SECRETARIAS_IGNORAR:
                    continue
                for indice_faixa_etaria in range (6):
                    for indice_sexo, nome_sexo in zip (range (2), ['H', 'M']):
                        celula = a_folha.cell(
                            row=linha_administracao,
                            column=3 + indice_faixa_etaria * 3 + indice_sexo
                        )
                        linha = [
                            tempo,
                            nome_administracao,
                            nome_ministerio_secretaria,
                            indice_faixa_etaria,
                            nome_sexo,
                            celula.value,
                        ]
                        writer.writerow (linha)
            print (f'   {nome_administracao} @ {linha_inicial_administracao} -> {linha_administracao}')
        indice_quadro += 1
