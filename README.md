# Introdução

Este repositório contém um conjunto de scripts para analisar e simular a evolução do emprego público utilizando os dados disponíveis na página internet da [Direção-Geral da Admnistração e do Emprego Público (DGAEP)](https://www.dgaep.gov.pt/).
A DGAEP publica semestralmente o [boletim estatístico do emprego público](https://www.dgaep.gov.pt/index.cfm?OBJID=C0F56E62-5381-4271-B010-37ECE5B31017).
A página do boletim disponibiliza três ficheiros excel com quadros com vários dados.
Os scripts analisam os dados do ficheiro *Quadros Cap1 Administrações públicas*.
Este ficheiro contém quatro conjuntos de folhas:

1. Emprego no sector das administrações públicas por subsector e ministérios/secretarias regionais, segundo escalões etários e sexo;
2. Emprego no sector das administrações públicas por subsector e ministérios/secretarias regionais, segundo nível de escolaridade e sexo;
3. Emprego no sector das administrações públicas por subsector segundo o cargo / carreira / grupo, escalões etários e sexo;
4. Emprego no sector das administrações públicas por subsector, segundo o cargo / carreira / grupo, nível de escolaridade e sexo.

Os scripts neste repositório analisam o primeiro e o terceiro conjuntos de folhas.

# Instalação

1. Clonar o repositório

       cd
	   mkdir work
	   cd work
	   git clone git@github.com:plsm/DGAEP-Simulador.git

   Deve aparecer os seguintes ficheiros

   - `dados`
   - `DGAEP-Simulador.Rproj`
   - `doc`
   - `README.md`
   - `requirements.txt`
   - `src`

2. Criar um ambiente virtual python

       cd DGAEP-Simulador.git
	   mkdir .venv
       python3 -m venv .venv

3. Ativar o ambiente virtual python

       . .venv/bin/activate

4. Instalar as dependências

	   pip3 install -r ./requirements.txt

# Utilização

Para correr os scripts é necessário python3 e o R. Para ambos existem configurações de projetos. No caso do R, aconselha-se o uso do [rstudio](https://posit.co/download/rstudio-desktop/).

1. Descarregar o ficheiro Excel correspondente ao *BOEP n.º __x__ Quadros Cap1 Administrações públicas* da página [boletim estatístico do emprego público](https://www.dgaep.gov.pt/index.cfm?OBJID=C0F56E62-5381-4271-B010-37ECE5B31017) e colocar na pasta `~/work/DGAEP-Simulador.git/dados`.

2. Criar o ficheiro CSV com os dados dos postos de trabalho por cargo, carreira e grupo. No terminal executar os comandos:

       cd
       cd work/DGAEP-Simulador.git/dados
	   python3 ../src/constroi_tabela_postos_trabalho_cargo_carreira_grupo.py DGAEP-DIOEP_Quadros_CAP1-Admin_Publ_BOEP27-2024dez.xlsx

   No exemplo acima, substituir `DGAEP-DIOEP_Quadros_CAP1-Admin_Publ_BOEP27-2024dez.xlsx` pelo ficheiro excel mais recente.

3. Criar o ficheiro CSV com os dados dos postos de trabalho por ministérios e secretarias regionais.

	   python3 ../src/constroi_tabela_postos_trabalho_ministerios_secretarias_regionais.py DGAEP-DIOEP_Quadros_CAP1-Admin_Publ_BOEP27-2024dez.xlsx

   Após a criação dos ficheiros CSV devem constar os seguintes ficheiros na pasta `~/work/DGAEP-Simulador.git/dados`:

   - `DGAEP-DIOEP_Quadros_CAP1-Admin_Publ_BOEP27-2024dez.xlsx`
   - `faixas-etarias.csv`
   - `postos-trabalho-por-cargo-carreira-grupo.csv`
   - `postos-trabalho-por-ministerios-secretarias-regionais.csv`

4. No *rstudio* abrir o projeto `~/work/DGAEP-Simulador.git/DGAEP-Simulador.Rproj`

5. Criar os parâmetros do modelo de simulação nº 6. Na consola do *rstudio* executar os comandos:

       source("~/work/DGAEP-Simulador/src/calcula_parametros_modelo_simulação_6_ministerios_secretarias_regionais.R")
       calcula.parametros.modelo.simulação(0.5)
       calcula.parametros.modelo.simulação(1)

   Os parâmetros calculados assumem que se pretende obter a variação do número de postos de trabalho de seis em seis meses e de um em um ano. Se se pretender a variação a cada dois anos, deve-se passar o valor 2 à função `calcula.parametros.modelo.simulação`.

   Após a execução devem constar os seguintes ficheiros na pasta `~/work/DGAEP-Simulador.git/dados`:

   - `DGAEP-DIOEP_Quadros_CAP1-Admin_Publ_BOEP27-2024dez.xlsx`
   - `faixas-etarias.csv`
   - `'parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=0.5.csv'`
   - `'parametros-modelo-6_postos-trabalho-por-ministerios-secretarias-regionais_delta=1.csv'`
   - `postos-trabalho-por-cargo-carreira-grupo.csv`
   - `postos-trabalho-por-ministerios-secretarias-regionais.csv`


6. Executar o simulador do modelo nº 6. Abrir o ficheiro `dashboard_modelo_simulação_6_ministerios_secretarias_regionais.Rmd`. Ir ao menu *File* e escolher opção *Knit Document*, ou teclar *Ctrl+Shift+K*.

   O dashboard utiliza o ficheiro `motor_dashboard_modelo_simulação_6.R` para calcular os gráficos que são apresentados. O motor assume que se vai utilizar os parâmetros baseados numa variação do número de postos de trabalho a cada 6 meses. Para utilizar outra variação, alterar o valor da variável `delta` na linha 44.
