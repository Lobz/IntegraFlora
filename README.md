# IntegraFlora

## Como usar as listas de espécie

As listas de espécies já geradas podem ser baixadas aqui: <incluir zip>
Você pode ler ou baixar listas individuais aqui:

## Resumo do workflow da ferramenta

A ferramenta IntegraFlora consiste em um pacote R e uma coleção de scripts que devem ser executados sequencialmente, assim como scripts para construção dos arquivos auxiliares de entrada. Para usar a ferramenta para gerar novas listas de espécie, o usuário precisa baixar os dados de ocorrência de espécies da área desejada (eg, do estado de São Paulo) das fontes de dados, e salvar os arquivos nas pastas especificadas no README. Os arquivos auxiliares (de descrição das UCs) estão inclusos no repositório, mas também podem ser atualizados pelo usuário.

Nos primeiros scripts, em [analyses/formatData/](analyses/formatData/), os dados de ocorrência das diferentes fontes são padronizados, garantindo que os nomes das colunas estejam no padrão DarwinCore e que colunas com os mesmos nomes estejam no mesmo padrão de formatação. Em seguida, no script [analyses/joinData.R](analyses/joinData.R), os dados são consolidados em uma única tabela e formatados com auxílio do pacote plantR. Nessa etapa, os nomes de coletores e identificadores são padronizados, assim como datas, números de coleta e códigos institucionais. Além disso, fazemos o tratamento e correção dos campos de localidade, a atribuição e checagem de coordenadas geográficas, a correção dos nomes científicos, e a atribuição de um valor de confiança da identificação, baseada na especialização do identificador.

Com os dados tratados e checados, o script [analyses/deduplicate.R](analyses/deduplicate.R) aplica o algoritmo de detecção de duplicatas, baseado em combinações de sobrenome do coletor, ano de coleta, local de coleta, família ou espécie, e número de coleta. A detecção de duplicatas reduz drasticamente o volume de dados ao remover as duplicatas virtuais (mesmo registro de herbário, baixado de bancos de dados diferentes), e ajuda a completar dados faltantes ou corrigir identificações pouco confiáveis.

A partir daqui, temos uma tabela de ocorrências, que filtramos para conter apenas ocorrências do estado de São Paulo. No script [analyses/getOccs.R](analyses/getOccs.R), essas ocorrências serão filtradas por seus campos de município, localidade e coordenadas geográficas para gerar arquivos separados para cada UC, contendo todas as ocorrências associadas àquela UC, com diferentes graus de confiança de acordo com a origem da associação (mais detalhes abaixo). Por fim, no script [analyses/treatOccs.R](analyses/treatOccs.R), são selecionadas as ocorrências com maior grau de confiança para cada táxon, e essas são organizadas em listas de espécie ordenadas por família e nome científico.


## Estrutura de diretorios e conteúdo do repositório

- [analyses/](analyses/) - scripts para tratamento dos dados
    - [formatData](analyses/formatData/) - scripts de padronização dos dados de cada fonte
- [data](data) - dados usados pela ferramenta
- [data-input](data-input) - dados de ocorrência e localidade fornecidos pelo usuário
    -[Occurrences](data-input/Occurrences/) - dados brutos de ocorrência
        - [GBIF](data-input/Occurrences/GBIF) - arquivos baixados do [GBIF](https://www.gbif.org/occurrence/search?taxon_key=6&occurrence_status=present)
        - [JABOT](data-input/Occurrences/JABOT) - arquivos baixados do [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php)
        - [Reflora](data-input/Occurrences/Reflora) - arquivos baixados do [Reflora](https://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20)
        - [splink](data-input/Occurrences/splink) - arquivos baixados do [splink](https://specieslink.net/search/)
        - [OtherSources](data-input/Occurrences/OtherSources) - outros arquivos, no padrão darwinCore.
    -[Locations](data-input/Locations) - dados sobre as UCs
        - [info](data-input/Locations/info/) - listas com nomes e locais das UCs
        - [shapes](data-input/Locations/shapes/) - arquivos .shp com mapas das UCs
- [data-tmp](data-tmp) - arquivos intermediários criados por esta ferramenta
- [plots](plots) - figuras
- [R](R) - funções usadas pelos scripts
- [results](results) - resultados, incluindo as listas de espécies
    - [checklists](results/checklists) - listas de espécies no formato do Catálogo de Plantas das UCs do Brasil
    - [total](results/total) - todos os registros encontrados em cada UC, em formato .rda
    - [total-treated](results/total-treated) - todos os registros encontrados em cada UC, em formato .csv


## Como usar esta ferramenta:

1. Antes de começar, edite o arquivo [config.R](config.R), selecionando o estado de interesse. Por padrão, a ferramenta restringe os dados a apenas um estado antes de aplicar os algoritmos mais pesados de tratamento.

2. Baixe os dados brutos atualizados das bases de dados. Podem ser baixados vários arquivos de cada plataforma, e os dados duplicados serão removidos automaticamente. Este repositório vem com alguns arquivos de exemplo. As plataformas aceitas são as seguintes:
- [GBIF](https://www.gbif.org/occurrence/search?taxon_key=6&occurrence_status=present) - arquivos .zip
- [Reflora](https://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20) - arquivos .csv
- [splink](https://specieslink.net/search/) (obs.: para baixar dados em grandes quantidades, será necessário criar uma conta) - arquivos .txt
- [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php) - arquivos .csv
- Outras fontes de dados podem ser usadas, adicionando arquivos .csv na pasta [OtherSources](data-input/Occurrences/OtherSources/). Os arquivos devem ter colunas nomeadas no padrão Darwin Core em inglês. Obs.: é possível baixar dados Jabot e Reflora neste formato usando a função [downloadIPTResource](R/downloadIPTResource.R).

Os dados devem ser salvos nas respectivas pastas dentro de [data-input/Occurrences/](data-input/Occurrences/).
No caso de mais de um arquivo serem salvos na mesma pasta, o script combinará os dados dos arquivos diferentes antes de iniciar o tratamento dos dados.
No caso dos dados Reflora, por favor abra os arquivos e salve como csv na mesma pasta antes de prosseguir.

3. Execute o script [make.R](make.R). Alternativamente, para garantir que cada etapa executada por esse script funciona corretamente, ou para customizar a execução, siga os passos:

    3.1. Execute o script [config.R](config.R)

    3.2. Execute os scripts da pasta [analyses/formatData/](analyses/formatData/).

    3.3. Execute o script [analyses/joinData.R](analyses/joinData.R). Esse script pode consumir muita memória e processamento, dependendo do número de registros. Por isso, garanta que os recursos de seu computador estejam disponíveis. Antes dessa etapa, você pode opcionalmente adicionar sinônimos de localidades no arquivo [results/locations/locGazetteer.csv](results/locations/locGazetteer.csv).

    3.4. Opcionalmente, adicione nomes alternativos de localidades na [tabela de nomes alternativos](results/locations/checkedLocations.csv).

    3.5. Execute os scripts [analyses/getOccs.R](analyses/getOccs.R) e [analyses/treatOccs.R](analyses/treatOccs.R).

4. Você pode produzir algumas estatísticas e figuras a partir dos seus resultados usando o script [analyses/resultStats.R](analyses/resultStats.R).

5. Os resultados podem ser encontrados na pasta [results/checklist](results/checklist/).

## Critério de Confiança e critério de seleção para lista

A partir dos totais de registros associados a cada UC, as listas finais são produzidas selecionando-se um registro para representar cada espécie, variedade, forma ou subspécie, além de um registro para representar cada gênero ou família não representado por registros com identificações mais precisas.
Esses representantes são selecionados de acordo com os critérios de confiança em localidade e identificação, na escala "Ouro">"Prata">"Bronze">"Latão", e em caso de empate, na presença de informação de barcode e em quão recente foi a coleta.

O critério de confiança em localização se baseia em qual foi o método de seleção utilizado, de acordo com a seguinte tabela:

| Categoria        | Fonte da seleção                                                        | Grau de confiança |
| ---------------- | ----------------------------------------------------------------------- | ----------------- |
| locality_exact   | Busca pelos nomes da UC usando expressões regulares nos campos textuais | Ouro |
| plantr_exact     | Identificador de localidade plantR idêntico ao da UC em algum município | Ouro |
| intersect_high   | Registros encontrados pelos dois métodos anteriores em UCs com mais de 98% de sua área dentro da UC alvo | Ouro |
| intersect_medium | Registros encontrados pelos dois métodos anteriores em UCs com mais de 80% de sua área dentro da UC alvo | Prata |
| coords_original  | Coordenadas originais do registro | Bronze |
| coords_gazet     | Coordenadas da localidade, obtidas do gazeteiro | Prata |
| coords_both      | Ambas as coordenadas originais e da localidade | Ouro |

O critério de confiança na identificação depende da especialização do identificador ou do coletor, usando os valores da coluna `tax_check` produzida pela função `validateTax` do pacote plantR:

| tax_check | Significado                                         | Grau de confiança |
| --------- | --------------------------------------------------- | ----------------- |
| high      | Identificador é taxonomista especialista da família | Ouro              |
| medium    | Identificador é taxonomista generalista             | Prata             |
| low       | Identificador não é nem especialista nem generalista| Bronze            |
| unkown    | Identificador não está listado                      | Latão             |


## Apoio

Esta ferramenta foi financiada pela FAPESP como parte do projeto 2024/07747-9 - "Aprimoramento e integração de bases de dados geoespaciais sobre a flora paulista", filiado ao Biota Síntese.

### Colaboradores

Mali Oz C. Salles (autor correspondente)
Renato A. F. Lima
Renata Ivanauskas
Thuane Bochorny
Guilherme S. Grittz
Pablo Pains
Andre M. Pereira
Marisa Domingos