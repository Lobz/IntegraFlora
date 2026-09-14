# TO DOs

## Escrever pedido de verba
- perfil da pessoa
- tempo
- custo
- o que é o travbalhop

## Issues

### Gazetteer has duplicated loc.correct

[23] "brazil_sao paulo_porto ferreira_pe porto ferreira (antiga reserva estadual porto ferreira)"
[24] "brazil_sao paulo_porto ferreira_parque estadual porto ferreira"

### checkInverted error if col classes are wrong

Error in -tmp[, lon] : argumento inválido para operador unário

fix: use as.numeric

## locationTables

- remove all locations that match grep(uc_string) -> done?? maybe redo later
- search database for all occurrences of "estação experimental", "floresta d", 'eec", etc

## Listas de UCs

- Ver com colegas quais UCs devemos focar e quais devemos filtrar (ex: APAs que contém zonas industriais e metrópoles)
- Baixar dados do ICMBIO https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais
- Escrever script para combinar dados do ICMBIO com os do CNUC
- Enviar emails para: ICMBio, Reservas Votorantim, Instituto Florestal, Secretaria do Estado, etc, perguntando sobre dados de UCs
- lista cncflora (perguntar Guilherme) tem no site do JBRJ
- ari de Teixeira oliveira-filho ou Danilo neves da UFMG - neotroptree (Renato tem contato)
- fazer lista preliminar de todos os possíveis nomes científicos (certos ou errados) baixando por exemplo do gbif, usar para fazer um filtro pelo bash por exemplo com grep para aplicar em bancos de dados imensos (ex: gbif sem filtros)
- fazer um arquivo com a lista de nomes que não foram identificados no plantr
- wdpa world database of protected areas wdpar no cran?

- perguntar para marisa se há reserva técnica para fazer serviço de terceiros para dados de localidades e plano de manejo

## Data cleaning

- todo: decide what to do with barcode NA

- try getLoc again after substituting \\s*,\\s* with _
- Repensar o código para tratar cada arquivo separadamente/tratar arquivos menores, usar menos memória
### Notes

- last update improved loc resolution for ~ 47k records (I had to redo all treatements so I don't have the exact number of cases where it resulted in worse resolution...)

- Nome repetido

- alguém que eu possa supervisionar
- montar uma pasta que dê pra adicionar novos arquivos facilmente
- mandar e-mail com demandas
- cruzar dados sobre ucs: data de criação, área, acesso, alojamento, infraestrutura, prox instituição de pesquisa
- montar tabela de UCs para revisão
- revisar a lista de shapes, taxonomistas
- dois produtos: ferramenta e publicação sobre padrões de resultados
- adicionar no README: critérios de confiança, critério de seleção do melhor registro
- listas de florísticas?
- conversar com thaty sobre listas de planos de manejo

## Notas da apresentação EcoEncontros 9/6/2026

Apresentação durou apenas 20min!! 40min de perguntas!

é um pacote R? (deveria ser tvz??)

iNaturalist pode ser uma boa fonte de dados para um grupo e não para outro. É bom para lepidóptera e não para amfíbios

"tem um vazio de dados fundiários de mata atlântica"

sugestão: separar dados com problemas de identificação (eg, identificação apenas a nivel de gênero ou família) para mandar para especialistas identificarem.




