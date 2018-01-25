## Doelstelling

Deze repository zal de samengevatte onderbouwing bevatten voor keuzes in de Meetnetten Natuurlijk Milieu (MNM).
De keuzes kunnen ook dienen voor (mogelijke) prioriteiten in onderzoek en monitoring van het natuurlijk milieu in het algemeen.
Het bevat tevens de up-to-date keuzes zelf.

In het huidige stadium blijft de keuze-oefening intern INBO-ANB / Vlaamse Overheid. Dit komt omdat de meetnetten
nog in ontwerp zijn en omdat maatschappelijk debat in dit stadium tot onnodige vertraging en commotie kan leiden.

De informatie zal op termijn in de vorm van een eenvoudige website ter beschikking worden gesteld, in eerste instantie voor interne deling.
Enkele aparte documenten gaan in meer detail in op bepaalde deelaspecten.


## Praktisch gebruik van de repository

### Indeling repository

- Onder de map `source` gebeurt het schrijfwerk. Er zijn twee subfolders:
    - `site`: bevat de setup om een kleine, bookdown-gebaseerde website te kunnen maken met R Markdown. **Belangrijk is dat dit per topic _zeer kort_ is en licht verteerbaar**. Dus ingaan op de essenties en de principes.
    - `detailed`: bevat de R Markdown documenten die meer detail geven over specifieke topics. Deze kunnen dan als pdf worden aangeroepen vanaf de website. Ook kunnen hier downloadbare tabellen als csv of andere downloadbare bestanden staan. De bestanden zijn best te organiseren in subfolders volgens topic.
- Onder de map `background` is 'hulp'-materiaal te vinden dat kan dienen als input voor concrete Rmd-bestanden.
- Wie de Rmd bestanden op zijn PC compileert naar een uitvoerformaat (html, pdf), zal deze bestanden zien verschijnen onder een map `docs`. Deze map is niet opgenomen onder versiebeheer (is uitgesloten door .gitignore). Het ontsluiten van de webpagina's gebeurt in een later stadium.


### Inhoudelijke onderdelen

_Cursieve delen_ kunnen normaal evengoed dienen voor HabNorm!

- Overzicht (doel van de site)
- Vraagstelling (uit het basisrapport)
- _Concepten en definities_ --> Maud trekt
- _De essentiële principes en uitkomsten van volgende tools:_
    - conceptueel systeemschema van de standplaats --> Cécile trekt
    - afwegingskader milieudrukken
    - afwegingskader types (i.e. habitat(sub)types en regionaal belangrijke biotopen)
- De gemaakte keuzes voor de MNM met behulp van deze tools + post-hoc correcties:
    - de milieudrukken die worden bekeken
    - de doelpopulatie per milieudruk (welke types bekijken per milieudruk)
    - primaire standplaatsfactoren en milieuvariabelen per milieudruk en per type
    - de relatie met de opdeling van meetnetten
    - de habitatgroepen binnen elke doelpopulatie
    
Voor HabNorm wordt er eveneens gewerkt aan het documenteren van de keuzes. Tevens wordt gewerkt aan het documenteren van de ecologische kennis en van de onderzoeksmethodologie. Een site, gericht op HabNorm (zie [repository](https://www.github.com/inbo/XXXXXXXXXXXXXXXXXXXXXXXX)), kan de onderstaande onderdelen bevatten. Ook hier zijn cursieve onderdelen gemeenschappelijk te zien met MNM.

- Afbakening van de vegetatie- en habitattypes
- _Beschrijving van milieuprocessen en effectrelaties_ (geeft tevens onderbouwing en uitwerking bij het conceptueel systeemschema):
    - beschrijving van standplaatsprocessen in natuurlijke omstandigheden; de rol van milieuvariabelen hierin en de relatie met de vegetatie.
    - beschrijving hoe individuele milieudrukken de milieukwaliteit van de standplaats, en de vegetatie beïnvloeden.
- Onderbouwing van gemaakte keuzes:
    - rationale van selectie van milieuvariabelen
    - typespecifieke keuzes (desgevallend alleen in de typespecifieke rapporten)

In het algemeen zijn de cursieve onderdelen nog veel breder bruikbaar, namelijk voor (prioritering in) onderzoek en monitoring van het natuurlijk milieu in het algemeen.


### Samenwerken

- Gebruik steeds een eigen branch om in te committen.
- Maak daarin naar believen commits aan.
- Mergen naar de develop-branch gebeurt steeds via een pull request (PR) op www.github.com/inbo/mnm_keuzes. In de develop-branch brengen we steeds alles samen (dus toch voldoende frequent een PR maken, zeker indien met meerderen op hetzelfde bestand wordt gewerkt).
- Bij een pull request zal floris checken of er nog technische verbeteringen nodig zijn om te mergen met de develop-branch (bv. R Markdown syntax, merge-conflicten). _Indien het ook de bedoeling is om inhoudelijk na te lezen, geef dit specifiek aan bij je pull request._





