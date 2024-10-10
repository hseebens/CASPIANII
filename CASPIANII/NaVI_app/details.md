## Was bietet die Anwendung?

NaVI liefert Information zum Vorkommen von invasiven und etablierten gebietsfremden Arten in Deutschland. Zusätzlich bietet NaVI Informationen zum potentziellen Auftauchen bisher nicht beobachteter Neobiota für Landkreise an. Die Informationen können deutschlandweit und für einzelne Landkreise dargestellt und heruntergeladen werden.

### Vorkommen der Arten

Das tatsächliche Vorkommen einer Art in NaVI basiert auf Einträgen in den online Datenbanken Global Biodiversity Information Facility (<a href="https://www.gbif.org/" target="_blank">GBIF</a>), Ocean Biodiversity Information System (<a href="https://obis.org/" target="_blank">OBIS</a>) und <a href="https://www.inaturalist.org/" target="_blank">iNaturalist</a>. Die Einträge wurden für jede Art extrahiert, bereinigt, harmonisiert und werden hier gemeinsam angeboten. Ein Eintrag besteht aus den Koordinaten des Fundorts der jeweiligen Art. Die Darstellung der Verbreitung kann für keine Art als vollständig angesehen werden. Die Einträge der Verbreitung stellen daher nur eine Mindestverbreitung dar.

Die Einträge zum tatsächlichen Vorkommen können für jede Art und für jeden Landkreis dargestellt und heruntergeladen werden. In den Menüs "Kreise" und "Wissenschaftlicher Name" kann der entsprechende Namen eingegeben werden. Die Daten für die jeweilige Auswahl können dann über das Feld "Download" als Tabelle im csv Format heruntergeladen werden.

### Potenzielle Neobiota

Potenzielle Neobiota sind gebietsfremde Arten, die ein hohes Potenzial haben, in den ausgewählten Landkreis einzuwandern und dort stabile Populationen aufzubauen. Potenzielle Neobiota weisen folgende Eigenschaften auf:

-   Kein aktuelles Vorkommen im ausgewählten Landkreis (basierend auf den vorhandenen Vorkommensdaten)
-   Vorkommen im Umkreis von 100km um den ausgewählten Landkreis
-   Vorhandensein geeigneter Habitate im ausgewählten Landkreis (s. Methodik zur Ermittlung der geeigneten Habitate)

Potenzielle Neobiota können für alle Landkreise als Tabelle im csv Format heruntergeladen werden.

## Begriffe

*Gebietsfremde Arten*: Gebietsfremde Arten sind Arten, die durch den Menschen in Gebiete gebracht wurden, in denen sie ursprünglich nicht vorkamen. Einbringung kann bewusst oder unbewusst erfolgen. Falls die eingebrachten Arten stabile Populationen aufbauen können, spricht man von etablierten Arten.

*Neobiota*: Neobiota sind gebietsfremde Arten, die nach 1492 eingeführt wurden.

*Invasive Arten*: Invasive Arten bezeichnen etablierte gebietsfremde Arten, die erhebliche negative Auswirkungen auf ihre Umgebung, die Gesellschaft oder Wirtschaft haben (Definition der Europäischen Kommission: <https://environment.ec.europa.eu/topics/nature-and-biodiversity/invasive-alien-species_en>).

*Invasive Arten von unionsweiter Bedeutung ("union concern")*: Dies stellen invasive Arten dar, die in der Europäischen Union als besonders bedenklich angesehen werden und in der Liste invasiver Arten von unionsweiter Bedeutung geführt werden (<a href="https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:32014R1143" target="_blank">Regulation (EU) 1143/2014</a>).

## Methodik

NaVI liegen umfangreiche Arbeiten zum Vorkommen und zur Vorhersage von gebietsfremden Arten zugrunde, die im Forschungsvorhaben ... durchgeführt wurden. Diese beinhalten die Erstellung einer einheitlichen Liste etablierter gebietsfremder Arten für Deutschland, die Ermittlung der Vorkommen dieser Arten und die Modellierung der potenziellen Verbreitung in Deutschland. Im Folgenden wird ein Überblick über die Methodik gegeben. Weiterführende Informationen sind im Abschlussbericht des Forschungsvorhabens (s. unten) und der Dokumentation der Methodik (s. unten) enthalten. Alle Daten und der Computercode sind frei verfügbar.

### 1. Erstellung einer Liste gebietsfremder Arten

Zur Erstellung einer umfassenden und einheitlichen Artenliste wurden 12 Listen etablierter gebietsfremder Arten für Deutschland zusammengeführt. Diese stammen aus Pulikationen des Bundesamt für Naturschutz (Nehring et al. (2013, 2015), Rabitsch und Nehring (2017)), Berichten weiterer Bundes- und Landesbehörden (Tackenberg (2017), BfG 2022, Lackschewitz et al. (2015)), wissenschaftlichen Publikationen (Capinha et al. (2017), Dyer et al. (2017), Seebens et al. (2017), van Kleunen et al. (2019), Monteiro et al. (2020), Biancolini et al. (2021), Pagad et al. (2022)) und online Datenbanken (Editorial Board of AquaNIS (2022)). Die Listen dieser Datenquellen wurden extrahiert und die taxonomischen Namen entsprechend der <a href="https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c" target="_blank">GBIF Backbone Taxonomy</a> harmonisiert und anschließend in eine Liste gebietsfremder Arten für Deutschland integriert.

### 2. Ermittlung der Vorkommen der Arten

Für jede Art in der Liste gebietsfremder Arten wurden Vorkommen aus den online Datenbanken GBIF, OBIS und iNaturalist bezogen. Die Daten wurden bereinigt, um z.B. fehlerhafte Koordinaten und unwahrscheinliche Einträge zu entfernen, die taxonomischen Namen wurden entsprechend der GBIF Backbone Taxonomy harmonisiert und die Einträge anschließend integriert. Die Einträge werden in NaVI in der Karte dargestellt.

### 3. Modellierung der potenzielle Vorkommen der Arten

Für jede Art mit einer ausreichenden Datenbasis wurde eine Modellierung der Habitateignung durchgeführt. Hierzu wurden übliche Verfahren zur Modellierung ("species distribution models") angewandt. Eine Modellierung setzt aus Mindestanzahl von Einträgen zum Vorkommen voraus, so dass nicht für alle Arten eine Modellierung durchgeführt werden konnte. Die Modellierung ergab eine Einschätzung der Güte der Habitate (Habitateignung) für jede Rasterzelle in Deutschland auf einer Skala von 0 (keine Eignung) bis 1 (sehr gute Eignung). Die modellierte Habitateignung lieferte die Basis zur Ermittlung der potenziellen Neobiota (s. oben). Eine Habitateignung von \>0.7 wurde als Hinweis für geeignete Habitate angenommen.

## Weitere Informationen

Hintergrund: Diese Arbeit entstand im Auftrag...

Link zum Abschlussbericht

Beteiligte Behörden
