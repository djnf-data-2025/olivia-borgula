---
title: The Trump administration's trade war and the promotional products industry
output: 
  html_document:
    theme: readable

---
## Diving into data about tariff effects on a top promotional product: T-shirts
<br>

#### June 7, 2025
Olivia Borgula
oborgula@terpmail.umd.edu | 616-970-3620

T-shirts are consistently a top promotional product. U.S. President Donald Trump has levied heavy tariffs since taking office in January on top exporters of cotton T-shirts to the U.S. including China, Vietnam and India, resulting in steeper prices and a blow to industry.  

The Trump administration’s trade war is relevant in the current political climate and has a direct tie to ASI’s coverage because it targets items and generally has led to cost increases. 

ASI has covered tariffs extensively, but the bulk of coverage is breaking news stories or research about stakeholder opinions on tariffs and their general implication for the industry. From what I have seen, there has not been a data-driven story with integrated graphics like the one I’m proposing that analyzes how a promotional product will be affected, like T-shirts.

For this analysis, I used category 340 items with the descriptor "Doz M&B COTTON SHIRTS, NOT KNIT" as specified with <a href="https://www.trade.gov/data-visualization/us-textile-and-apparel-imports-country" target="_blank">data</a> from the International Trade Association. This does not include other cotton aparrel or mixed fabric blend T-shirts that includle cotton. I decided on this item based on research indicating that cotton T-shirts <a href="https://chuxprint.com/blogs/chux-blog/understanding-t-shirt-fabric-options-which-material-is-right-for-you" target="_blank">tend<a/> to be a popular promo item compared to other fabrics. 

<div class="flourish-embed flourish-map" data-src="visualisation/23601140"><script src="https://public.flourish.studio/resources/embed.js"></script><noscript><img src="https://public.flourish.studio/visualisation/23601140/thumbnail" width="100%" alt="map visualization" /></noscript></div>

### 2024 data

<iframe src='https://flo.uri.sh/story/3154895/embed' title='Interactive or visual content' class='flourish-embed-iframe' frameborder='0' scrolling='no' style='width:100%;height:600px;' sandbox='allow-same-origin allow-forms allow-scripts allow-downloads allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation'></iframe><div style='width:100%!;margin-top:4px!important;text-align:right!important;'><a class='flourish-credit' href='https://public.flourish.studio/story/3154895/?utm_source=embed&utm_campaign=story/3154895' target='_top' style='text-decoration:none!important'><img alt='Made with Flourish' src='https://public.flourish.studio/resources/made_with_flourish.svg' style='width:105px!important;height:16px!important;border:none!important;margin:0!important;'> </a></div>

This is the most recent data from the International Trade Administration, but how does it compare to past years? How has it shifted since Trump took office amid heavy tariffs imposed on apparel?

<style>
.flourish-container {
  display: flex;
  gap: 20px;
  justify-content: center;
  flex-wrap: wrap;
}
.flourish-chart-wrapper {
  flex: 1 1 45%;
  min-width: 300px;
}
</style>

<div class="flourish-container">
  <div class="flourish-chart-wrapper">
    <div class="flourish-embed" data-src="story/3154895"><script src="https://public.flourish.studio/resources/embed.js"></script><noscript><img src="https://public.flourish.studio/story/3154895/thumbnail" width="100%" alt="visualization" /></noscript></div>
    </div>
  </div>

  <div class="flourish-chart-wrapper">
      <div class="flourish-embed" data-src="story/3154895"><script src="https://public.flourish.studio/resources/embed.js"></script><noscript><img src="https://public.flourish.studio/story/3154895/thumbnail" width="100%" alt="visualization" /></noscript></div>
    </div>
  </div>
</div>


### I: Tariffs 

<iframe src='https://flo.uri.sh/story/3153791/embed' title='Interactive or visual content' class='flourish-embed-iframe' frameborder='0' scrolling='no' style='width:100%;height:900px;' sandbox='allow-same-origin allow-forms allow-scripts allow-downloads allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation'></iframe><div style='width:70%!;margin-top:4px!important;text-align:right!important;'><a class='flourish-credit' href='https://public.flourish.studio/story/3153791/?utm_source=embed&utm_campaign=story/3153791' target='_top' style='text-decoration:none!important'><img alt='Made with Flourish' src='https://public.flourish.studio/resources/made_with_flourish.svg' style='width:105px!important;height:16px!important;border:none!important;margin:0!important;'> </a></div>

<b>Data, visuals and graphics:</b> I will analyze data from the International Trade Association — which tracks U.S. textile and apparel imports by country — to determine top countries that import materials of interest based on what I’ve researched about promotional T-shirts: cotton T-shirts and polyester filament fabrics. I’m interested in data quantifying the number of shirts imported from other countries, rather than imports on the raw materials, because promotional T-shirts tend to be shipped from other countries rather than made domestically. 

I’ll also find how much money the U.S. spent on these imports from the top countries and how much cotton and polyester T-shirt imports have changed over time by using another data tool from the International Trade Association. 

For data on T-shirts as a promotional product, I’ll use ASI’s market research to explore what the total promotional T-shirt sales were in 2024 — the most recent data — and whether that’s changed over the last five to 10 years by examining past state of the industries. 

For visuals, I plan on creating a map in Flourish showing the flow of cotton T-shirt imports from top countries outside the U.S. with the weight of the line representing the total monetary value of the transaction. I also want to create additional Flourish graphics like bar charts, line charts and potentially an alluvial diagram to compare imports across countries and over time. I prefer Flourish over ggplot or Datawrapper because it’s the site I’m most familiar with and I want interactive graphics on the final markdown presentation. 

<b>Other information I’ll need:</b> I want to do additional data analysis for secondary imported materials in promotional T-shirts, like polyester shirts and textiles, and will do additional research on how tariffs will affect the cost of these goods. 

<b>Estimated delivery:</b> mid to late June. 

### Upload data and load libraries  

``` r
#load in libraries 
library(tidyverse)
library(lubridate)
library(ellmer)
library(httr)
library(jsonlite)
library(rvest)
library(janitor)
library(DT)

# turn off scientific notation
options(scipen = 999)  
```


``` r
# import data for monthly cotton T-shirt imports for all countries between 2020 and 2025
all_countries <- read_csv("../data/all_countries_cotton.csv")%>%clean_names()
```


### I: T-shirts as a promotional product 

<b>What percentage of promotional product sales were from T-shirts in 2024?</b><br>
In this chunk, I used Google's Gemini API fed it a screenshot from an ASI report with a pie chart and bar chart showing findings from their research about promotional products. It identified the top promotional products and created a CSV based on the data. I commented out this part because running it uses up all my API tokens. 


``` r
# get data based on ASI's past reports 
#gemini_chat <- chat_google_gemini(model="gemini-2.5-flash-preview-04-17")

#asi_report <- google_upload("../data/x2024_asi_report.png")

#asi_csv <- gemini_chat$chat("Create a CSV where one row is one product in the pie chart, and columns are the values corresponding to each product", asi_report)
```

T-shirts have been the most popular promotional product category for years. More than 17% of all promotional product sales in 2024 were in the T-shirt category, according to ASI's research, amounting to over $4.5 billion in sales. 

<iframe src='https://flo.uri.sh/visualisation/23598698/embed' title='Interactive or visual content' class='flourish-embed-iframe' frameborder='0' scrolling='no' style='width:100%;height:600px;' sandbox='allow-same-origin allow-forms allow-scripts allow-downloads allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation'></iframe><div style='width:100%!;margin-top:4px!important;text-align:right!important;'><a class='flourish-credit' href='https://public.flourish.studio/visualisation/23598698/?utm_source=embed&utm_campaign=visualisation/23598698' target='_top' style='text-decoration:none!important'><img alt='Made with Flourish' src='https://public.flourish.studio/resources/made_with_flourish.svg' style='width:105px!important;height:16px!important;border:none!important;margin:0!important;'> </a></div>

Promotional T-shirts <a href= "https://printify.com/blog/t-shirt-fabric-guide/" target="_blank"> tend </a>to be made of cotton, followed by polyester. 

Here's where cotton T-shirts come from. 

## II: Top Cotton T-shirt Exporters to the US

<b>Which countries exported the most cotton T-shirts to the U.S. in 2024?</b>
In 2024, Bangladesh, India and Vietnam accounted for more than 60% of the total cotton T-shirt imports to the U.S. Bangladesh made up close to 40% out of the 112 countries that export this specific product to the U.S.  

``` r
#find quantity & value of exports to US for all countries with at least one export in 2024
quantity_x2024 <- all_countries %>%
  filter(year == 2024) %>%
  group_by(country) %>%
  summarize(
    total_quantity = sum(quantity, na.rm = TRUE),
    total_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_of_total_quantity = total_quantity / sum(total_quantity) * 100
  ) %>% # find how much each country contributes to the total exports as a percent of the total exports to the US 
  arrange(desc(total_quantity))

datatable(quantity_x2024, options = list(pageLength = 10)) 
```

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-12a795a4527603ca3f1b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-12a795a4527603ca3f1b">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112"],["Bangladesh","India","Vietnam","China","Indonesia","Sri Lanka","Nicaragua","Honduras","Madagascar","Cambodia","Mexico","Kenya","Ethiopia","Turkey","Thailand","Italy","Pakistan","Egypt","Jordan","Burma (Myanmar)","Mauritius","El Salvador","Portugal","Haiti","Morocco","Dominican Republic","Peru","France","Tunisia","Romania","Nepal","Philippines","Guatemala","South Korea","Japan","Colombia","Hong Kong","Sweden","United Kingdom","Poland","Canada","Albania","Laos","Liberia","Netherlands","Estonia","Bulgaria","North Macedonia","Brunei","Taiwan","Lithuania","Spain","Fiji","Switzerland","Chile","Ecuador","Kuwait","Australia","Germany","Panama","Hungary","Brazil","Slovakia","Zimbabwe","Nigeria","Ghana","Senegal","Uzbekistan","Serbia","United Arab Emirates","Czech Republic","Belgium","Greece","South Africa","Croatia","Singapore","Tanzania","Afghanistan","Maldive Islands","Latvia","Saudi Arabia","Burundi","Armenia","Denmark","New Zealand","Malaysia","Moldova","Israel","Ireland","Austria","Togo","Slovenia","Ukraine","Mali","Yemen","Cayman Islands","Macau","Eritrea","Federated States of Mic","Finland","Gabon","Mauritania","Sierra Leone","Argentina","Bahamas","French Indian Ocean Areas","Georgia","Malta","Norway","Paraguay","Rwanda","Venezuela"],[7578132,3219412,2488060,1780172,1247334,631732,309431,264559,248424,218418,212434,195024,170999,164181,139891,114668,114424,78820,70856,69849,64520,35302,34124,32188,30413,23793,23312,22734,19296,14813,13604,10688,9798,8192,5549,5482,5328,4741,4396,3172,2738,2258,2109,2104,2035,1889,1838,1581,1564,1414,1267,1242,1179,1155,1118,717,607,590,580,450,398,366,262,255,224,154,145,132,131,127,121,103,83,78,66,59,57,54,42,39,39,36,30,30,30,27,27,22,20,18,12,11,11,8,6,4,4,3,3,3,3,3,2,1,1,1,1,1,1,1,1,1],[490897450,249071571,278528117,118261104,131551017,72025436,51382499,49604994,29133699,20710505,42775246,13662645,12515186,30623930,25443526,48221883,6554373,7499473,7393013,9759057,16223126,2531509,12194916,2459225,5518931,3382516,2759859,4995060,5302843,6663796,990420,2173363,1013841,1989534,3317819,863788,1366556,586932,2428821,1879157,1905796,1096946,348595,277421,48710,1048150,716606,645619,230924,157783,575738,450630,91433,832641,624500,76486,19650,107233,163912,67913,234344,61995,74265,26351,4301,13928,11149,25088,74671,24990,39337,65189,14957,32383,5114,10123,2198,7654,1823,27414,4351,3528,2608,12791,8711,10341,10879,4851,2968,17715,935,4739,5362,500,320,750,453,338,2544,7685,1344,332,667,414,979,907,251,353,533,639,312,333],[38.42874505253739,16.32565425979377,12.61696463131233,9.027261063500291,6.325236916084553,3.203516113143654,1.569126139575254,1.341580004459442,1.259759339231825,1.107598764033816,1.077253870279737,0.9889676737124731,0.8671367792536262,0.8325626673526721,0.709387956576173,0.5814819981605437,0.5802446729473092,0.3996966119145189,0.3593111283153406,0.3542046263082622,0.327181240811022,0.179016617531164,0.173042973673827,0.1632255080475074,0.1542244742217237,0.1206544213052797,0.1182152679136167,0.1152842270396431,0.09785011194497026,0.07511679665427262,0.06898595164279517,0.05419890114364855,0.04968570671832601,0.04154167273285636,0.02813900659114013,0.02779924925799787,0.02701831449226791,0.02404163457354395,0.02229213785810993,0.01608522777204839,0.01388441161408212,0.01145032922739132,0.01069474948652271,0.01066939446166135,0.01031949511857455,0.009579128392622763,0.009320507139036864,0.008017258861162831,0.007931051776634198,0.007170401030793322,0.006424963299869265,0.006298188175562451,0.005978714862309284,0.005857010742974744,0.005669383559000661,0.003635910565119386,0.003078100018169411,0.002991892933640778,0.002941182883918053,0.002281952237522628,0.002018259978964457,0.001855987819851737,0.001328603302735396,0.001293106267929489,0.001135905113789041,0.0007809347657299658,0.0007352957209795132,0.0006693726563399708,0.0006643016513676982,0.0006440176314786082,0.0006135916016449731,0.0005223135121440681,0.0004208934126986179,0.0003955383878372554,0.0003346863281699854,0.0002991892933640778,0.0002890472834195328,0.0002738342685027153,0.0002129822088354452,0.0001977691939186277,0.0001977691939186277,0.0001825561790018102,0.0001521301491681751,0.0001521301491681751,0.0001521301491681751,0.0001369171342513576,0.0001369171342513576,0.0001115621093899951,0.0001014200994454501,9.12780895009051e-05,6.085205966727007e-05,5.578105469499756e-05,5.578105469499756e-05,4.056803977818004e-05,3.042602983363503e-05,2.028401988909002e-05,2.028401988909002e-05,1.521301491681752e-05,1.521301491681752e-05,1.521301491681752e-05,1.521301491681752e-05,1.521301491681752e-05,1.014200994454501e-05,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06,5.071004972272505e-06]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>country<\/th>\n      <th>total_quantity<\/th>\n      <th>total_value<\/th>\n      <th>percent_of_total_quantity<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"country","targets":1},{"name":"total_quantity","targets":2},{"name":"total_value","targets":3},{"name":"percent_of_total_quantity","targets":4}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```


<b>How do some of the top exporters compare over time? </b> <br>
This figure shows the top six exporters in terms of aggregated cotton T-shirt importers to the US between January 2024 and March 2025, which is why it looks different than just looking at March 2025 data. 

It shows that China's imports have trended down in 2025, Bangladesh's have steadily risen and Honduras and Nicaragua have spiked in 2025. 

<iframe src='https://flo.uri.sh/visualisation/23591396/embed' title='Interactive or visual content' class='flourish-embed-iframe' frameborder='0' scrolling='no' style='width:100%;height:600px;' sandbox='allow-same-origin allow-forms allow-scripts allow-downloads allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation'></iframe><div style='width:100%!;margin-top:4px!important;text-align:right!important;'><a class='flourish-credit' href='https://public.flourish.studio/visualisation/23591396/?utm_source=embed&utm_campaign=visualisation/23591396' target='_top' style='text-decoration:none!important'><img alt='Made with Flourish' src='https://public.flourish.studio/resources/made_with_flourish.svg' style='width:105px!important;height:16px!important;border:none!important;margin:0!important;'> </a></div>

<b>Currently, what are the top countries exporting cotton T-shirts to the US? </b> <br>
In March 2025, the most recent data available, the top countries exporting cotton T-shirts to the US in terms of total quantity were Bangladesh, India and Vietnam. 

``` r
# find the top 10 countries exporting cotton T-shirts to the US in March 2025 - the most current data 

quantity_march_x2025 <- all_countries %>%
  filter(year == 2025, month == 3) %>%
  group_by(country) %>%
  summarize(
    total_quantity = sum(quantity, na.rm = TRUE),
    total_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 10)

datatable(quantity_march_x2025, options = list(pageLength = 10)) 
```

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-fe4f8d7d37080ab56d1a" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-fe4f8d7d37080ab56d1a">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10"],["Bangladesh","India","Vietnam","Indonesia","China","Sri Lanka","Cambodia","Honduras","Madagascar","Mexico"],[509635,374333,219352,113875,63734,58610,27561,26664,17659,17579],[32490923,28070893,24246907,12082398,4037125,6452224,2306981,4874226,2384990,3645732]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>country<\/th>\n      <th>total_quantity<\/th>\n      <th>total_value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"country","targets":1},{"name":"total_quantity","targets":2},{"name":"total_value","targets":3}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

<b>What are the top exporters of cotton T-shirts to the US between 2020 and 2025?</b>
<br>
Explore the datatable and sort each year column to see top countries by total quantity shipped to the US for that year. 2025 data is not complete because it's only available until March 2025. 

Based on this, Bangladesh, India, Vietnam and China have consistently been the top five exporters to the US, which some variation each year. 


``` r
# find the top 10 countries exporting cotton T-shirts to the US for each year between 2020 and 2025

quantity_x2020_x2025 <- all_countries%>%
  group_by(country, year)%>%
  summarize(yearly_total = sum(quantity, na.rm=TRUE))%>%
  pivot_wider(
    names_from = year,
    values_from = yearly_total,
    names_prefix = "year_"
  )%>%
  select(country, year_2020, year_2021, year_2022, year_2023, year_2024, year_2025)

datatable(quantity_x2020_x2025, options = list(pageLength = 10)) 
```

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-670763fb83860288f0b4" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-670763fb83860288f0b4">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148"],["Afghanistan","Albania","Algeria","Argentina","Armenia","Aruba","Australia","Austria","Bahamas","Bahrain","Bangladesh","Belarus","Belgium","Benin","Bermuda","Bolivia","Bosnia-Hercegovina","Brazil","British Virgin Isls.","Brunei","Bulgaria","Burma (Myanmar)","Burundi","Cambodia","Cameroon","Canada","Cayman Islands","Chile","China","Colombia","Congo, Republic of","Cook Islands","Costa Rica","Cote D’Ivoire","Croatia","Czech Republic","Denmark","Dominican Republic","Ecuador","Egypt","El Salvador","Eritrea","Estonia","Ethiopia","Federated States of Mic","Fiji","Finland","France","French Indian Ocean Areas","French Polynesia","French West Indies","Gabon","Georgia","Germany","Ghana","Greece","Guatemala","Guinea","Guinea-Bissau","Haiti","Honduras","Hong Kong","Hungary","Iceland","India","Indonesia","Iraq","Ireland","Israel","Italy","Japan","Jordan","Kenya","Kuwait","Laos","Latvia","Lebanon","Liberia","Lithuania","Macau","Madagascar","Malaysia","Maldive Islands","Mali","Malta","Mauritania","Mauritius","Mexico","Moldova","Morocco","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","North Macedonia","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russia","Rwanda","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Sint Maarten","Slovakia","Slovenia","Solomon Islands","South Africa","South Korea","Spain","Sri Lanka","St Vincent/Grenadines","Sweden","Switzerland","Syria","Taiwan","Tanzania","Thailand","Togo","Tokelau Islands","Trinidad and Tobago","Tunisia","Turkey","Uganda","Ukraine","United Arab Emirates","United Kingdom","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"],[null,1802,null,1,null,2,272,29,null,null,7158524,9,83,null,null,null,9,76,null,1117,564,102009,null,135853,null,3580,null,16,2031197,2234,null,null,100,8,86,113,42,32580,400,57078,53933,null,558,246373,1,726,null,18731,null,85,null,null,5,856,154,60,2137,null,null,27382,167565,5321,345,null,1877426,1662860,null,254,570,68565,4652,1868,299884,null,12014,19,25,4,2178,247,132390,228190,null,57,14,null,358623,157679,39,12542,null,8386,261,15,419192,3,74,4261,null,1,92061,26,null,null,23931,144526,3714,23349,null,13143,null,null,1,6,81,null,null,null,128,5,82,77,11594,2169,677336,null,36,693,1,4959,null,26211,null,null,null,4520,104572,null,451,14,3140,22,null,null,2555001,null,null,130],[3,1868,null,4234,null,null,198,15,1,null,6532227,14,49,153,1,null,4,152,null,687,583,72499,null,193278,2,4301,null,14,1835575,5087,5,null,4,null,1,228,79,31039,1458,68473,89945,null,1494,211477,null,407,6,15577,null,null,null,null,3,4040,602,176,15905,null,null,64398,236641,7018,291,null,2416740,1111146,10,111,321,76951,5199,2001,264129,2,351,15,72,4,1497,16,174141,8634,null,85,null,16,102035,308992,12,34785,null,8050,130,null,463035,null,483,16704,6,4,104404,36,null,null,21688,87158,4296,33819,8,21489,14,null,42,98,59,3,6,null,153,3,null,169,8499,3331,571150,1,81,868,null,704,null,62349,11,70,null,4954,152218,2,407,3,2326,6,null,4,2800363,null,1,157],[6,2369,103,246,7,null,398,18,null,6,9519320,null,107,null,null,null,null,321,null,1066,1738,128380,null,395821,null,5886,null,75,2721070,10271,null,5,5,null,1,252,74,68762,871,75244,140119,null,2100,191904,null,1823,11,29161,null,null,null,null,null,1807,276,103,6052,null,null,46879,289913,7793,329,1,3702358,1583054,null,43,484,106729,4027,19234,240420,1,1596,18,56,6,1380,7,300700,18,null,59,null,null,76153,294464,12,31460,1,15150,392,1,477925,null,282,30144,null,null,146378,364,1,81,26637,95473,7549,44571,2,24744,null,null,115,94,241,25,287,null,150,null,null,203,11776,1283,880678,null,100,1191,null,1608,null,149343,null,null,306,12574,212702,null,63,511,3487,1,18,null,3720747,null,1,384],[7,3854,null,567,38,null,409,13,null,null,8525529,null,293,null,null,5,2,213,28,976,1315,89921,null,254282,null,4129,null,668,2108613,12815,null,null,null,null,14,124,29,50613,869,54141,119787,null,1943,220379,4,2394,2,22384,null,null,4,null,1,1169,276,36,2747,null,null,40151,251985,3612,376,1,3320102,1404767,8,36,372,135282,10507,9661,187290,1,2374,38,null,null,1106,513,249040,21,null,47,null,1,92022,266641,9,40648,null,13839,1865,66,420089,null,391,3709,32,null,142618,29,null,47,35657,45124,7717,42364,1,15841,null,null,null,122,94,37,3513,6,219,22,null,96,12401,1864,748075,null,424,852,null,916,null,175751,null,null,10,10812,170904,null,14,3447,4605,1,null,5,2760120,null,null,677],[54,2258,null,1,30,null,590,18,1,null,7578132,null,103,null,null,null,null,366,null,1564,1838,69849,36,218418,null,2738,4,1118,1780172,5482,null,null,null,null,66,121,30,23793,717,78820,35302,3,1889,170999,3,1179,3,22734,1,null,null,3,1,580,154,83,9798,null,null,32188,264559,5328,398,null,3219412,1247334,null,20,22,114668,5549,70856,195024,607,2109,39,null,2104,1267,4,248424,27,42,8,1,3,64520,212434,27,30413,null,13604,2035,30,309431,null,224,1581,1,null,114424,450,null,1,23312,10688,3172,34124,null,14813,null,1,39,145,131,2,59,null,262,11,null,78,8192,1242,631732,null,4741,1155,null,1414,57,139891,12,null,null,19296,164181,null,11,127,4396,null,132,1,2488060,6,null,255],[15,623,null,null,19,null,84,5,null,null,1840492,null,15,null,null,null,null,145,null,232,406,25472,null,93640,null,739,null,296,430973,712,null,null,null,null,2,31,5,10658,269,15030,2816,null,965,41769,null,null,1,3653,null,null,null,null,null,83,12,21,1516,4,4,8170,64027,1304,82,34,1051117,291001,null,11,26,31154,1765,5311,17499,null,406,16,32,null,213,null,60838,1,null,24,null,null,3697,52045,13,7208,null,267,25,1,41149,null,19,546,1,null,43792,116,1,1,5882,1143,1375,5912,null,3636,null,null,null,37,27,null,27,null,47,null,null,null,1406,532,141415,null,515,753,null,244,null,40338,null,null,null,2351,31758,null,1,8,876,null,642,null,650571,null,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>country<\/th>\n      <th>year_2020<\/th>\n      <th>year_2021<\/th>\n      <th>year_2022<\/th>\n      <th>year_2023<\/th>\n      <th>year_2024<\/th>\n      <th>year_2025<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"country","targets":1},{"name":"year_2020","targets":2},{"name":"year_2021","targets":3},{"name":"year_2022","targets":4},{"name":"year_2023","targets":5},{"name":"year_2024","targets":6},{"name":"year_2025","targets":7}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

### Original pitch
It was originally suggested that I look into Trump's recent cuts on top promotional product industries, including education, healthcare, manufacturing and others. I chose to pitch this idea instead because a key factor in those industries, GDP data, is not available for 2025 until June 26. I also struggled to narrow down that topic and was unsure about whether I could produce meaningful analysis in a week given how many story possibilities there are within that topic. 

But when the data is available, I plan to use it and assess how each industry has performed in 2025 and compare it to the past five to 10 years. I also want to quantity Trump's cuts from the beginning of the year, do some explanatory analysis, and also consider how factors like further cuts in the proposed budget will affect the promotional products industry. 

### Questions
1. How have tariff rates changed over time for top importers of cotton T-shirts? 
2. How has the general rate of imports changed over time for all countries? (Big jump in the beginning of 2025 as people rushed to buy products before tariffs hit) 
3. How do cotton T-shirt imports compare with other apparel items, like polyester shirts? 
4. What's an example of how a tariff on an example brand item would affect a consumer? How much would it raise prices? 


While tariffs have wide-reaching effects on the promotional product industry, this story may encourage manufacturers to be more creative outside of T-shirts for their promotional products or seek domestic sellers. 

<b>Audience:</b> This story’s audience would be stakeholders in the promotional products industry, especially those who sell T-shirts or are interested in staying up-to-date about tariff developments. I can interview industry experts, T-shirt manufacturers and economic experts with expertise on how tariffs work.
