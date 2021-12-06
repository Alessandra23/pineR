# Download weather data
library(googledrive)

#  Hortland
downloadData(name = "dunsanyHortland",
             link = "https://docs.google.com/spreadsheets/d/1FWqEU2iUUz0mE3T9Ye2n8fuL6-WYtt8W/edit#gid=849338479",
             path = "Data/Weather Stations/",
             type = "xlsx", gdrive = TRUE)

downloadData(name = "derrygreenaghHortland",
             link = "https://docs.google.com/spreadsheets/d/1hIvFy1KQP8cV4ypjhMx4DY4FxcYkTLZN/edit#gid=648005046",
             path = "Data/Weather Stations/")

downloadData(name = "ballinlaHouseHortland",
             link = "https://docs.google.com/spreadsheets/d/18SJlW0GL215bc5aQ7ZH5W1rN5bTAHcJf/edit#gid=102146889",
             path = "Data/Weather Stations/")

downloadData(name = "lullymoreHortland",
             link = "https://docs.google.com/spreadsheets/d/1DCuFpe5NKIrfKCtClyDLTut5zChDNgxf/edit#gid=11036804",
             path = "Data/Weather Stations/")




## COntinuar daqui

# Weather station: Ashford - Ballinagee
ashford_ballinagee_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1Zx88rFQnAFWVVKBjSY3I28jiwXs4T8f6/edit#gid=549792086"),
  path = 'Data/ashford_ballinagee_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Glenealy - Ballinagee
glenealy_ballinagee_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1Mu2s7Qm9SDFdQO_IfvkvVWIQbPglYDYt/edit#gid=1733744410"),
  path = 'Data/glenealy_ballinagee_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ashford - Oakwood
ashford_oakwood_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1q5EbUpHoUvOu_9q5ajLS4Iv7Rpsr27RE/edit#gid=1564937292"),
  path = 'Data/ashford_oakwood_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Glenealy - Oakwood
glenealy_oakwood_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1PsPiAJqJIDo3_MozyUuV1rbe0shNQuAx/edit#gid=83099104"),
  path = 'Data/glenealy_oakwood_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Nealstown - Glendine 1
nealstown_glendine_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1gakcZSVfwJwBmrzsWdshbCrxwPlgWOOr/edit#gid=496898422"),
  path = 'Data/nealstown_glendine_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Gurteen - Glendine 1
gurteen_glendine_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1_qsBe1YQSbaGGs5AvR8qysf9oQn5RxtM/edit#gid=1973907557"),
  path = 'Data/gurteen_glendine_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Durrow - Glendine 1
durrow_glendine_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1Sh1bs-sQEDnIIv5seUQ8AHh7ow5S60yX/edit#gid=2117804555"),
  path = 'Data/durrow_glendine_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Nealstown - Glendine 2
nealstown_glendine_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1qXvSpZKhuYcsvtVrUmcIWJMpBdlgr7ar/edit#gid=1362606147"),
  path = 'Data/nealstown_glendine2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Gurteen - Glendine 2
gurteen_glendine_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1FvH39OpA2g7X5buLe20OTs6vFDhmG2tV/edit#gid=1197944619"),
  path = 'Data/gurteen_glendine2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Moorepark - Lacknrea
moorepark_lacknrea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1P6WgDflZd5R3kIVEhWC3f2p3394QOCy_/edit#gid=1692591770"),
  path = 'Data/moorepark_lacknrea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Dungarvan - Lacknrea
dungarvan_lacknrea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1nGnK9dYi6w_JNEREqWXRNOyMjwq6zXLI/edit#gid=65424407"),
  path = 'Data/dungarvan_lacknrea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballincurrig - Lacknrea
ballincurrig_lacknrea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1XySzOEgUM_HZUVydBh0DloiH2BlX86Oh/edit#gid=400349753"),
  path = 'Data/ballincurrig_lacknrea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Fethard - Lacknrea
fethard_lacknrea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1mH8UKCQDhy3dZg7FPbSs5_Esf4yuhIdR/edit#gid=399246493"),
  path = 'Data/fethard_lacknrea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Dunsany - Summerhill
dunsany_summerhill_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1oWhnkg9QrTyo5wL7WorioCBXUrNttMDh/edit#gid=363176389"),
  path = 'Data/dunsany_summerhill_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Oakwood - Deerpark
oakwood_deerpark_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1rif8PeKrRIL5m1MaQ0GWev9iBlkyj2sm/edit#gid=387294123"),
  path = 'Data/oakwood_deerpark_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Glenealy - Deerpark
glenealy_deerpark_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1yWm6wr24rsSiq1-1L1b5FE1a6OZcGotl/edit#gid=295736229"),
  path = 'Data/glenealy_deerpark_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mount Russel - Ballymacshaneboy
mountrussel_ballymacshaneboy_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1Wn3s_vkteOGUSK7uedLQBR2Ns3aJitwN/edit#gid=1273186342"),
  path = 'Data/mountrussel_ballymacshaneboy_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mullingar - Kilduff
mullingar_kilduff_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1YrWZ7v6sWk2IsftOVsrPNKR7XqeVkrMy/edit?rtpof=true#gid=1392737468"),
  path = 'Data/mullingar_kilduff_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Kilduff
derrygreenagh_kilduff_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1QvjLfi77kXMuAC23uWTNrUOCc3iGh_0l/edit#gid=311772492"),
  path = 'Data/derrygreenagh_kilduff_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Kilduff
ballinla_house_kilduff_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1aE7KIEcLSHoIHdHtcK_2dapWzvM0MnJa/edit#gid=772682915"),
  path = 'Data/ballinla_house_kilduff_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Horse Leap - Kilduff
horse_leap_kilduff_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1aE7KIEcLSHoIHdHtcK_2dapWzvM0MnJa/edit#gid=772682915"),
  path = 'Data/horse_leap_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Oakpark - Rossnagad
oakpark_rossnagad_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1-nQToG6oavpxQo0TMWkaQGQEjAI2PTJ3/edit#gid=1915491499"),
  path = 'Data/oakpark_rossnagad_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Rossnagad
ballinla_house_rossnagad_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1qJEscCOyGpnprsPeXDX-k15XHbuRAcvh/edit#gid=1534201503"),
  path = 'Data/ballinla_house_rossnagad_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Durrow - Rossnagad
durrow_rossnagad_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1qfcbu9amFu5Tg8S0KRQsUJ7kTuRRN0mw/edit#gid=989988742"),
  path = 'Data/durrow_rossnagad_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Nealstown - Rossnagad
nealstown_rossnagad_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1yj-tN72cH1PrtkANDIaFmusf3DptWGDG/edit#gid=68718657"),
  path = 'Data/nealstown_rossnagad_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athy - Ballyroan 1
athy_ballyroan1_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1ci3D2NrgZYKheAozQlQHXBJXKPfONQZV/edit#gid=685107390"),
  path = 'Data/athy_ballyroan1_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Ballyroan 1
ballinla_house_ballyroan1_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/13jC3y1HQQ4eDB7FQmCMSfuMCkbzIgJ3s/edit#gid=1367323961"),
  path = 'Data/ballinla_house_ballyroan1_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Nealstown - Ballyroan 1
nealstown_ballyroan1_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1fVicBP7vLKbV-oOAUfeJs4Glvreww_9Z/edit#gid=255313562"),
  path = 'Data/nealstown_ballyroan1_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Ballyroan 1
derrygreenagh_ballyroan1_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1bxNowg_d61EMuKpbQzKmE39-4d3-mkHo/edit#gid=1295631686"),
  path = 'Data/derrygreenagh_ballyroan1_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athy - Ballyroan 2
athy_ballyroan2_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1EKXkjTlUGSfEMeI0XLpSztub9RSeNY4t/edit#gid=1519392834"),
  path = 'Data/athy_ballyroan2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Ballyroan 2
ballinla_house_ballyroan2_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1lVunODpeJRcoHqrxXqt5r3TYzBixmJSW/edit#gid=1648970569"),
  path = 'Data/ballinla_house_ballyroan2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Nealstown - Ballyroan 2
nealstown_ballyroan2_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1C4F5lJq5BM9zsE7-AdahfIm-M3-aHlxu/edit#gid=1914122730"),
  path = 'Data/nealstown_ballyroan2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Ballyroan 2
derrygreenagh_ballyroan2_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1792hZ1VZok63JDydNIg8OqtgF5pQgw_a/edit#gid=2056811640"),
  path = 'Data/derrygreenagh_ballyroan2_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Donadea
ballinla_house_donadea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1A37S2iIZjWf79knAdTo2UC7QHZfC_lYh/edit#gid=1472576234"),
  path = 'Data/ballinla_house_donadea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Donadea
derrygreenagh_donadea_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1wzgsAD5munyuUuHkNGc191MlV7XjSm0n/edit#gid=279507087"),
  path = 'Data/derrygreenagh_donadea_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mount Dillon - Cloondara
mount_dillon_cloondara_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1j5HvyTlTVHusfp5qHtRCdjITS0PIdxXL/edit#gid=555488580"),
  path = 'Data/mount_dillon_cloondara_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mullingar - Knockaville
mullingar_knockaville_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1bNI0w7nRU5uAjhzYmoZb2XqWec05MJFQ/edit#gid=753302658"),
  path = 'Data/mullingar_knockaville_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Knockaville
derrygreenagh_knockaville_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1dMTHPjbZrnzNTlcafGFqE-9PBjGEhn8J/edit#gid=132064984"),
  path = 'Data/derrygreenagh_knockaville_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Ballinla House - Knockaville
ballinla_house_knockaville_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1EJog1jnLz7o6ZVKvCXVgGRuzKfOeaqjk/edit#gid=1997655247"),
  path = 'Data/ballinla_house_knockaville_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Horseleap - Knockaville
horseleap_knockaville_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1JbMlWAXjRB8ccVUCmXGL4qfM-Kse_4eN/edit#gid=172767422"),
  path = 'Data/horseleap_knockaville_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Fethard - Kilurney
fethard_kilurney_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1vKblFbUjdfuC9whueudrzvuVwsXYTYa4/edit#gid=976320838"),
  path = 'Data/fethard_kilurney_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Cashel - Kilurney
cashel_kilurney_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1UzMd8B8ajVOMfBDKZew08gmbpfYaTFfa/edit#gid=1091188279"),
  path = 'Data/cashel_kilurney_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Dungarvan - Kilurney
dungarvan_kilurney_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1lnoWniKL26OGycre-ASbluotFQGqLGxh/edit#gid=933342761"),
  path = 'Data/dungarvan_kilurney_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Kilkenny - Kilurney
kilkenny_kilurney_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1jsIzXnhcU3Y2MUviXCy2lDuHwR1Onxna/edit#gid=362863409"),
  path = 'Data/kilkenny_kilurney_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Horseleap - Doon
horseleap_doon_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1cfQfLXAaQ18XxUD5It-tDB5HSt9HmhO4/edit#gid=2031661179"),
  path = 'Data/horseleap_doon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Gurteen - Doon
gurteen_doon_gd <- drive_download(
  as_id("hhttps://docs.google.com/spreadsheets/d/1W7GfSduQ4ucmSoPIG6QEhXq6uQsorAHG/edit#gid=1926767941"),
  path = 'Data/gurteen_doon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mullingar - Doon
mullingar_doon_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1l-epu5Zut1b1TiMtB4ymBL2gVXsBdOOw/edit#gid=444905697"),
  path = 'Data/mullingar_doon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Nealstown - Clonoghil
nealstown_clonoghil_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1ZKmCJs-LaNiYfi1d5pVcYSRcSqgTLRk7/edit#gid=1769073385"),
  path = 'Data/nealstown_clonoghil_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Glenealy - Tigroney
glenealy_tigroney_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1BqRl5UzMNIQo6XDm396_smvI7R5jVytU/edit#gid=1454592606"),
  path = 'Data/glenealy_tigroney_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Fethard - Gurtnapisha
fethard_gurtnapisha_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/15SjSZB4HWgMMBjAYtbtN_n5mRaVXOCY6/edit#gid=89273476"),
  path = 'Data/fethard_gurtnapisha_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Mullingar - Rickardstown
mullingar_rickardstown_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1C4Muqw688f5da4T5B8zetabSMumWa1vT/edit#gid=108649663"),
  path = 'Data/mullingar_rickardstown_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Derrygreenagh - Rickardstown
derrygreenagh_rickardstown_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1DyiJeSUcaH19E21XockQ6kgc-FfKSlBn/edit#gid=1935417646"),
  path = 'Data/derrygreenagh_rickardstown_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Ballinla House - Rickardstown
ballinla_house_rickardstown_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1mgxm9VcSlKpvM4Wso6WcuaPLymU5Lptq/edit#gid=2145210903"),
  path = 'Data/ballinla_house_rickardstown_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Fethard- Longfordpass
fethard_longfordpass_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1RP4OgEcAKP5gPU970skFKBDp0AEiNO5H/edit#gid=753876997"),
  path = 'Data/fethard_longfordpass_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Durrow- Longfordpass
durrow_longfordpass_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1bjAZKOIM78kT2xEKlk3zD89t6AAf4xTt/edit#gid=1532188084"),
  path = 'Data/durrow_longfordpass_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Kilkenny- Longfordpass
kilkenny_longfordpass_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1nnNgvkrgccrp7Ues2R7IgvhI78feBc8_/edit#gid=37845484"),
  path = 'Data/kilkenny_longfordpass_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Nealstown- Longfordpass
nealstown_longfordpass_gd <- drive_download(
  as_id("hhttps://docs.google.com/spreadsheets/d/1A2bG5HtxQjWPIenmHbEZ_JHQKYrLIHsK/edit#gid=1267437544"),
  path = 'Data/nealstown_longfordpass_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Knock Airport- Cashelduff
knock_airport_cashelduff_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1ORR-7PMXeQ4sc25-6-WXCwVFZIFCEYkf/edit#gid=1015441292"),
  path = 'Data/knock_airport_cashelduff_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athenry- Woodford
athenry_woodford_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1S7JMcqXAe9wKZdrpCCgOzdFMp_OSupF8/edit#gid=152073375"),
  path = 'Data/athenry_woodford_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Gurteen - Woodford
gurteen_woodford_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1LzSh-c8uucBf5RZi9MZD6AMUGVWgy_mN/edit#gid=668370201"),
  path = 'Data/gurteen_woodford_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athy - Ballybrittas
athy_ballybrittas_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1kItkkhOffPcTJdH3wtQDEkou3kmEwFKr/edit#gid=516871812"),
  path = 'Data/athy_ballybrittas_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Ballinla House - Ballybrittas
ballinla_house_ballybrittas_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1DXGCEv3pyjvOv8dW781_FOETQjU1upt6/edit#gid=1583815335"),
  path = 'Data/ballinla_house_ballybrittas_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")

# Weather station: Derrygreenagh - Ballybrittas
derrygreenagh_ballybrittas_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1C4EfmnXn7lA59aAwXE2yTPUSRK1Nj6wq/edit#gid=234984106"),
  path = 'Data/derrygreenagh_ballybrittas_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athenry - Corracloon
athenry_corracloon_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/19-j9aFrX785IC5cJxcY1y_1WGyg0sddp/edit#gid=149015213"),
  path = 'Data/athenry_corracloon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Shannon Airport - Corracloon
shannon_airport_corracloon_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1dCFrKiXARlrC_bje9AcXpyFBr1mD_7FD/edit#gid=2027663094"),
  path = 'Data/shannon_airport_corracloon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Gurteen - Corracloon
gurteen_corracloon_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/18Hfz6T0dfE0EpKBz84DC9WuJ9iuEaNVz/edit#gid=82901107"),
  path = 'Data/gurteen_corracloon_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Athenry - Corrakyle
athenry_corrakyle_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1HQIi_9D5hMWBmpGUbQBdhm7eaoCa-1fD/edit#gid=87294011"),
  path = 'Data/athenry_corrakyle_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Shannon Airport - Corrakyle
shannon_airport_corrakyle_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1VlVzHCMqjoYIsX87sDGYWzzTFBFqJPfi/edit#gid=2080014209"),
  path = 'Data/shannon_airport_corrakyle_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")


# Weather station: Gurteen - Corrakyle
gurteen_corrakyle_gd <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1xK7d7_M13p31oS9hfJV0d4LoccIKl1R6/edit#gid=544175683"),
  path = 'Data/gurteen_corrakyle_tempdata_3years.xlsx',
  overwrite = TRUE,
  type = "xlsx")






