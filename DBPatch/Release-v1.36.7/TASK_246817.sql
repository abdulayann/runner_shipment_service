UPDATE awb
SET awb_other_info = jsonb_set(
    awb_other_info,
    '{executedAt}',
    to_jsonb(
      CASE awb_other_info ->> 'executedAt'
        WHEN '0IFotNu5a71babMrnz2Z' THEN 'Karachi Seaport'
        WHEN '1b9mJAkMoKRbbwRZTiGA' THEN 'mumbai'
        WHEN '1pyEvcu6LYMeYL1xbu4G' THEN 'passagem'
        WHEN '1QARaQBNJwyx4CQmj6CJ' THEN 'igenhausen'
        WHEN '1TLyXEuVB0TJVIpmkdnd' THEN 'santos'
        WHEN '29W22P4xWwxoI82Tmc7c' THEN 'aganwada'
        WHEN '3lqPUSXFyoNEYblgdvTn' THEN 'a.g.college'
        WHEN '5917Remg6fFo3mHzeIPv' THEN 'madrid'
        WHEN '73xEChJxAFfv62anIzAG' THEN '9 brd'
        WHEN '8oUAuL5XxU5xAwQlxRzE' THEN 'mumbai'
        WHEN '8oVeDNQ8CnFhauZKOAWg' THEN 'linz'
        WHEN '9nVmFhEIxDe7JbGqGqzY' THEN 'neufahrn'
        WHEN 'afylOYFhTAF1nKEf0azn' THEN 'paris'
        WHEN 'AhHJzCEcHbzwjetVQimO' THEN 'mumbai'
        WHEN 'APjOOBFXy5k07vXr25rj' THEN 'basel'
        WHEN 'arewniN2aodoJnJzd0R5' THEN 'a k kavalu'
        WHEN 'b1D8BfPFUnI3qzQZdB7P' THEN 'achalgarh'
        WHEN 'BRSSZ_TER' THEN 'Santos'
        WHEN 'CAY1oTKeRkFozou5GeNp' THEN 'bucuresti'
        WHEN 'cH0nG9cVaAsmPH90pyzp' THEN 'bucuresti'
        WHEN 'DEFRA_AIR' THEN 'Frankfurt am Main Airport'
        WHEN 'DjT1pOLnMliPEpMRJAnp' THEN 'mumbai'
        WHEN 'DkKJgvY1lRFvhEsZRHvK' THEN '3 eme center'
        WHEN 'dnYDJ4GHYYoLnWkVE6l6' THEN 'bilbao'
        WHEN 'Ds4VAABQpYhtGSQcRi2S' THEN 'dublin'
        WHEN 'ESBCN_AIR' THEN 'Barcelona International Airport'
        WHEN 'ESBCN_POR' THEN 'Barcelona Port'
        WHEN 'Et8tqEqz6pWtamLRxCZb' THEN 'katima mulilo'
        WHEN 'FLZIZiVOz6WTDCVhLNCB' THEN 'lahore'
        WHEN 'FoXWLOCvspJL5LKaD1Lb' THEN 'paris'
        WHEN 'FRBSL_AIR' THEN 'Euroairport Basel Mulhouse Freiburg Airport'
        WHEN 'FUNnGLIwk51CSZfesUDv' THEN 'schiphol'
        WHEN 'fXLfRwGPvQBfP44KK1q7' THEN 'a mallapura'
        WHEN 'gduwM8NOIIwMmJuvaHXH' THEN 'morfelden-walldorf'
        WHEN 'GSFXCTBNzxbhdVK72FiM' THEN 'charlotte'
        WHEN 'INDEL_AIR' THEN 'Indira Gandhi International Airport'
        WHEN 'INICD_POR' THEN 'new delhi'
        WHEN 'j2UffdxMlnMSFVZ0Quug' THEN 'bhooya'
        WHEN 'K81tdHX8DtdhWkfmerAy' THEN 'dublin'
        WHEN 'kcbC8ecnN4yrHA4rwjct' THEN 'schiphol'
        WHEN 'Ks5f35ceZoO8KKlCVR9V' THEN 'a. k. road'
        WHEN 'LgWtUPnGAPNAZjdlkIBo' THEN 'tilburg'
        WHEN 'lo1rebs1zhwf9lqn' THEN 'Madrid'
        WHEN 'lo2nbd5a1ebts8e1' THEN 'Barcelona'
        WHEN 'lo40tczyvqoaj88u' THEN 'Barcelona'
        WHEN 'lo4ljyi17lfy5xqk' THEN 'Barcelona'
        WHEN 'lo5l9gtpa51qaaol' THEN 'Barcelona'
        WHEN 'lo7tvdvzax11utxx' THEN 'Barcelona'
        WHEN 'lo8djntwupw9e1el' THEN 'Miami'
        WHEN 'lo8z49d90ghhdrkk' THEN 'Madrid'
        WHEN 'lo9fre5ezimp5z3t' THEN 'Madrid'
        WHEN 'loa692cpf6i9t6o2' THEN 'Kreisfreie Stadt Frankfurt Am Main'
        WHEN 'loaxbploly2aiwzc' THEN 'Barcelona'
        WHEN 'lob7xzcgw6y2ar7v' THEN '108 Mile Ranch'
        WHEN 'loci1pwc52i50j5i' THEN 'Madrid'
        WHEN 'lodssk05rzx5kgam' THEN 'Madrid'
        WHEN 'loe3lufud6d0advi' THEN 'Barcelona'
        WHEN 'lof9gym36fagf21v' THEN 'Madrid'
        WHEN 'logpxlxkxrcoeqwg' THEN 'Barcelona'
        WHEN 'loh5gfwpmuguixos' THEN 'Madrid'
        WHEN 'lohe997i000evm0n' THEN 'Charlotte'
        WHEN 'loifydqzqwor8h6b' THEN 'Madrid'
        WHEN 'loig9uf612tzhggz' THEN 'Madrid'
        WHEN 'loikvdb6cnqw86hi' THEN 'Madrid'
        WHEN 'loj6zwz7dr7wov92' THEN 'Barcelona'
        WHEN 'lojcxe9egqymskeu' THEN 'Paris'
        WHEN 'lok2dasgcd55zlya' THEN 'Madrid'
        WHEN 'lolc446othdftw25' THEN 'Barcelona'
        WHEN 'lolj49al21pq6ipu' THEN 'Madrid'
        WHEN 'lolusyra94ro0aaj' THEN 'Madrid'
        WHEN 'lom5cx09hurveptq' THEN 'Madrid'
        WHEN 'lomcen5uyqwct7w2' THEN 'Madrid'
        WHEN 'lomy91xniifszlo2' THEN 'Madrid'
        WHEN 'lonsd0n2dtjkeeui' THEN 'Madrid'
        WHEN 'loobxv59jaxra0hx' THEN 'Madrid'
        WHEN 'loodu450alby3o26' THEN 'Madrid'
        WHEN 'looncmbgb5tx40ib' THEN 'Barcelona'
        WHEN 'looo4mgti1y8i5vn' THEN 'Munich'
        WHEN 'lop4jw6okjn32o69' THEN 'Barcelona'
        WHEN 'loprnazyt4kxidvk' THEN 'Barcelona'
        WHEN 'lorb8ab0j5c4limx' THEN 'Kreisfreie Stadt Frankfurt Am Main'
        WHEN 'lormg442avbagt93' THEN 'Madrid'
        WHEN 'lot90ih6c37d9w8e' THEN 'Madrid'
        WHEN 'louccr51gzfwlfij' THEN 'Stuttgart'
        WHEN 'lovaczcvys01q9rx' THEN 'Madrid'
        WHEN 'lovjhnwq3vggroca' THEN 'Miami'
        WHEN 'loz4mh1d587y6srs' THEN 'Madrid'
        WHEN 'loz5xpvdx4piwcgg' THEN 'Madrid'
        WHEN 'mglHDgHZd3yqHn3iw9Kk' THEN 'stanwell'
        WHEN 'N6moWdkaci7Ob7uOJprM' THEN 'linz'
        WHEN 'naMl60J58jHcVzKo23cC' THEN 'santos'
        WHEN 'NbRlS1NOCvUCspDesnYp' THEN 'basel'
        WHEN 'NLAMS_AIR' THEN 'Amsterdam Airport Schiphol'
        WHEN 'NRxSFHadEXJmoiDU8ODm' THEN 'hamburg'
        WHEN 'ODrszhutZDpjkVBOUyFf' THEN 'otopeni'
        WHEN 'oH4xCUfgjE8jbPBIkff6' THEN 'Karachi Seaport'
        WHEN 'oNeXDU4PubwCn2WluM93' THEN 'frankfurt'
        WHEN 'OVJydn7UZbSu2Kjlzy1Z' THEN 'london'
        WHEN 'PKKHI_AIR' THEN 'Jinnah International Airport'
        WHEN 'PKKHI_POR' THEN 'Karachi'
        WHEN 'PnYM6oiaQgZmZqrTSWzM' THEN 'frankfurt am main'
        WHEN 'pWkTOWUqwKhofReqIAd7' THEN 'stanwell'
        WHEN 'QQNs89OilaMsk92Gjnsi' THEN 'Navi Mumbai'
        WHEN 'QsMOADT9L4wTEdxdzKGX' THEN 'duisburg'
        WHEN 'RgByBXe0hjnelN9ZyVcB' THEN 'tilburg'
        WHEN 'ROBUH_AIR' THEN 'Bucharest Airport'
        WHEN 'rOxXoSGfCULFCIlDs1TH' THEN 'munich'
        WHEN 'RRa9OfWaoawTfWEV04VE' THEN 'barcelona'
        WHEN 'RTq4jllsLigk16JgHDrI' THEN 'charlotte'
        WHEN 'S4uRVwoO145cC98fYSBK' THEN 'mÃ¶rfelden-walldorf'
        WHEN 'sKFQIdzF6g77vdcziqxQ' THEN 'louny'
        WHEN 'T5GG3ZAY3HEqBJkRF7HL' THEN '5 star midc kagal'
        WHEN 'THBKK_AIR' THEN 'Suvarnabhumi Airport'
        WHEN 'uonQrl7fjXmweauNtF2n' THEN 'Milano'
        WHEN 'USMIA_AIR' THEN 'Miami International Airport'
        WHEN 'v7nQnqNS1wZZNru0T32t' THEN 'a.gs office'
        WHEN 'VcXkI6G7y2VunoBqFDmR' THEN 'mÃ¶rfelden-walldorf'
        WHEN 'w0COtzb6b6JmHuOIBSPN' THEN 'aabud'
        WHEN 'WPkKys7C9fViPgrZHfqk' THEN 'deoriya raiputan'
        WHEN 'xPYgnWfVcIkX6XBN4kC6' THEN 'frankfurt am main'
        WHEN 'yATKX0ieevLHhVOYnLkY' THEN 'mÃ¼nchen'
        WHEN 'YFSLHbKARlKUmtC2v6Xp' THEN 'basel'
        WHEN 'yVzAIAoTWT56FMIOLlEz' THEN 'bucharest'
        WHEN 'zDnDFiXgsy28vvHWESte' THEN 'bangkok'
        WHEN 'zNJZUIIPLSOtBimXdIWH' THEN 'a.konduru'
      END
    )
)
WHERE awb_other_info ->> 'executedAt' IN (
  '0IFotNu5a71babMrnz2Z','1b9mJAkMoKRbbwRZTiGA','1pyEvcu6LYMeYL1xbu4G','1QARaQBNJwyx4CQmj6CJ',
  '1TLyXEuVB0TJVIpmkdnd','29W22P4xWwxoI82Tmc7c','3lqPUSXFyoNEYblgdvTn','5917Remg6fFo3mHzeIPv',
  '73xEChJxAFfv62anIzAG','8oUAuL5XxU5xAwQlxRzE','8oVeDNQ8CnFhauZKOAWg','9nVmFhEIxDe7JbGqGqzY',
  'afylOYFhTAF1nKEf0azn','AhHJzCEcHbzwjetVQimO','APjOOBFXy5k07vXr25rj','arewniN2aodoJnJzd0R5',
  'b1D8BfPFUnI3qzQZdB7P','BRSSZ_TER','CAY1oTKeRkFozou5GeNp','cH0nG9cVaAsmPH90pyzp',
  'DEFRA_AIR','DjT1pOLnMliPEpMRJAnp','DkKJgvY1lRFvhEsZRHvK','dnYDJ4GHYYoLnWkVE6l6',
  'Ds4VAABQpYhtGSQcRi2S','ESBCN_AIR','ESBCN_POR','Et8tqEqz6pWtamLRxCZb',
  'FLZIZiVOz6WTDCVhLNCB','FoXWLOCvspJL5LKaD1Lb','FRBSL_AIR','FUNnGLIwk51CSZfesUDv',
  'fXLfRwGPvQBfP44KK1q7','gduwM8NOIIwMmJuvaHXH','GSFXCTBNzxbhdVK72FiM','INDEL_AIR',
  'INICD_POR','j2UffdxMlnMSFVZ0Quug','K81tdHX8DtdhWkfmerAy','kcbC8ecnN4yrHA4rwjct',
  'Ks5f35ceZoO8KKlCVR9V','LgWtUPnGAPNAZjdlkIBo','lo1rebs1zhwf9lqn','lo2nbd5a1ebts8e1',
  'lo40tczyvqoaj88u','lo4ljyi17lfy5xqk','lo5l9gtpa51qaaol','lo7tvdvzax11utxx',
  'lo8djntwupw9e1el','lo8z49d90ghhdrkk','lo9fre5ezimp5z3t','loa692cpf6i9t6o2',
  'loaxbploly2aiwzc','lob7xzcgw6y2ar7v','loci1pwc52i50j5i','lodssk05rzx5kgam',
  'loe3lufud6d0advi','lof9gym36fagf21v','logpxlxkxrcoeqwg','loh5gfwpmuguixos',
  'lohe997i000evm0n','loifydqzqwor8h6b','loig9uf612tzhggz','loikvdb6cnqw86hi',
  'loj6zwz7dr7wov92','lojcxe9egqymskeu','lok2dasgcd55zlya','lolc446othdftw25',
  'lolj49al21pq6ipu','lolusyra94ro0aaj','lom5cx09hurveptq','lomcen5uyqwct7w2',
  'lomy91xniifszlo2','lonsd0n2dtjkeeui','loobxv59jaxra0hx','loodu450alby3o26',
  'looncmbgb5tx40ib','looo4mgti1y8i5vn','lop4jw6okjn32o69','loprnazyt4kxidvk',
  'lorb8ab0j5c4limx','lormg442avbagt93','lot90ih6c37d9w8e','louccr51gzfwlfij',
  'lovaczcvys01q9rx','lovjhnwq3vggroca','loz4mh1d587y6srs','loz5xpvdx4piwcgg',
  'mglHDgHZd3yqHn3iw9Kk','N6moWdkaci7Ob7uOJprM','naMl60J58jHcVzKo23cC','NbRlS1NOCvUCspDesnYp',
  'NLAMS_AIR','NRxSFHadEXJmoiDU8ODm','ODrszhutZDpjkVBOUyFf','oH4xCUfgjE8jbPBIkff6',
  'oNeXDU4PubwCn2WluM93','OVJydn7UZbSu2Kjlzy1Z','PKKHI_AIR','PKKHI_POR',
  'PnYM6oiaQgZmZqrTSWzM','pWkTOWUqwKhofReqIAd7','QQNs89OilaMsk92Gjnsi','QsMOADT9L4wTEdxdzKGX',
  'RgByBXe0hjnelN9ZyVcB','ROBUH_AIR','rOxXoSGfCULFCIlDs1TH','RRa9OfWaoawTfWEV04VE',
  'RTq4jllsLigk16JgHDrI','S4uRVwoO145cC98fYSBK','sKFQIdzF6g77vdcziqxQ','T5GG3ZAY3HEqBJkRF7HL',
  'THBKK_AIR','uonQrl7fjXmweauNtF2n','USMIA_AIR','v7nQnqNS1wZZNru0T32t',
  'VcXkI6G7y2VunoBqFDmR','w0COtzb6b6JmHuOIBSPN','WPkKys7C9fViPgrZHfqk','xPYgnWfVcIkX6XBN4kC6',
  'yATKX0ieevLHhVOYnLkY','YFSLHbKARlKUmtC2v6Xp','yVzAIAoTWT56FMIOLlEz','zDnDFiXgsy28vvHWESte',
  'zNJZUIIPLSOtBimXdIWH'
);
