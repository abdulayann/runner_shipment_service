update parties set
 org_code = 'FRC00014041',
 address_code = 'FRO00014378',
 org_data =
 '{
     "Id": 129218,
     "City": "SINGEN HOHENTWIEL",
     "Guid": "9da68249-99ca-441a-833d-3c3fc7ba24d4",
     "Email": "na@na.com",
     "State": "Baden-Württemberg",
     "label": "AMCOR FLEXIBLES SINGEN GMBH",
     "value": "FRC00014041",
     "Country": "DEU",
     "Address1": "ALUSINGENPLATZ 1",
     "FullName": "AMCOR FLEXIBLES SINGEN GMBH",
     "Payables": false,
     "CompanyId": 372,
     "Receivables": false,
     "ZipPostCode": "78224",
     "ForworderAgent": false,
     "OrganizationCode": "FRC00014041",
     "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com"
 }',
 address_data = '{
                    "Id": 184063,
                    "City": "SINGEN HOHENTWIEL",
                    "Guid": "db6ee768-3a1e-45c4-8bb7-1f62e3024418",
                    "Email": "na@na.com",
                    "State": "Baden-Württemberg",
                    "Country": "DEU",
                    "Address1": "ALUSINGENPLATZ 1",
                    "OrgSource": "CRP",
                    "AddressType": 2,
                    "CompanyName": "AMCOR FLEXIBLES SINGEN GMBH",
                    "OrgFullName": "AMCOR FLEXIBLES SINGEN GMBH",
                    "OrgPayables": false,
                    "ZipPostCode": "78224",
                    "OrgReceivables": false,
                    "OrgActiveClient": true,
                    "AddressShortCode": "FRO00014378",
                    "OrgOrganizationCode": "FRC00014041"
                }'
 where id IN (select client_id from shipment_details where shipment_id IN ('HAMS24080405', 'HAMS24080409', 'HAMS24090477', 'HAMS24090480') and tenant_id = 474);