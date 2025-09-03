update parties set
 org_code = 'FRC00031466',
 address_code = 'FRO00032506',
 org_id = '148049',
 address_id = '240771',
 org_data =
 '{
       "Id": 148049,
       "Guid": "6214bc64-b6f1-4573-bbe6-cc9fd0f187bc",
       "OrganizationCode": "FRC00031466",
       "FullName": "ROTAREX PRAHA, Spol. S R. O.",
       "Address1": "Plzeňská 2085",
       "Country": "CZE",
       "City": "Tachov",
       "ZipPostCode": "347 01",
       "Email": "dvorak.vaclav@rotarex.com",
       "ForworderAgent": false,
       "Receivables": true,
       "Payables": false,
       "CompanyId": 372,
       "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
       "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
       "label": "ROTAREX PRAHA, Spol. S R. O.",
       "value": "FRC00031466"
   }',
 address_data =
 '{
      "Id": 240771,
      "Guid": "01c24ea0-48cb-456b-aa03-8f7ec293ce07",
      "AddressShortCode": "FRO00032506",
      "CompanyName": "ROTAREX PRAHA, Spol. S R. O.",
      "AddressType": 1,
      "SiteIdentifier": "1841575_001_CZLOU_001_B",
      "Address1": "Plzeňská 2085",
      "Country": "CZE",
      "City": "Tachov",
      "ZipPostCode": "347 01",
      "Email": "dvorak.vaclav@rotarex.com",
      "OrgGuid": "6214bc64-b6f1-4573-bbe6-cc9fd0f187bc",
      "OrgOrganizationCode": "FRC00031466",
      "OrgSource": "CRP",
      "OrgFullName": "ROTAREX PRAHA, Spol. S R. O.",
      "OrgActiveClient": true,
      "OrgReceivables": true,
      "OrgPayables": false,
      "TaxRegNumber": "CZ61169951",
      "RegulatedAgent": false,
      "KnownConsignor": false
  }'
 where id IN ('1072168');

 update shipment_details
 set client_dps_address_id = 90062
 where id = 70143;