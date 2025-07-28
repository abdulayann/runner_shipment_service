update parties set
 org_code = 'FRC00014765',
 address_code = 'FRO00015118',
 org_id = '131056',
 address_id = '186974',
 updated_at = NOW(),
 org_data =
 '{
              "Id": 131056,
              "Guid": "ad3b3857-ab13-4eec-b835-64d631ee9247",
              "OrganizationCode": "FRC00014765",
              "FullName": "Grandex limited",
              "Address1": "Flat/RM A9 1/F TAI TAK INDUSTRIAL BLDG 2-12KWAI FAT ROAD",
              "Country": "HKG",
              "City": "Kowloon",
              "Email": "info@grandexltd.com",
              "ForworderAgent": false,
              "Receivables": true,
              "Payables": false,
              "CompanyId": 372,
              "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
              "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
  }',
 address_data =
 '{
              "Id": 186974,
              "Guid": "5ee7498e-48c2-45ae-848b-20587157fddc",
              "AddressShortCode": "FRO00015118",
              "CompanyName": "Grandex limited",
              "AddressType": 2,
              "Address1": "Flat/RM A9 1/F TAI TAK INDUSTRIAL BLDG 2-12KWAI FAT ROAD",
              "Country": "HKG",
              "City": "Kowloon",
              "Email": "info@grandexltd.com",
              "OrgGuid": "ad3b3857-ab13-4eec-b835-64d631ee9247",
              "OrgOrganizationCode": "FRC00014765",
              "OrgSource": "CRP",
              "OrgFullName": "Grandex limited",
              "OrgActiveClient": true,
              "OrgReceivables": true,
              "OrgPayables": false,
              "TaxRegNumber": "3409746300008246",
              "RegulatedAgent": false,
              "KnownConsignor": false
          }'
 where id IN ('1160416');


 update shipment_details set client_dps_address_id = 92860, updated_at = NOW()
 where id IN (75090);