update parties set
 org_code = 'FRC00016218',
 address_code = 'FRO00016616',
 org_id = '132823',
 address_id = '192137',
 org_data =
 '{
      "Id": 132823,
      "City": "PARTA",
      "Guid": "a04f9ac9-6864-477a-9bce-fbb6336ee3cd",
      "Email": "npotcovaru@frigoglass.com",
      "State": "TM",
      "label": "FRIGOGLASS ROMANIA S.R.L",
      "value": "FRC00016218",
      "Country": "ROU",
      "Address1": "DN 59 TIMISOARA - MORAVITA, KM 16",
      "FullName": "FRIGOGLASS ROMANIA S.R.L",
      "Payables": false,
      "CompanyId": 372,
      "Receivables": true,
      "ZipPostCode": "307396",
      "ForworderAgent": false,
      "OrganizationCode": "FRC00016218",
      "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
  }',
 address_data =
 '{
      "Id": 192137,
      "City": "PARTA",
      "Guid": "e0332ce5-1caa-43fd-b353-1c078de4e713",
      "Email": "npotcovaru@frigoglass.com",
      "State": "Timis",
      "Country": "ROU",
      "OrgGuid": "a04f9ac9-6864-477a-9bce-fbb6336ee3cd",
      "Address1": "DN 59 TIMISOARA - MORAVITA, KM 16",
      "OrgSource": "CRP",
      "AddressType": 2,
      "CompanyName": "FRIGOGLASS ROMANIA S.R.L",
      "OrgFullName": "FRIGOGLASS ROMANIA S.R.L",
      "OrgPayables": false,
      "ZipPostCode": "307396",
      "TaxRegNumber": "J35327524111994",
      "OrgReceivables": true,
      "OrgActiveClient": true,
      "AddressShortCode": "FRO00016616",
      "OrgOrganizationCode": "FRC00016218"
  }'
 where id IN ('1042482');

 update shipment_details
 set client_dps_address_id = 87867
 where id = 68555;