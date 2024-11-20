update parties
set org_code = 'FRC00020282',
address_code = 'FRO00020896',
org_data = '{"Id": 135952, "Guid": "e4a7a241-c9a0-43f9-b664-537572188ed3", "OrganizationCode": "FRC00020282", "ForworderAgent": false, "FullName": "CLI Transport GmbH", "Address1": "Koehlstrasse 10", "Country": "DEU", "City": "Cologne", "State": "NW", "ZipPostCode": "50827", "Email": "markus.fruehauf@cli.ag", "Receivables": true, "Payables": false}',
address_data = '{"Id": 200522, "AddressShortCode": "FRO00020896", "Guid": "bfb9b162-6f97-41ca-8afc-40a2b52060fb", "CompanyName": "CLI Transport GmbH", "Address1": "Koehlstrasse 10", "OrgOrganizationCode": "FRC00020282", "AddressType": "1", "Country": "DEU", "City": "Cologne", "State": "Nordrhein-Westfalen", "ZipPostCode": "50827", "Email": "kontakt@cli.ag", "SiteIdentifier": "3817036_B", "TaxRegNumber": "DE329858128"}'
where id in (select client_id from shipment_details where id in (20047,22033,20670));