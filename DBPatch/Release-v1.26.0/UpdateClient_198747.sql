update parties set
org_code = 'FRC00011310',
address_code = 'FRO00011625',
org_data = '{"Receivables": false,"Email": "na@na.com","CompanyId": 372,"ZipPostCode": "91555","Payables": false,"City": "Feuchtwangen","Guid": "01a5a0de-4ca7-4685-9951-3f00df9986d4","label": "arcon Flach- und Sicherheitsglas GmbH & Co. KG","Address1": "Werk Sicherheitsglas Industriestraße 10","ForworderAgent": false,"State": "Bavaria","FullName": "arcon Flach- und Sicherheitsglas GmbH & Co. KG","Country": "DEU","OrganizationCode": "FRC00011310","Id": 125798,"value": "FRC00011310","InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com"}',
address_data = '{"AddressShortCode": "FRO00011625","Email": "na@na.com","OrgFullName": "arcon Flach- und Sicherheitsglas GmbH & Co. KG","OrgSource": "CRP","ZipPostCode": "91555","OrgReceivables": false,"City": "Feuchtwangen", "Guid": "437ae2a4-1ab1-4958-a2b5-3d56b604f2e0","Address1": "Werk Sicherheitsglas Industriestraße 10", "CompanyName": "arcon Flach- und Sicherheitsglas GmbH & Co. KG","State": "Bavaria","OrgOrganizationCode": "FRC00011310", "Country": "DEU","Id": 161731, "OrgActiveClient": true, "AddressType": 2,"OrgPayables": false}',
org_id = '125798',
address_id = '161731'
where id in (select client_id from shipment_details where id in ('31615') and tenant_id = '551');