UPDATE parties
SET
  org_id = '142325',
  org_code = 'FRC00024318',
  address_id = '224684',
  address_code = 'FRO00025071',
  org_data = '{"Id": 142325, "Guid": "07ebe9a6-07e0-410e-90a6-bef1740c5ef0", "OrganizationCode": "FRC00024318", "FullName": "REVLON AUSTRALIA PTY LTD", "Address1": "LEVEL 20, 83 CLARANCE STREET NSW  CONTACT SHANE HARRIS", "Country": "AUS", "City": "SYDNEY", "ZipPostCode": "2000", "Email": "na@na.com", "ForworderAgent": false, "Receivables": false, "Payables": false, "CompanyId": 442, "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com", "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"}',
  address_data = '{"Id": 224684, "Guid": "4a3b92c1-3377-450d-b505-ba0be52c63f4", "AddressShortCode": "FRO00025071", "CompanyName": "REVLON AUSTRALIA PTY LTD", "AddressType": 2, "Address1": "LEVEL 20, 83 CLARANCE STREET NSW  CONTACT SHANE HARRIS", "Country": "AUS", "City": "SYDNEY", "State": "New South Wales", "ZipPostCode": "2000", "Email": "na@na.com", "OrgGuid": "07ebe9a6-07e0-410e-90a6-bef1740c5ef0", "OrgOrganizationCode": "FRC00024318", "OrgSource": "CRP", "OrgFullName": "REVLON AUSTRALIA PTY LTD", "OrgActiveClient": true, "OrgReceivables": false, "OrgPayables": false}'
WHERE id = '1010719';

update shipment_details
set client_dps_address_id = 86615
where id = 66742;