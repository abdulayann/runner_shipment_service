UPDATE parties
SET
  org_id = '149891',
  org_code = 'FRC00032903',
  address_id = '246212',
  address_code = 'FRO00034014',
  org_data ='{"Id":149891,"City":"new cairo","Guid":"8a4b2e4a-e34d-4485-aadf-ae394db16539","Email":"Haytham.hamada@polarislog.com","label":"Polaris for logistics services","value":"FRC00032903","Country":"EGY","Address1":"abu el hole","FullName":"Polaris for logistics services","Payables":false,"CompanyId":372,"Receivables":true,"ZipPostCode":"11865","ForworderAgent":false,"OrganizationCode":"FRC00032903","InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com","UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com"}',
  address_data = '{"Id":246212,"Guid":"beb36510-83e7-43aa-b1fb-09e6977d3866","AddressShortCode":"FRO00034014","CompanyName":"Polaris for logistics services","AddressType":2,"Address1":"abu el hole","Country":"EGY","City":"new cairo","ZipPostCode":"11865","Email":"Haytham.hamada@polarislog.com","OrgGuid":"8a4b2e4a-e34d-4485-aadf-ae394db16539","OrgOrganizationCode":"FRC00032903","OrgSource":"CRP","OrgFullName":"Polaris for logistics services","OrgActiveClient":true,"OrgReceivables":true,"OrgPayables":false,"TaxRegNumber":"764817825","RegulatedAgent":false,"KnownConsignor":false}'
WHERE id = '1104629';



update shipment_details
set client_dps_address_id = 93959
where id = 71986;

