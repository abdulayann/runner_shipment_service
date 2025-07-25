UPDATE parties
SET
  org_id = '157275',
  org_code = 'FRC00039841',
  address_id = '262371',
  address_code = 'FRO00041585',
  org_data = '{"Id":157275,"City":"Dottingen","Guid":"52634fc2-8c10-4015-8e66-dfd8a62f2b3f","Email":"Nal@sara-transporte.ch","State":"Aargau","label":"Sara Transporte AG","value":"FRC00039841","Country":"CHE","Address1":"Vorhard 15","FullName":"Sara Transporte AG","Payables":false,"CompanyId":372,"Receivables":true,"ZipPostCode":"5312","ForworderAgent":false,"OrganizationCode":"FRC00039841","InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com","UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com"}',
  address_data = '{"Id":262371,"City":"Dottingen","Guid":"78f54b9b-6117-44e3-a9e1-a3e812a420d3","Email":"Nal@sara-transporte.ch","State":"Aargau","Country":"CHE","OrgGuid":"52634fc2-8c10-4015-8e66-dfd8a62f2b3f","Address1":"Vorhard 15","OrgSource":"CRP","AddressType":2,"CompanyName":"Sara Transporte AG","OrgFullName":"Sara Transporte AG","OrgPayables":false,"ZipPostCode":"5312","TaxRegNumber":"CHE264842468","KnownConsignor":false,"OrgReceivables":true,"RegulatedAgent":false,"OrgActiveClient":true,"AddressShortCode":"FRO00041585","OrgOrganizationCode":"FRC00039841"}'
WHERE id = '1168427';



update shipment_details
set client_dps_address_id = 96622
where id = 75571;