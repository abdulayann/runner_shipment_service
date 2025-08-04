UPDATE parties
SET
  org_id = '147025',
  org_code = 'FRC00030167',
  address_id = '238682',
  address_code = 'FRO00031144',
  org_data ='{"Id":147025,"City":"Ostrava","Guid":"0247d8f1-2b63-40c3-8077-9164e115a964","Email":"petr.revaj@dhl.com","IsGSA":false,"Phone":"739547756","label":"DHL Express Czech Republic s.r.o.","value":"FRC00030167","Broker":false,"Source":"CRP","Carrier":false,"Country":"CZE","Address1":"Nadrazni 2967/93, Moravska Ostrava","FullName":"DHL Express Czech Republic s.r.o.","IsActive":1,"Payables":false,"Services":false,"TenantId":442,"CompanyId":372,"Consignee":true,"Consigner":true,"IsParnter":false,"PANNumber":"25683446","TaxVendor":false,"WareHouse":false,"AirCarrier":false,"ExtraParam":"","InsertDate":"2025-03-27T16:37:52.000","SeaCarrier":false,"UpdateDate":"2025-04-23T10:25:30.000","IsSuspended":false,"RailCarrier":false,"Receivables":true,"RoadCarrier":false,"ZipPostCode":"70200","ActiveClient":true,"CurrencyCode":"CZK","EmailInvoice":false,"InsertUserId":1712,"UpdateUserId":1712,"VatRegNumber":"CZ25683446","ForworderAgent":false,"IsCreditEnabled":false,"IsV2PaymentTerm":false,"TransportClient":false,"IsWalkInCustomer":false,"OrganizationCode":"FRC00030167","EnableBulkInvoice":false,"CustomerIdentifier":"1829790_001","ReceivableTermsDate":2,"InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com","UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com","IsClientEInvoiceEnabled":false,"ImportExportClearanceLock":false,"FusionCreditlimitOverrideApproved":false}',

  
  address_data = '{"Id":238682,"City":"Ostrava","Email":"petr.revaj@dhl.com","label":"DHL Express Czech Republic s.r.o., Nadrazni 2967/93, Moravska Ostrava, Ostrava, CZE","value":"FRO00031144","Country":"CZE","OrgGuid":"0247d8f1-2b63-40c3-8077-9164e115a964","Address1":"Nadrazni 2967/93, Moravska Ostrava","OrgSource":"CRP","AddressType":1,"CompanyName":"DHL Express Czech Republic s.r.o.","OrgFullName":"DHL Express Czech Republic s.r.o.","OrgPayables":false,"ZipPostCode":"70200","TaxRegNumber":"CZ25683446","OrgReceivables":true,"SiteIdentifier":"1829790_001_CZLOU_001_B","OrgActiveClient":true,"AddressShortCode":"FRO00031144","OrgOrganizationCode":"FRC00030167"}'
WHERE id = '1242122';



update shipment_details
set client_dps_address_id = 95830
where id = 79814;