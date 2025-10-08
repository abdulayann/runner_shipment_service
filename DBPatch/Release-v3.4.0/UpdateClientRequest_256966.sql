-- Shipment ID : (107397)
-- Update Parties
update
    parties
    set
        org_code = 'FRC00026871',
        address_code = 'FRO00027736',
        org_id = '145614',
        address_id = '234171',
        updated_at = NOW(),
        org_data = '{
                "Id": 145614,
                "City": "Dubai",
                "Guid": "92423ed3-d406-446d-bd85-c5ce10a15565",
                "Email": "ramdas@rivoligroup.com",
                "IsGSA": false,
                "Phone": "42026800",
                "label": "Rivoli Group LLC",
                "value": "FRC00026871",
                "Broker": false,
                "Source": "CRP",
                "Carrier": false,
                "Country": "ARE",
                "Address1": "1, Sheikh Zayed Road, The H Dubai, Office Tower, Level 23, P.O. Box 121",
                "FullName": "Rivoli Group LLC",
                "IsActive": 1,
                "Payables": false,
                "Services": false,
                "TenantId": 442,
                "CompanyId": 372,
                "Consignee": true,
                "Consigner": true,
                "IsParnter": false,
                "PANNumber": "100223600600003",
                "TaxVendor": false,
                "WareHouse": false,
                "AirCarrier": false,
                "ExtraParam": "",
                "InsertDate": "2025-03-12T21:27:02.000",
                "SeaCarrier": false,
                "UpdateDate": "2025-03-26T13:40:35.000",
                "IsSuspended": false,
                "RailCarrier": false,
                "Receivables": true,
                "RoadCarrier": false,
                "ActiveClient": true,
                "CurrencyCode": "AED",
                "EmailInvoice": false,
                "InsertUserId": 1712,
                "UpdateUserId": 1712,
                "VatRegNumber": "100223600600003",
                "ForworderAgent": false,
                "IsCreditEnabled": false,
                "IsV2PaymentTerm": false,
                "TransportClient": false,
                "IsWalkInCustomer": false,
                "OrganizationCode": "FRC00026871",
                "EnableBulkInvoice": false,
                "CustomerIdentifier": "1813816_001",
                "ReceivableTermsDate": 2,
                "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                "IsClientEInvoiceEnabled": false,
                "ImportExportClearanceLock": false,
                "FusionCreditlimitOverrideApproved": false
            }',
    address_data = '{
                "Id": 234171,
                "City": "Dubai",
                "Email": "ramdas@rivoligroup.com",
                "label": "FRO00027736",
                "value": "FRO00027736",
                "Country": "ARE",
                "OrgGuid": "92423ed3-d406-446d-bd85-c5ce10a15565",
                "Address1": "1, Sheikh Zayed Road, The H Dubai, Office Tower, Level 23, P.O. Box 121",
                "OrgSource": "CRP",
                "AddressType": 1,
                "CompanyName": "Rivoli Group LLC",
                "OrgFullName": "Rivoli Group LLC",
                "OrgPayables": false,
                "TaxRegNumber": "100223600600003",
                "OrgReceivables": true,
                "SiteIdentifier": "1813816_001_CHTWL_001_B",
                "OrgActiveClient": true,
                "AddressShortCode": "FRO00027736",
                "OrgOrganizationCode": "FRC00026871"
                    }'
    where
        id = 1724542 and tenant_id = 685;
-- Update Shipments
UPDATE shipment_details
    SET
        client_dps_address_id = 117071,
        updated_at = NOW()
    WHERE
        id = 107397
        AND tenant_id = 685;