-- Shipment ID : (106808)
-- Update Parties
update parties
    set
        org_code = 'FRC00016303',
        address_code = 'FRO00031623',
        org_id = '147113',
        address_id = '238858',
        updated_at = NOW(),
        org_data = '{
                "Id": 147113,
                "City": "Wels",
                "Guid": "083b390f-a13f-403d-901c-a8ebd55c38ff",
                "Email": "import@xxxlutz.at",
                "IsGSA": false,
                "Phone": "1199999999",
                "label": "XXXLutz KG",
                "value": "FRC00016303",
                "Broker": false,
                "Source": "CRP",
                "Carrier": false,
                "Country": "AUT",
                "Address1": "Romerstrasse 39 ",
                "FullName": "XXXLutz KG",
                "IsActive": 1,
                "Payables": false,
                "Services": false,
                "TenantId": 442,
                "CompanyId": 372,
                "Consignee": true,
                "Consigner": true,
                "IsParnter": false,
                "PANNumber": "FRC00016303",
                "TaxVendor": false,
                "WareHouse": false,
                "AirCarrier": false,
                "ExtraParam": "",
                "InsertDate": "2025-03-28T18:51:51.000",
                "SeaCarrier": false,
                "UpdateDate": "2025-05-05T22:06:07.000",
                "IsSuspended": false,
                "RailCarrier": false,
                "Receivables": false,
                "RoadCarrier": false,
                "ZipPostCode": "4600",
                "ActiveClient": true,
                "CurrencyCode": "EUR",
                "EmailInvoice": false,
                "InsertUserId": 1712,
                "UpdateUserId": 1712,
                "ForworderAgent": false,
                "IsCreditEnabled": false,
                "IsV2PaymentTerm": false,
                "TransportClient": false,
                "IsWalkInCustomer": false,
                "OrganizationCode": "FRC00016303",
                "EnableBulkInvoice": false,
                "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                "IsClientEInvoiceEnabled": false,
                "ImportExportClearanceLock": false,
                "FusionCreditlimitOverrideApproved": false
            }',
    address_data = '{
                "Id": 238858,
                "City": "Wels",
                "Email": "import@xxxlutz.at",
                "label": "FRO00031623",
                "value": "FRO00031623",
                "Country": "AUT",
                "OrgGuid": "083b390f-a13f-403d-901c-a8ebd55c38ff",
                "Address1": "Romerstrasse 39 ",
                "OrgSource": "CRP",
                "AddressType": 2,
                "CompanyName": "XXXLutz KG",
                "OrgFullName": "XXXLutz KG",
                "OrgPayables": false,
                "ZipPostCode": "4600",
                "OrgReceivables": false,
                "OrgActiveClient": true,
                "AddressShortCode": "FRO00031623",
                "OrgOrganizationCode": "FRC00016303"
            }'
    where
        id = 1714204 and tenant_id = 551;
-- Update Shipments
UPDATE shipment_details
    SET
        client_dps_address_id = 99274,
        updated_at = NOW()
    WHERE
        id = 106808
        AND tenant_id = 551;