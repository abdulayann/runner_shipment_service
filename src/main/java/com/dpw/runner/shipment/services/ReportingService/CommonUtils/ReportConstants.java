package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class ReportConstants
{
    public static final String MASTER_BILL = "MasterBill";
    public static final String HOUSE_BILL = "HouseBill";
    public static final String VESSEL_NAME = "VesselName";
    public static final String VOYAGE = "Voyage";
    public static final String POR = "POR";
    public static final String POL = "POL";
    public static final String POD = "POD";
    public static final String FPOD = "FPOD";
    public static final String ITEMS = "Items";
    public static final String POD_COUNTRY = "PODCountry";
    public static final String POL_COUNTRY = "POLCountry";
    public static final String POL_PORTNAME = "POLPortName";
    public static final String POD_PORTNAME = "PODPortName";
    public static final String SHIPMENT_BOOKING_NUMBER = "ShipmentBookingNumber";
    public static final String CARRIER = "Carrier";
    public static final String CARRIER_NAME = "CarrierName";
    public static final String CARRIER_CONTACT_PERSON = "CarrierContactPerson";
    public static final String PORT_OF_LOADING = "PortOfLoading";
    public static final String PORT_OF_DISCHARGE = "PortOfDischarge";
    public static final String PLACE_OF_DELIVERY = "PlaceOfDelivery";
    public static final String FINAL_DESTINATION = "FinalDestination";
    public static final String CARGO_GROSS_VOLUME = "CargoGrossVolume";
    public static final String CARGO_GROSS_VOLUME_UNIT = "CargoGrossVolumeUnit";
    public static final String CARGO_GROSS_WEIGHT = "CargoGrossWeight";
    public static final String CARGO_GROSS_WEIGHT_UNIT = "CargoGrossWeightUnit";
    public static final String CARGO_NET_WEIGHT = "CargoNetWeight";
    public static final String CARGO_NET_WEIGHT_UNIT = "CargoNetWeightUnit";
    public static final String CARGO_GROSS_PACKAGE_COUNT = "CargoGrossPackageCount";
    public static final String CARGO_GROSS_PACKAGE_TYPE = "CargoGrossPackageType";
    public static final String CARGO_GROSS_QUANTITY = "CargoGrossQunatity";
    public static final String CARGO_GROSS_QUANTITY_CODE = "CargoGrossQunatityCode";
    public static final String BL_COMMENTS = "BlComments";

    public static final String ALL_CONTAINERS = "All_Containers";
    public static final String REFERENCE_NUMBER = "ReferenceNumber";
    public static final String ORIGIN_NAME = "OriginName";
    public static final String ORIGIN_COUNTRY = "OriginCountry";
    public static final String ETA = "Eta";
    public static final String ETA_CAPS = "ETA";
    public static final String ETD = "Etd";
    public static final String ETD_CAPS = "ETD";
    public static final String ATA = "Ata";
    public static final String ATD = "Atd";
    public static final String INCO_TERM = "IncoTerm";
    public static final String CHARGEABLE = "Chargeable";
    public static final String CHARGEABLE_UNIT = "ChargableUnit";
    public static final String TRANSPORT_MODE = "TransportMode";
    public static final String TRANSPORT_MODE_DESCRIPTION = "TransportModeDescription";
    public static final String SHIPMENT_TYPE_DESCRIPTION = "ShipmentTypeDescription";
    public static final String SHIPMENT_NUMBER ="ShipmentNumber";
    public static final String SHIPMENT_NO = "ShipmentNo";
    public static final String PACKS = "Packs";
    public static final String PACKS_UNIT = "PacksUnit";
    public static final String GROSS_WEIGHT = "GrossWeight";
    public static final String GROSS_WEIGHT_UNIT = "GrossWeightUnit";
    public static final String GROSS_VOLUME = "GrossVolume";
    public static final String GROSS_VOLUME_UNIT = "GrossVolumeUnit";
    public static final String CLIENT_CONTACT_PERSON = "ClientContactPerson";
    public static final String CLIENT_ADDRESS_1 = "ClientAddress1";
    public static final String CLIENT_ADDRESS_PHONE = "ClientAddressPhone";
    public static final String CLIENT_ADDRESS_MOBILE = "ClientAddressMobile";
    public static final String CLIENT_ADDRESS_CONTACT_PERSON = "ClientAddressContactPerson";
    public static final String CONSIGNEE_CONTACT_PERSON = "ConsigneeContactPerson";
    public static final String CONSIGNER_CONTACT_PERSON = "ConsignorContactPerson";
    public static final String NOTIFY_PARTY_CONTACT_PERSON = "NotifyPartyContactPerson";
    public static final String DELIVERY_CFS = "DeliveryCfs";
    public static final String PICKUP_CFS = "PickUpCfs";
    public static final String PICKUP_TRANSPORT = "PickUpTransport";
    public static final String DELIVERY_AGENT = "DeliveryAgent";
    public static final String MARKS_N_NUMS = "MarksnNums";
    public static final String ORIGINALS = "Originals";
    public static final String ORIGINAL_WORDS = "OriginalsInWords";
    public static final String COPY_BILLS = "CopyBills";
    public static final String NO_OF_PACKAGES = "NoofPackages";
    public static final String ISSUE_PLACE_NAME = "IssuePlaceName";
    public static final String ISSUE_PLACE_COUNTRY = "IssuePlaceCountry";
    public static final String PAID_PLACE_NAME = "PaidPlaceName";
    public static final String PAID_PLACE_COUNTRY = "PaidPlaceCountry";
    public static final String PAID_PLACE_COUNTRY_NAME = "PaidPlaceCountryName";
    public static final String DATE_OF_ISSUE = "DateOfIssue";
    public static final String DATE_TIME = "DateTime";
    public static final String HSN_NUMBER = "HsnNumber";
    public static final String ESTIMATED_READY_FOR_PICKUP = "EstimatedReadyForPickup";
    public static final String DATE_OF_RECEIPT = "DateofReceipt";
    public static final String DESTINATION_NAME_ = "DestinationName";
    public static final String DESTINATION_COUNTRY = "DestinationCountry";
    public static final String SHIPMENT_CONTAINERS = "ShipmentContainers";
    public static final String CONTAINER_COUNT_BY_CODE = "ContainerCountByCode";
    public static final String VESSEL_NAME_AND_VOYAGE = "VesselNameAndVoyage";
    public static final String CONSIGNER = "Consigner";
    public static final String CONSIGNOR = "Consigner";
    public static final String CONSIGNEE = "Consignee";
    public static final String EXPORTER = "Exporter";
    public static final String CONSIGNER_FREETEXT = "ConsignerAddressFreeText";
    public static final String CONSIGNEE_FREETEXT = "ConsigneeAddressFreeText";
    public static final String NOTIFY_PARTY_FREETEXT = "NotifyPartyAddressFreeText";
    public static final String CONSIGNER_NAME = "ConsignerName";
    public static final String CONSIGNEE_NAME = "ConsigneeName";
    public static final String CLIENT = "Client";
    public static final String CLIENT_NAME = "ClientName";
    public static final String CONSIGNEE_LOCAL_NAME = "ConsigneeLocalName";
    public static final String CONSIGNER_LOCAL_NAME = "ConsignerLocalName";
    public static final String NOTIFY_PARTY_NAME = "NotifyPartyName";
    public static final String NOTIFY_PARTY_LOCAL_NAME = "NotifyPartyLocalName";
    public static final String COMPANY_NAME = "CompanyName";
    public static final String ADDRESS1 = "Address1";
    public static final String STATE = "State";
    public static final String COUNTRY = "Country";
    public static final String ZIPCODE = "Zip";
    public static final String NOTIFY_PARTY = "NotifyParty";
    public static final String OPEN_DATE = "OpenDate";
    public static final String DUE_DATE = "DueDate";
    public static final String PP_CC = "PP_CC";
    public static final String PPCC = "PPCC";
    public static final String FLIGHT_NAME = "FlightName";
    public static final String FLIGHT_NUMBER = "FlightNo";
    public static final String DATE_OF_DEPARTURE = "DateOfDeparture";
    public static final String DELIVERY_ADDRESS = "DeliveryAddress";
    public static final String SYSTEM_DATE = "SysDate";
    public static final String CURRENT_DATE = "CurrentDate";
    public static final String ONBOARD_DATE = "OnBoardDate";
    public static final String PLACE_OF_RECEIPT = "PlaceOfReceipt";
    public static final String DESCRIPTION = "Description";
    public static final String WAREHOUSE_NAME = "WarehouseName";
    public static final String FLIGHT_CARRIER = "FlightCarrier";
    public static final String ADDITIONAL_TERMS = "AdditionalTerms";
    public static final String NOTES = "Notes";
    public static final String CMS_RELEASE_NO = "ReleaseNo";
    public static final String CMS_REMARKS = "Remarks";
    public static final String CMS_PICK_CY = "PickUpCy";
    public static final String PRE_CARRIAGE = "PreCarriage";
    public static final String INVNO = "INVNO";
    public static final String EXPORTER_TAX_ID = "ExporterTaxID";
    public static final String CONSIGNEE_TAX_ID = "ConsigneeTaxID";
    public static final String AIRWAY_BILL_NUMBER = "AirwaybillNumber";
    public static final String SHIP_DATE = "ShipDate";
    public static final String WEIGHT = "Weight";
    public static final String WEIGHT_UNIT = "WeightUnit";
    public static final String TOTAL_PACKS = "TotalPacks";
    public static final String UOTW = "UOTW";
    public static final String PACKAGE_TYPE = "PackageType";
    public static final String SPECIAL_INSTRUCTION = "SpecialInstructions";


        /*
            AR Object Fields
        */
    public static final String INVOICE_NUMBER = "InvoiceNumber";
    public static final String BILLCHARGES_LOCAL_SUM = "BillChargesLocalSum";
    public static final String BILLCHARGES_LOCAL_TAX_SUM = "BillChargesLocalTaxSum";
    public static final String BILLCHARGES_OVERSEAS_SUM = "BillChargesOverseasSum";
    public static final String INVOICE_CURRENCY = "InvoiceCurrency";
    public static final String TOTAL_BILL_AMOUNT_WORDS_LOCAL = "TotalbillamountWordsLocal";
    public static final String TOTAL_BILL_AMOUNT_WORDS_OVERSEAS = "TotalbillamountWordsOverseas";
    public static final String CUMULATIVE_SUM_LOCAL = "CumulativeSumLocal";
    public static final String EXCHANGE_RATE_OVERSEA = "ExchangeRateOversea";
    public static final String TOTAL_REVENUE_AMOUNT = "TotalRevenueAmount";
    public static final String TOTAL_LOCAL_SELL_AMOUNT = "TotalLocalSellAmount";

        /*
            Consolidation Fields
        */
    public static final String BOOKING_NO = "BookingNo";
    public static final String BOOKING_NUMBER = "BookingNumber";
    public static final String CARGO_CLOSING_TIME = "CargoClosingTime";
    public static final String DOCS_CLOSING_TIME = "DocsClosingTime";
    public static final String EXPORT_AGENT = "ExportAgent";
    public static final String IMPORT_AGENT = "ImportAgent";
    public static final String RECEIVING_AGENT_NAME = "ReceivingAgentName";
    public static final String RECEIVING_AGENT_LOCAL_NAME = "ReceivingAgentLocalName";
    public static final String SENDING_AGENT_NAME = "SendingAgentName";
    public static final String SENDING_AGENT_LOCAL_NAME = "SendingAgentLocalName";
    public static final String CONSOL_VESSEL_NAME = "ConsolVesselName";
    public static final String CONSOL_VOYAGE = "ConsolVoyage";
    public static final String CREDITOR = "Creditor";
    public static final String EXPORT_AGENT_FREETEXT = "SendingAgentAddressFreeText";
    public static final String IMPORT_AGENT_FREETEXT = "ReceivingAgentAddressFreeText";
    public static final String CREDITOR_LOCAL_NAME = "CreditorLocalName";
    public static final String CONSOL_CARRIER = "ConsolCarrierName";
    public static final String CONSOL_ETA = "ConsolEta";
    public static final String CONSOL_ETD = "ConsolEtd";
    public static final String CONSOL_ATA = "ConsolAta";
    public static final String CONSOL_ATD = "ConsolAtd";
    public static final String CONSOL_DATE_OF_DEPARTURE = "ConsolDateOfDeparture";
    public static final String CONSOL_REFERENCE_NUMBER = "ConsolReferenceNumber";
    public static final String LAST_FOREIGN_PORT = "LastForeignPort";
    public static final String LAST_FORGEIN_PORT_COUNTRY = "LastForeignPortCountry";
    public static final String DEST_PORT_NAME = "DestPortName";
    public static final String DEST_PORT_COUNTRY = "DestPortCountry";
    public static final String MSN_NUMBER = "MsnNumber";
    public static final String MRN_NUMBER = "MrnNumber";
    public static final String MASTER_BILL_ISSUE_DATE = "MasterBillIssueDate";
    public static final String MASTER_BILL_ISSUE_PLACE = "MasterBillIssuePlace";
    public static final String CONSOL_ADDITIONAL_TERMS = "ConsolAdditionalTerms";
    public static final String CONSOL_FLIGHT_NUMBER = "ConsolFlightNo";
    public static final String CONSOL_NOTIFY_ADDRESS = "ConsolNotifyAddress";


        /*
            Tenant fields
        */
    public static final String TENANT = "Tenant";
    public static final String TENANT_NAME = "TenantName";
    public static final String TENANT_ADDRESS_1 = "TenantAddress1";
    public static final String TENANT_ADDRESS_2 = "TenantAddress2";
    public static final String TENANT_EMAIL = "TenantEmail";
    public static final String TENANT_CITY = "TenantCity";
    public static final String TENANT_STATE = "TenantState";
    public static final String TENANT_FAX = "TenantFax";
    public static final String TENANT_COUNTRY = "TenantCountry";
    public static final String TENANT_COUNTRY_PHONE = "TenantContactPhone";
    public static final String TENANT_MOBILE = "TenantMobile";
    public static final String TENANT_ZIP_POST_CODE = "TenantZipPostCode";
    public static final String TENANT_CURRENCY = "TenantCurrency";
    public static final String TENANT_URL = "TenantUrl" ;
    public static final String TENANT_VATREGNUMBER = "VatRegNumber" ;

        /*
            User fields
        */
    public static final String USER_FULLNAME = "UserFullName";
    public static final String USER_NAME = "UserName";
    public static final String USER_EMAIL = "UserEmail";


    public static final String CURRENCY_SUMMARY = "CurrencySummary";
    public static final String REVENUE_BILL_CHARGES = "RevenueBillCharges";
    public static final String DESTINATION = "Destination";
    public static final String DEBTOR_ADDRESS = "DebtorAddress";
    public static final String OVERSEAS_CURRENCY = "OverseasCurrency";
    public static final String LOCAL_CURRENCY = "LocalCurrency";
    public static final String SHIPMENT_AND_CONTAINER = "ShipmentAndContainer";
    public static final String SERVICE_MODE_DESCRIPTION = "ServiceModeDesc";
    public static final String SHIPMENTS = "Shipments";
    public static final String CONTAINER_COUNT = "ContainerCount";
    public static final String CONTAINER_AGGREGATION_STRING = "ContainerAggregation";
    public static final String SHIPMENT_COUNT = "ShipmentCount";
    public static final String PICKUP_TRANSPORT_COMPANY = "PickUpTransportCompany";
    public static final String PICKUP_TRANSPORT_CONTACT_PERSON = "PickUpTransportContactPerson";
    public static final String LOADING_PORT_NAME = "LoadingPortName";
    public static final String LOADING_PORT_COUNTRY = "LoadingPortCountry";
    public static final String DISCHARGE_PORT_NAME = "DischargePortName";
    public static final String DISCHARGE_PORT_COUNTRY = "DischargePortCountry";
    public static final String CONSOL_CONTAINERS = "ConsolContainers";
    public static final String YYYY_MM_DD_FORMAT = "yyyy/MM/dd";
    public static final String BL_VESSEL_NAME = "BL_VesselName";
    public static final String BL_VOYAGE = "BL_Voyage";
    public static final String BL_NOTIFY_PARTY = "BL_NotifyParty";
    public static final String TOTAL_WEIGHT = "TotalPacksWeight";
    public static final String TOTAL_VOLUME = "TotalPacksVolume";
    public static final String TOTAL_WEIGHT_UNIT = "TotalPacksWeightUnit";
    public static final String TOTAL_VOLUME_UNIT = "TotalPacksVolumeUnit";
    public static final String AS_AGREED = "asAgreed";
    public static final String COPY_AS_AGREED = "copyAsAgreed";
    public static final String HAS_CHARGES = "hasCharges";
    public static final String CHARGES = "Charges";
    public static final String COPY_CHARGES = "copyCharges";
    public static final String BILL_REMARKS = "BillRemark";

    public static final String Cargo_Total_Information = "BillRemark";
    public static final String PP_REVENUE_TOTAL = "PrepaidRevenueTotal";
    public static final String CC_REVENUE_TOTAL = "CollectRevenueTotal";
    public static final String AT = "At";
    public static final String SHIPMENT_IDS = "shipmentIds";
    public static final String SHIPMENT_ID = "ShipmentId";
    public static final String SHIPMENT_TYPE = "ShipmentType";
    public static final String CUSTOM_SHIPMENT_TYPE = "CustomShipmentType";
    public static final String BLComments = "Bl_Comments";
    public static final String BL_DELIVERY_AGENT = "BL_DeliveryAgent";
    public static final String ISSUEPLACECOUNTRYNAME = "IssuePlaceCountryName";

        /*
            Truck Driver Details fields
        */

    public static final String TRUCK_NUMBER_PLATE = "TruckNumberPlate";
    public static final String TRAILER_NUMBER_PLATE = "TrailerNumberPlate";
    public static final String TRUCK_OR_TRAILER_TYPE = "TruckOrTrailerType";
    public static final String CONTAINER_NUMBER = "ContainerContainerNumber";
    public static final String CONTAINER_TYPE = "ContainerTypeCode";
    public static final String DRIVER_NAME = "DriverName";
    public static final String DRIVER_MOBILE_NUMBER = "DriverMobileNumber";
    public static final String TRANSPORTER_NAME = "TransporterName";

    /*
        Report Constants
     */
    public static final String ARRIVAL_NOTICE = "ArrivalNotice";
    public static final String DELIVERY_ORDER = "DeliveryOrder";
    public static final String BOOKING_CONFIRMATION = "BookingConfirmation";
    public static final String HBL = "Hbl";
    public static final String HAWB = "Hawb";
    public static final String MAWB = "Mawb";

    public static final String BL_NOTIFY_PARTY_CAPS = "BL_NotifyPartyInCaps";
    public static final String CONSIGNER_CAPS = "ConsignerInCaps";
    public static final String CONSIGNEE_CAPS = "ConsigneeInCaps";
    public static final String MARKS_N_NUMS_CAPS = "MarksnNumsInCaps";

    public static final String PACKS_UNIT_DESC = "PacksUnitDescription";

    public static final String DESCRIPTION_CAPS = "DescriptionInCaps";
    public static final String POL_CODE = "POLCode";
    public static final String POD_CODE = "PODCode";
    public static final String BL_DELIVERY_AGENT_ADDRESS = "BL_DeliveryAgentAddress";
    public static final String BL_CARGO_TERMS_DESCRIPTION = "BLCargoTermsDescription";
    public static final String BL_REMARKS_DESCRIPTION = "BLRemarksDescription";

    //HAWB
    public static final String AGENT = "Agent";
    public static final String SHIPPER_ADDRESS = "ShipperAddress";
    public static final String CONSIGNEE_ADDRESS = "ConsigneeAddress";
    public static final String ISSUING_CARRIER_AGENT_NAME = "IssuingCarrierAgentName";
    public static final String ISSUiNG_CARRIER_CITY = "IssuingCarrierCity";
    public static final String AGENT_IATA_CODE = "AgentIATACode";
    public static final String CASSCODE = "CassCode";
    public static final String FIRST_CARRIER = "FirstCarrier";
    public static final String JOB_NUMBER = "JobNumber";
    public static final String MAWB_NO = "MAWBNo";
    public static final String NEUTRAL_AWB_NO = "NeutralAWBNo";
    public static final String HAWB_NO = "HAWBNo";
    public static final String PRINT_USER_NAME = "PrintUserName";
    public static final String MAWB_REMAINING = "MAWBRemaining";
    public static final String SPECIAL_HANDLING_CODE = "SpecialHandlingCodes";
    public static final String OTHER_CHARGES_IATA_OAT = "OtherChargesIATAOAT";
    public static final String OTHER_CHARGES_OAT = "OtherChargesOAT";
    public static final String NEW_OTHER_CHARGES_IATA = "NewOtherChargesIATA";
    public static final String OTHER_CHARGES_IATA = "OtherChargesIATA";
    public static final String NEW_OTHER_CHARGES = "NewOtherCharges";
    public static final String OTHER_CHARGES = "OtherCharges";
    public static final String SIGN_OF_ISSUING_CARRIER = "SignOfIssuingCarrier";
    public static final String SIGN_OF_SHIPPER = "SignOfShipper";
    public static final String EXECUTED_ON = "ExecutedOn";
    public static final String EXECUTED_AT_NAME = "ExecutedAtName";
    public static final String EXECUTED_AT = "ExecutedAt";
    public static final String MAWB_NUMBER = "mawbnumber";
    public static final String OTHER_AMOUNT_TEXT_C = "OtherAmountText_C";
    public static final String TOTAL_OTHERS_C  = "TotalOthers_C";
    public static final String CARRIER_DUE_C = "CarrierDue_C";
    public static final String AGENT_DUE_C = "AgentDue_C";
    public static final String OTHERS_C = "OthersC";
    public static final String OTHER_AMOUNT_TEXT_P = "OtherAmountText_P";
    public static final String TOTAL_OTHERS_P = "TotalOthers_P";
    public static final String CARRIER_DUE_P = "CarrierDue_P";
    public static final String AGENT_DUE_P = "AgentDue_P";
    public static final String OTHERS_P = "OthersP";
    public static final String FREIGHT_AMOUNT_TEXT_C = "FreightAmountText_C";
    public static final String TOTAL_FREIGHT_C = "TotalFreight_C";
    public static final String TAX_C = "Tax_C";
    public static final String VALUATION_CHARGES_C = "ValuationCharges_C";
    public static final String WT_CHARGE_C = "WtChargeC";
    public static final String WTVALC = "WTVALC";
    public static final String FREIGHT_AMOUNT_TEXT_P = "FreightAmountText_P";
    public static final String TOTAL_FREIGHT_P = "TotalFreight_P";
    public static final String TAX_P = "Tax_P";
    public static final String VALUATION_CHARGES_P = "ValuationCharges_P";
    public static final String WT_CHARGE_P = "WtChargeP";
    public static final String WTVALP = "WTVALP";
    public static final String WT_CHARGE_CFAT = "WtChargeCFAT";
    public static final String CARRIER_DUE_POAT = "CarrierDue_POAT";
    public static final String AGENT_DUE_POAT = "AgentDue_POAT";
    public static final String TOTAL_PREPAID = "TotalPrepaid";
    public static final String TOTAL_COLLECT = "TotalCollect";
    public static final String FLIGHT_NO = "FlightNo";
    public static final String FLIGHT_DATE = "FlightDate";
    public static final String FLIGHT_NO3 = "FlightNo3";
    public static final String FLIGHT_DATE3 = "FlightDate3";
    public static final String BY_THIRD = "ByThird";
    public static final String TO_THIRD = "ToThird";
    public static final String FLIGHT_NO2 = "FlightNo2";
    public static final String FLIGHT_DATE2 = "FlightDate2";
    public static final String BY_SECOND = "BySecond";
    public static final String TO_SECOND = "ToSecond";
    public static final String FLIGHT_NO1 = "FlightNo1";
    public static final String FLIGHT_DATE1 = "FlightDate1";
    public static final String BY_FIRST = "ByFirst";
    public static final String TO_FIRST = "ToFirst";
    public static final String BY = "By";
    public static final String TO = "To";
    public static final String ISSUED_BY_NAME = "IssuedByName";
    public static final String ISSUED_BY = "IssuedBy";
    public static final String AO_DEPT_CODE = "AODeptCode";
    public static final String SUM_OF_CHARGEABLE_WT = "SumOfChargeableWt";
    public static final String SUM_OF_TOTAL_AMOUNT_FAT = "SumOfTotalAmountFAT";
    public static final String SUM_OF_TOTAL_AMOUNT = "SumOfTotalAmount";
    public static final String TGW = "TGW";
    public static final String TOTAL_GROSS_WEIGHT = "TotalGrossWeight";
    public static final String TOtAl_PIECES = "TotalPieces";
    public static final String PACKING_LIST_FAT = "PackingListFAT";
    public static final String TOTAL_AMOUNT = "TotalAmount";
    public static final String RATE_CHARGE = "RateCharge";
    public static final String CHARGEABLE_WT = "ChargeableWt";
    public static final String GROSS_WT = "GrossWt";
    public static final String RATE_CLASS = "RateClass";
    public static final String NATURE_QLTY_OF_GOODS = "NatureQltyOfGoods";
    public static final String PACKING_LIST = "PackingList";
    public static final String OTHER_AMOUNT_TEXT= "OtherAmountText";
    public static final String FREIGHT_AMOUNT_TEXT = "FreightAmountText";
    public static final String DESTINATION_AIRPORT_COUNTRY = "DestinationAirportCountry";
    public static final String DEPARTURE_AIRPORT_COUNTRY = "DepartureAirportCountry";
    public static final String PAYMENT_TERMS = "PaymentTerms";
    public static final String AIRPORT_OF_DESTINATION = "AirportOfDestination";
    public static final String AIRPORT_OF_DEPARTURE = "AirportOfDeparture";
    public static final String SCI = "SCI";
    public static final String NATURE_OF_GOODS = "NatureOfGoods";
    public static final String HANDLING_INFORMATION = "HandlingInformation";
    public static final String ACCOUNTING_INFORMATION = "AccountingInformation";
    public static final String CURRENCY = "Currency";
    public static final String CHARGE_CODE = "ChargeCode";
    public static final String DECLARED_VALUE_FOR_CUSTOMS = "DeclaredValueForCustoms";
    public static final String DECLARED_VALUE_FOR_CARRIAGE = "DeclaredValueForCarriage";
    public static final String AMOUNT_OF_INSURANCE = "AmountOfInsurance";
    public static final String AOI = "AOI";
    public static final String OTHER_INFORMATION = "OtherInformation";
    public static final String OPTIONAL_SHIPPING_INFORMATION_OTHER = "OptionalShippingInformationOther";
    public static final String OPTIONAL_SHIPPING_INFORMATION = "OptionalShippingInformation";

    public static final String PAYMENTS = "Payments";
    public static final String PORT_OF_DEPARTURE = "PortofDeparture";
    public static final String PORT_OF_DEPARTURE_COUNTRY = "PortofDepartureCountry";
    public static final String PORT_OF_ARRIVAL = "PortofArrival";
    public static final String PORT_OF_ARRIVAL_COUNTRY = "PortofArrivalCountry";
    public static final String MOVEMENT_TYPE = "MovementType";
    public static final String BOOKING_NO_BASED_ON_TYPE = "BookingNoBasedOnType";
    public static final String MOTHER_REFERENCE_NO = "MotherReferenceNo";
    public static final String FEEDER_REFERENCE_NO = "FeederReferenceNo";

    public static final Set OBJECT_TYPE_REPORTS = new HashSet(Arrays.asList("ImportConsolManifest", "ExportConsolManifest", "ImportShipmentManifest", "ExportShipmentManifest"));

    public static final String OBJECT_TYPE = "OBJECT_TYPE";
    public static final String AWB_LABLE = "AWBLable";
    public static final String COMMERCIAL_INVOICE = "CommercialInvoice";
    public static final String CUSTOMS_INSTRUCTION = "CustomsInstructions";
    public static final String SEAWAY_BILL = "SeawayBill";
    public static final String SHIP_TRUCKWAY_BILL = "ShipTruckwayBill";
    public static final String CONS_TRUCKWAY_BIll = "ConsTruckwayBill";
    public static final String SHIP_TRUCK_DRIVER_PROOF = "ShipTruckDriverProof";
    public static final String CONS_TRUCK_DRIVER_PROOF = "ConsTruckDriverProof";
    public static final String SHIPMENT_CAN_DOCUMENT = "CAN";
    public static final String CUSTOMS_INSTRUCTIONS = "CustomsInstructions";
    public static final String AIRWAY_BILL = "AirwayBill";
    public static final String SHIPMENT_HOUSE_BILL = "HouseBill";
    public static final String FREIGHT_CERTIFICATION = "FreightCertification";
    public static final String PRE_ALERT = "PreAlert";
    public static final String PROOF_OF_DELIVERY = "ProofOfDelivery";
    public static final String PICKUP_ORDER = "PickupOrder";
    public static final String CSR = "CSR";
    public static final String SHIPPING_REQUEST = "ShippingRequest";
    public static final String SHIPPING_REQUEST_AIR = "ShippingRequestAir";
    public static final String IMPORT_SHIPMENT_MANIFEST = "ImportShipmentManifest";
    public static final String EXPORT_SHIPMENT_MANIFEST = "ExportShipmentManifest";
    public static final String IMPORT_CONSOL_MANIFEST = "ImportConsolManifest";
    public static final String EXPORT_CONSOL_MANIFEST = "ExportConsolManifest";
    public static final String AWB_NEUTRAL = "AWBNeutral";
    public static final String CARGO_MANIFEST = "CargoManifest";
    public static final String CONSOLIDATED_PACKING_LIST = "ConsolidatedPackingList";
    public static final String COSTAL_DOC = "COSTALdoc";
    public static final String SHIPPING_INSTRUCTION = "ShippingInstruction";
    public static final Set NEW_TEMPLATE_FLOW = new HashSet(Arrays.asList("ShipTruckwayBill", "ConsTruckwayBill", "ShipTruckDriverProof", "ConsTruckDriverProof", "ShippingRequest", "ShippingRequestAir", "ExportShipmentManifest", "ImportShipmentManifest", "ExportConsolManifest", "ImportConsolManifest"));


    public static final String TRANS_AIR = "AIR";
    public static final String AIR = "AIR";
    public static final String SEA = "SEA";
    public static final String EXP = "EXP";

    public static final String ORIGINAL = "ORIGINAL";
    public static final String SURRENDER = "SURRENDER";
    public static final String NEUTRAL = "NEUTRAL";
    public static final String DRAFT = "DRAFT";
    public static final String PRINTED_ORIGINAL = "PrintedOriginal";
    public static final String HOUSE_BILL_TYPE = "HouseBillType";
    public static final String COUNT = "Count";
    public static final String HAWB_NUMBER = "HAWBNumber";
    public static final String PRINTING_FOR = "PrintingFor";
    public static final String LOGO = "Logo";
    public static final String INCREMENTAL_ORIGINALS = "IncrementalOriginals";
    public static final String ORIGINAL_OR_COPY = "OrginalOrCopy";
    public static final String COPY = "COPY";
    public static final String DISABLE_ORIGINAL = "DisableOriginal";
    public static final String ID = "Id";
    public static final String PURCHASE_ORDER_NUMBER = "PurchaseOrderNumber";

    // APis
    public static final String REPORT_API_HANDLE = "/api/v2/report";
    public static final String REPORT_CREATE_SUCCESSFUL = "Successful Report Creation";
}