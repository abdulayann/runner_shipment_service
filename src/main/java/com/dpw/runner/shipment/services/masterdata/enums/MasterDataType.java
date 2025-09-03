package com.dpw.runner.shipment.services.masterdata.enums;

import java.util.Objects;
import lombok.Getter;

@Getter
@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum MasterDataType {
    ORDER_STATUS(1, "OrderStatus"),
    QUOTE_STATUS(2, "QuoteStatus"),
    // BookingStatus(3, "BookingStatus"),
    ORDER_EVENTS(4, "OrderEvents"),
    SERVICE_MODE(5, "ServiceModes"),
    INCOTERMS(6, "IncoTerms"),
    TRANSPORT_MODE(7, "TransportModes"),
    SHIPMENT_TYPE(8, "ShipmentType"),
    WEIGHT_UNIT(9, "WeightUnit"),
    VOLUME_UNIT(10, "VolumeUnit"),
    PACKS_UNIT(11, "PacksUnit"),
    INNERS_UNIT(12, "InnersUnit"),
    ENTRY_DETAILS(13, "EntryDetails"),
    INSPECTIONS(14, "Inspections"),
    RELEASE_TYPE(15, "ReleaseType"),
    AIRWAY_BILL_DIMS(16, "AirwayBillDims"),
    HOUSE_BILL_TYPE(17, "HouseBillType"),
    ON_BOARD(18, "OnBoard"),
    HBL_DELIVERY_MODE(19, "HBLDeliveryMode"),
    CHARGES_APPLY(20, "ChargesApply"),
    EXPORTER_STMT(21, "ExporterStmt"),
    SHIPPER_COD_TYPE(22, "ShipperCODType"),
    SCREENING_STATUS(23, "ScreeningStatus"),
    PHASE(24, "Phase"),
    SPOT_RATE_TYPE(25, "SpotRateType"),
    EFREIGHT_STATUS(26, "efreightstatus"),
    CONTAINER_CATEGORY(27, "containercategory"),
    DIMENSION_UNIT(28, "DimensionUnit"),
    BILL_JOBS(29, "BillJobs"),
    COUNTRIES(30, "Countries"),
    ORG_CATEGORY(31, "OrgCategory"),
    CUSTOM_SHIPMENT_TYPE(32, "CustomShipmentType"),
    CUSTOM_DECL_TYPE(33, "CustomDeclType"),
    CARRIER(34, "Carrier"),
    DOC_TYPES(35, "DocTypes"),
    COUNTRY_STATES(60, "CountryStates"),
    AdditionalServices(61, "AdditionalServices"),
    PAY_TYPES(62, "PayTypes"),
    INVOICE_TYPE(63, "InvoiceType"),
    CONSOlIDATION_TYPE(36, "ConsolidationType"),
    PAYMENT(37, "Payment"),
    ROUTING_STATUS(38, "RoutingStatus"),
    MODE(39, "Mode"),
    PRINT_OPTIONS(40, "PrintOptions"),
    DG_CLASS(41, "DGClass"),
    TEMPERATURE_UNIT(42, "TemperatureUnit"),
    REFERENCE_NUMBER_TYPE(43, "ReferenceNumberType"),
    CONSOLIDATION_ADDRESS_TYPE(44, "ConsolidationAddressType"),
    DROP_MODE(45, "DropMode"),
    MOVE_TYPE(46, "MoveType"),
    HBL_FILING_INFO(47, "HBLFilingInfo"),
    CARRIAGE_TYPE(48, "CarriageType"),
    BOOKING_CHARGES(49, "BookingCharges"),
    CONTAINER_MODE(50, "ContainerMode"),
    ORG_TYPE(51, "OrgType"),
    INSTRUCTION_TYPE(52, "InstructionType"),
    SOURCE_TYPE(53, "SourceType"),
    SERVICE_LEVEL(54, "ServiceLevel"),
            // MAWBType(55, "MAWBType"),
    STOCK_STATUS(56, "StockStatus"),
    QUOTE_TERMS_AND_CONDITIONS(64, "QuoteTemrsAndConditions"),
    FREIGHT_MODE(67, "FreightMode"),
    TRANSPORT_INSTRUCTION_STATUS(66, "TransportInstructionStatus"),
    CHARGE_GROUP(65, "ChargeGroup"),
    TERMS_UNIT(69, "TermsUnit"),
    RECEIPT_METHOD(70, "ReceiptMethod"),
    PAYMENT_METHOD(71, "PaymentMethod"),
    PAYMENT_TYPE(72, "PaymentType"),
    BANK_ACCOUNT_TYPE(73, "BankAccountType"),
    PAYMENT_PROCESS_PROFILE(74, "PaymentProcessProfile"),
    RECEIPT_TYPE(75, "ReceiptType"),
    RECEIPT_STATUS(76, "ReceiptStatus"),
    ACCOUNT_GROUPS(77, "AccountGroups"),
    TERMS_DATE(78, "TermsDate"),
    CONTACT_TYPE(79, "ContactType"),
    HBL_HAWB_BACK_PRINT(80, "HblHawbBackPrint"),
    EMAIL_TEMPLATE_TYPES(81, "EmailTemplateTypes"),
    QUOTE_TYPE(82, "QuoteType"),
    TRUCK_TYPE(83, "TruckType"),
    BL_TYPE(84, "BLType"),
    BL_OBJECT_STATUS(85, "BLObjectStatus"),
    SEQUENCE_GROUPING(86, "SequenceGrouping"),
    SERVICES(87, "Services"),
    ORG_VAT_REGISTRATION_TYPE(88, "OrgVatRegistrationType"),
    PAYMENT_PARAMETERS_TYPE(89, "PaymentParametersType"),
    BE_TYPE(90, "BEType"),
    CUSTOM_CITY(91, "CustomCity"),
    CUSTOM_LOCATION(92, "CustomLocation"),
    CARRIAGE_MODE(93, "CarriageMode"),
    PERU_ENTRY_EXIT_POINT(95, "PeruEntryExitPoint"),
    TIPO_DOCUMENT(96, "TIPODocument"),
    UAE_TAX_RATE(97, "UAETaxRate"),
    UAE_TAX_TREATMENT(98, "UAETaxTreatment"),
    PLACE_OF_SUPPLY(99, "PlaceOfSupply"),
    SHIPMENT_CLONING_FIELDS(100, "ShipmentCloningFields"),
    AIRCRAFT_TYPE(101, "AircraftType"),
    REVENUE_TAX_TYPE(102, "RevenueTaxType"),
    EXPENSE_TAX_TYPE(103, "ExpenseTaxType"),
    CONTAINER_SIZE(104, "ContainerSize"),
    PAYMENT_CODES(105, "PaymentCodes"),
    SPECIAL_HANDLING_CODES(107, "SpecialHandlingCodes"),
    BUSINESS_TYPE(108, "BusinessType"),
    TDS_SECTION(109, "TDSSection"),
    TDS_VENTOR_CATEGORY(110, "TDSVentorCategory"),
    DOOR_STATUS(111, "DoorStatus"),
    ORDER_EVENTS_ESTIMATE_UPDATE(112, "OrderEventsEstimateUpdate"),
    STATEMENT_STATUS(127, "StatementStatus"),
    QUANTITY_CODE(130, "QuantityCode"),
    ACTIVITY_SERVICES(113, "ActivityServices"),
    BROKER_CODE(114, "BrokerCode"),
    KCS_CODE(115, "KcsCode"),
    BL_TERMS_AND_CONDITIONS(116, "BLTermsandConditions"),
    PaymentNumberPattern(117, "PaymentNumberPattern"),
    TAX_NUMBER_PATTERN(118, "TaxNumberPattern"),
    INTEGRATION_STATUS(119, "IntegrationSystems"),
    TERMINATE(120, "Terminals"),
    TRUCK_STATUS(121, "TruckStatus"),
    QUOTE_TEMPLATE_TYPE(122, "QuoteTemplateType"),
    INVOICE_TEMPLATE_TYPE(123, "InvoiceTemplateType"),
    PROFORMA_TYPE(124, "ProformaTemplateType"),
    RESTRICTED_ITEMS_FOR_CREDIT_LIMIT(125, "RestrictedItemsForCreditLimit"),
    DPW_DATE_FORMAT(126, "DPWDateFormat"),
    UNAVAILABLE_CONTAINER(131, "UnavailableContainer"),
    EXEMPTION_SERVICES(132, "ExemptionSection"),
    DPA_MANIFEST_FIELD(133, "DPAManifestField"),
    JOB_SERVICES(134, "JobServices"),
    PAYMENT_TEMPLATE_TYPE(135, "PaymentTemplateType"),
    LG_AREA_VALUES(136, "LGAreaValues"),
    MAWB_CHARGE_TEXT(137, "MAWBChargeText"),
    MAWB_GENERATION(138, "MAWBGeneration"),
    HAWB_GENERATION(139, "HAWBGeneration"),
    BL_CARGO_TERMS(140, "BLCargoTerms"),
    BL_REMARKS(141, "BLRemarks"),
    CONSOLIDATION_CHECK_ORDER(142, "ConsolidationCheckOrder"),
    CONSOL_CHECK_ETD_ETD_THRESHOLD(143, "ConsolcheckETDETDThreshold"),
    AUTO_ATTACH_TRANSPORT(144, "AUTOATTACHTRANSPORT"),
    COMMODITY_GROUP(145, "CommodityGroup"),
    BILL_CHARGES_TEMPLATE(146, "BillChargesTemplate"),
    MODE_OF_BOOKING(147, "ModeOfBooking"),
    CARRIER_BOOKING_EVENTS(148, "CarrierBookingEvents"),
    SI_EVENTS(149, "SIEvents"),
    VGM_EVENTS(150, "VGM Events"),
    HAWB_CARRIER_AGENT(151, "HAWBCarrier/Agent"),
    MAWB_CARRIER_AGENT(152, "MAWBCarrier/Agent"),
    SALES_AGENT_MASTER(1014, "SalesAgentMaster"),
    SECURITY_STATUS(159, "SecurityStatus"),
    EXEMPTION_CODES(160, "ExemptionCodes"),
    SCI(166, "SCI"),
    MODULE_MASTER(183, "ModuleMaster"),
    DEPARTMENT_MASTER_LIST(184, "DepartmentMasterList"),
    NATURE_OF_GOODS(185, "NatureOfGoods"),
    IATA_CHARGE_CODES(186, "IATAChargeCodes"),
    PACKING_GROUP(188, "PackingGroup"),
    LOCATION_ROLE(189, "LocationRole"),
    EVENT_SOURCE(190, "EventSource"),
    PACKAGE_TYPE(191, "PackageType"),
    CONTAINER_TYPE(192, "ContainerType"),
    ORDER_DPW(201, "OrderDPW");

    private int id;
    private String description;

    MasterDataType(int id, String description) {
        this.id = id;
        this.description = description;
    }

    public int getId() {
        return id;
    }

    public static MasterDataType masterData(int id){
        for(MasterDataType d : values()){
            if(d.id == id)
                return d;
        }
        return null;
    }

    public static String getNameFromDescription(String description) {
        if(Objects.isNull(description))
            return "";
        for (MasterDataType masterDataType : MasterDataType.values()) {
            if (masterDataType.getDescription().equalsIgnoreCase(description)) {
                return masterDataType.name();
            }
        }
        return "";
    }
}
