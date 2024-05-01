package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class Constants {
    private Constants(){}
    public static final int TRANSPORT_MODE_INDEX = 0;
    public static final String DIRECTION = "direction";
    public static final String SHIPMENT_TYPE = "shipmentType";
    public static final String CONTAINER_CATEGORY = "containerCategory";
    public static final int DIRECTION_INDEX = 1;
    public static final int SHIPMENT_TYPE_INDEX = 2;

    public static final String IS_DOMESTIC = "isDomestic";
    public static final String DOMESTIC = "domestic";
    public static final String INTERNATIONAL = "international";
    public static final int IS_DOMESTIC_INDEX = 3;
    public static final String DELIMITER = "-";
    public static final String SHIPMENT_LIST_PERMISSION = "ShipmentList";
    public static final String SHIPMENT_RETRIEVE_PERMISSION = "ShipmentRetrive";
    public static final String SHIPMENT_CREATE_PERMISSION = "ShipmentCreate";
    public static final String SHIPMENT_UPDATE_PERMISSION = "ShipmentUpdate";
    public static final String CONSOLIDATION_LIST_PERMISSION = "ConsolidationList";
    public static final String CONSOLIDATION_RETRIEVE_PERMISSION = "ConsolidationRetrive";
    public static final String CONSOLIDATION_CREATE_PERMISSION = "ConsolidationCreate";
    public static final String CONSOLIDATION_UPDATE_PERMISSION = "ConsolidationUpdate";

    public static final String ALL = "all";
    public static final String BROWSER_TIMEZONE = "browser-timezone";
    public static final String USER_SERVICE_V1 = "v1";
    public static final String USER_SERVICE_MAVANI = "mavani";
    public static final String NO_DATA = "Not Found!";
    public static final String SHIPMENT = "SHIPMENT";
    public static final String Shipment = "Shipment";
    public static final String Consolidation = "Consolidation";
    public static final String CUSTOMER_BOOKING = "CustomerBooking";
    public static final String BOOKING = "BOOKING";
    public static final String BOOKING_CHARGES = "BOOKING_CHARGES";
    public static final String CONSOLIDATION = "CONSOLIDATION";
    public static final String TENANT_SETTINGS = "TENANT_SETTINGS";
    public static final String ROUTING = "ROUTING";
    public static final String PACKING = "PACKING";
    public static final String MPK = "MPK";
    public static final String CONTAINER = "CONTAINER";
    public static final String CARRIAGE = "CARRIAGE";
    public static final String PreCarriage = "PreCarriage";
    public static final String SERVICE_DETAILS = "SERVICE_DETAILS";
    public static final String NOTES = "NOTES";
    public static final String Main = "Main";
    public static final String EVENTS = "EVENTS";
    public static final String JOBS = "JOBS";
    public static final String Cont20 = "20";
    public static final String Cont40 = "40";
    public static final String Cont20GP = "20GP";
    public static final String Cont20RE = "20RE";
    public static final String Cont40GP = "40GP";
    public static final String Cont40RE = "40RE";
    public static final String PageNumberError = "Page Number should be Greater than or equal to 1";
    public static final String Validation_Exception = "Validation errors occurred.";
    public static final String MASS = "Mass";
    public static final String VOLUME = "Volume";
    public static final String LENGTH = "Length";
    public static final String VOLUME_UNIT_M3 = "M3";
    public static final String VOLUME_UNIT_LITRE = "L";
    public static final String VOLUME_UNIT_CC = "CC";
    public static final String VOLUME_UNIT_CM = "CM";
    public static final String VOLUME_UNIT_AF = "AF";
    public static final String VOLUME_UNIT_Barrel_OIL = "Barrel (oil)";
    public static final String VOLUME_UNIT_Board_foot = "Board foot";
    public static final String VOLUME_UNIT_Bushel_US = "Bushel (US)";
    public static final String VOLUME_UNIT_Cup = "Cup";
    public static final String VOLUME_UNIT_Fluid_OUNCE_US = "Fluid ounce (US)";
    public static final String VOLUME_UNIT_CF = "CF";
    public static final String VOLUME_UNIT_GI = "GI";
    public static final String VOLUME_UNIT_GA = "GA";
    public static final String VOLUME_UNIT_Gallon_US_LIQ = "Gallon (US,liq)";
    public static final String VOLUME_UNIT_Gill_UK = "Gill (UK)";
    public static final String VOLUME_UNIT_Gill_US = "Gill (US)";
    public static final String VOLUME_UNIT_CI = "CI";
    public static final String VOLUME_UNIT_Liter_OLD = "Liter (old)";
    public static final String VOLUME_UNIT_Ounce_UK_FLD = "Ounce (UK,fluid)";
    public static final String VOLUME_UNIT_Ounce_US_FLD = "Ounce (US,fluid)";
    public static final String VOLUME_UNIT_Peck_US = "Peck (US)";
    public static final String VOLUME_UNIT_Pint_US_DRY = "Pint (US,dry)";
    public static final String VOLUME_UNIT_Pint_US_LIQ = "Pint (US,liq)";
    public static final String VOLUME_UNIT_Quart_US_DRY = "Quart (US,dry)";
    public static final String VOLUME_UNIT_Quart_US_LIQ = "Quart (US,liq)";
    public static final String VOLUME_UNIT_Stere = "Stere";
    public static final String VOLUME_UNIT_Tablespoon = "Tablespoon";
    public static final String VOLUME_UNIT_Teaspoon = "Teaspoon";
    public static final String VOLUME_UNIT_TON_REGISTER = "Ton (register)";
    public static final String VOLUME_UNIT_CY = "CY";
    public static final String VOLUME_UNIT_ML = "ML";
    public static final String VOLUME_UNIT_D3 = "D3";
    public static final String VOLUME_UNIT_CBM = "CBM";
    public static final String WEIGHT_UNIT_KG = "KG";
    public static final String WEIGHT_UNIT_GRAM = "G";
    public static final String WEIGHT_UNIT_MG = "MG";
    public static final String WEIGHT_UNIT_UG = "UG";
    public static final String WEIGHT_UNIT_MC = "MC";
    public static final String WEIGHT_UNIT_HUNDRED_WT_LONG = "Hundredweight (long)";
    public static final String WEIGHT_UNIT_HUNDRED_WT_SHORT = "Hundredweight (short)";
    public static final String WEIGHT_UNIT_LB = "LB";
    public static final String WEIGHT_UNIT_LT = "LT";
    public static final String WEIGHT_UNIT_OZ = "OZ";
    public static final String WEIGHT_UNIT_OT = "OT";
    public static final String WEIGHT_UNIT_Slug = "Slug";
    public static final String WEIGHT_UNIT_TA = "TA";
    public static final String WEIGHT_UNIT_TL = "TL";
    public static final String WEIGHT_UNIT_TN = "TN";
    public static final String WEIGHT_UNIT_TM = "TM";
    public static final String WEIGHT_UNIT_T = "T";
    public static final String WEIGHT_UNIT_HG = "HG";
    public static final String WEIGHT_UNIT_KT = "KT";
    public static final String WEIGHT_UNIT_DT = "DT";

    public static final String TRANSPORT_MODE_SEA = "SEA";
    public static final String TRANSPORT_MODE_RF = "RF";
    public static final String TRANSPORT_MODE_AIR = "AIR";
    public static final String TRANSPORT_MODE_ROA = "ROA";
    public static final String TRANSPORT_MODE_RAI = "RAI";
    public static final String TRANSPORT_MODE_FSA = "FSA";
    public static final String TRANSPORT_MODE_FAS = "FAS";
    public static final String SHIPMENT_TYPE_LCL = "LCL";
    public static final String SHIPMENT_TYPE_LSE = "LSE";
    public static final String CARGO_TYPE_FCL = "FCL";
    public static final String DIRECTION_EXP = "EXP";
    public static final String JOB_TYPE_CLB = "CLB";
    public static final String DIRECTION_IMP = "IMP";

    //MasterDataFactory
    public static final String MAPPER_MASTER_DATA = "Mapper";

    public static final String INVNO = "INVNO";
    public static final String CON = "CON";

    /**
     * Statuses
     */
    public static final String PENDING = "Pending";
    public static final String V1_MASTER_DATA = "v1";


    public static final String TOKEN = "Authorization";

    public static final String DMAWB = "DMAWB";
    public static final String HAWB = "HAWB";
    public static final String MAWB = "MAWB";
    public static final String MBL = "MBL";

    public static final String FORWARDING_AGENT = "Forwarding Agent";
    public static final String FAG = "FAG";

    public static final String DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB = "CONSOLIDATED CARGO AS PER MANIFEST ATTACHED";
    public static final double FACTOR_VOL_WT = 166.667;

    /**
     * Enum Constants
     **/

    public static final String SHIPMENT_STATUS = "ShipmentStatus";
    public static final String BOOKING_STATUS = "BookingStatus";

    public static final String MASTER_LIST = "MasterList";
    public static final String OWNERSHIP = "OwnerShip";

    public static final String SERVICE = "Service";
    public static final String TI_TEMPLATE_TYPE = "TransportInstructionTemplateType";
    public static final String  CUSTOMER_CATEGORY_RATES = "CustomerCategoryRates";
    public static final String CARRIER_BOOKING_STATUS = "CarrierBookingStatus";
    public static final String RA_KC_TYPE = "RAKCType";

    /**
     * Events
     **/

    public static final String INVGNTD = "INVGNTD";
    public static final String TAXSG = "TAXSG";
    public static final String CSEDI = "CSEDI";
    public static final String AMSEDI = "AMSEDI";
    public static final String SHPCNFRM = "SHPCNFRM";
    public static final String SHPCMPLT = "SHPCMPLT";
    public static final String CONCRTD = "CONCRTD";
    public static final String SHPCRTD = "SHPCRTD";
    public static final List<String> ATD_EVENT_CODES = List.of("VESSELDEPARTUREWITHCONTAINER", "VSDPR");
    public static final List<String> ATA_EVENT_CODES = List.of("VESSELARRIVALWITHCONTAINER", "VSARV");


    public static final String COMMODITY_TYPE_MASTER_DATA = "CommodityType";
    public static final String WARE_HOUSE_DATA = "WareHouse";
    public static final String DG_SUBSTANCE = "DGSubstance";
    public static final String CONTAINER_TYPE_MASTER_DATA = "ContainerType";
    public static final String CURRENCY_MASTER_DATA = "Currency";
    public static final String VESSEL_MASTER_DATA = "Vessel";
    public static final String CARRIER_MASTER_DATA = "Carrier";
    public static final String CHARGE_TYPE_MASTER_DATA = "ChargeType";
    public static final String CUSTOM_CITY = "customCity";
    public static final String ACTIVITY_TYPE = "ActivityType";
    public static final String SALES_AGENT = "SalesAgent";


    public static final String DEFAULT_DATE_FORMAT = "MM/dd/yyyy";
    public static final String CONSOLIDATION_TYPE_CLD = "CLD";

    public static final String CONSOLIDATION_TYPE_AGT = "AGT";

    public static final String AGENT_PREFIX = "A";
    public static final String CARRIER_PREFIX = "C";

    public static final String CARGO_RUNNER = "CargoRunner";

    public static final String Shipments = "[Shipments]";
    public static final String Consolidations = "[Consolidations]";

    public static final String IMP = "IMP";
    public static final String SHIPMENT_TYPE_DRT = "DRT";

    public static final String METRIC_TON = "MT";
    public static final String METRE = "M";
    public static final String CENTI = "CM";
    public static final String DECI = "DM";
    public static final String INCH = "IN";
    public static final String SHIPMENT_ADDRESSES = "SHIPMENT_ADDRESSES";
    public static final String CONSOLIDATION_ADDRESSES = "CONSOLIDATION_ADDRESSES";
    public static final String PICK_UP = "Pickup";
    public static final String DELIVERY = "Delivery";
    public static final String SHIPMENT_ID_PREFIX = "SHP000";

    // Source
    public static final String API = "API";
    public static final String KAFKA_EVENT_CREATE = "create";
    public static final String KAFKA_EVENT_UPDATE = "update";
    public static final String ORIGINAL_PRINT = "ORIGINAL_PRINT";
    public static final String ROUTING_CFD = "CFD";
    public static final String SHIPMENT_TYPE_STD = "STD";
    public static final String SYSTEM = "System";

    public static final String METER = "M";
    public static final String ANGSTROM = "A";
    public static final String ASTRONOMICAL_UNIT = "AU";
    public static final String CENTILEAGUE = "CL";
    public static final String CENTIMETER = "CM";
    public static final String KILOMETER = "KM";
    public static final String ELL = "Ell";
    public static final String EXAMETER = "Em";
    public static final String FATHOM = "Fathom";
    public static final String FURLONG = "Furlong";
    public static final String FOOT = "FT";
    public static final String FOOT_FT = "Foot (ft)";
    public static final String LI = "LI";
    public static final String LIGHT_YEAR = "LU";
    public static final String MICRO_METER = "UM";
    public static final String MIL = "Mil";
    public static final String MILLIMETER = "MM";
    public static final String NANOMETER = "NM";
    public static final String NAUTICAL_MILE = "MI";
    public static final String MICROINCH = "MIU";
    public static final String MILLIINCH = "MIS";
    public static final String PARSEC = "Parsec";
    public static final String PICA = "PI";
    public static final String PICOMETER = "PM";
    public static final String POINT = "PT";
    public static final String ROD = "Rod";
    public static final String YARD = "YD";

    public static final String DO_PRINT = "DOPR";
    public static final String HAWB_PRINT = "HAWBPR";
    public static final String MAWB_PRINT = "MAWBPR";
    public static final String HBL_PRINT = "HBLPR";

    public static final String MTR = "Meter";
    public static final String CM = "CM";
    public static final String IN = "IN";
    public static final String FT = "FT";
    public static final String M = "M";
    public static final String EQ = "=";
    public static final String CROSS = "X";
    public static final String DEFAULT_DIMN_TEXT = "DIMS: In ";
    public static final String KGS = "KGS";
    public static final String CMS = "CMS";
    public static final String FEET = "Feet";

    // Charge Codes for Payment Terms
    public static final String PREPAID_DESC = "PPD";
    public static final String PREPAID_CODE = "PP";
    public static final String COLLECT_DESC = "CCX";
    public static final String COLLECT_CODE = "CC";
    public static final String COLLECT_PREPAID_DESC_CODE = "CP";
    public static final String PREPAID_COLLECT_DESC_CODE = "PC";

    public static final String TRUE = "true";
    public static final String PRE_ALERT_EVENT_CODE = "PREALSNT";
    public static final String SHIPMENT_SENT = "ShipmentSent";
    public static final String CONSOLIDATION_SENT = "ConsolidationSent";

    public static final String CREDIT_LIMIT = "CREDIT_LIMIT";

    public static final String CARRIER_BOOKING_CREATE = "CarrierBooking:Create";
    public static final String CARRIER_BOOKING_VIEW = "CarrierBooking:View";

    public static final String CONTAINER_CODE = "containerCode";
    public static final String IS_DELETED = "isDeleted";
    public static final String SHIPMENT_ID = "shipmentId";
    public static final String TENANT_ID = "tenantId";
    public static final String CONSOLIDATION_ID = "consolidationId";
    public static final String SHIPMENTS_LIST = "shipmentsList";
    public static final String CREATED_AT = "createdAt";
    public static final String UPDATED_BY = "updatedBy";
    public static final String CREATED_BY = "createdBy";
    public static final String UPDATED_AT = "updatedAt";
    public static final String TRANSPORT_MODE = "transportMode";
    public static final String YYYY_MM_DD_T_HH_MM_SS = "yyyy-MM-dd'T'HH:mm:ss";
    public static final String CONTAINER_ID = "containerId";
    public static final String CARRIER_DETAILS = "carrierDetails";
    public static final String CONSOLIDATION_DETAILS = "ConsolidationDetails";
    public static final String ERROR_OCCURRED_FOR_EVENT = "Request: {} || Error occured for event: {} with exception: {}";
    public static final String XLSX = ".xlsx";
    public static final String YYYY_MM_DD_HH_MM_SS_FORMAT = "yyyyMMddHHmmss";
    public static final String CONTENT_TYPE_FOR_EXCEL = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
    public static final String CONTENT_DISPOSITION = "Content-Disposition";
    public static final String ATTACHMENT_FILENAME = "attachment; filename=";
    public static final String STRING_FORMAT = "%s %s";
    public static final String ORG_CODE = "orgCode";
    public static final String ADDRESS_CODE = "addressCode";
    public static final String DEFAULT_VIEW_RETRIEVE_BY_ID_ERROR = "View is null for Id {} with Request Id {}";
    public static final String SALT_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
    public static final String CLIENT = "client";
    public static final String CONSIGNER = "consigner";
    public static final String CONSIGNEE = "consignee";
    public static final String SHIPMENT_DETAILS = "ShipmentDetails";
    public static final String ADDITIONAL_DETAILS = "additionalDetails";
    public static final String STATUS = "status";
    public static final String JOB_TYPE = "jobType";
    public static final String DELIVERY_DETAILS = "deliveryDetails";
    public static final String PICKUP_DETAILS = "pickupDetails";
    public static final String FLIGHT_NUMBER = "flightNumber";
    public static final String SHIPPING_LINE = "shippingLine";
    public static final String VESSEL = "vessel";
    public static final String VESSEL_GUID_V1 = "Guid";
    public static final String VOYAGE = "voyage";
    public static final String ORIGIN_PORT = "originPort";
    public static final String DESTINATION_PORT = "destinationPort";
    public static final String IS_NULL = "ISNULL";
    public static final String CONTAINERS_LIST = "containersList";
    public static final String CONSOLIDATION_LIST = "consolidationList";
    public static final String ORDER_MANAGEMENT_ID = "orderManagementId";
    public static final String HANDLING_INFO = "handlingInfo";
    public static final String DESCRIPTION_OF_GOODS = "descriptionOfGoods";
    public static final String BOOKING_STATUS_FIELD = "bookingStatus";
    public static final String CONTAINER_NUMBER = "containerNumber";
    public static final String NET_WEIGHT = "netWeight";
    public static final String GROSS_WEIGHT = "grossWeight";
    public static final String PACKS = "packs";
    public static final String UNLOCATIONS = "Unlocations";
    public static final String CONTAINER_TYPES = "ContainerTypes";
    public static final String FLASH_POINT = "flashpoint";
    public static final String BRANCH = "branch";
    public static final String PRODUCT_PROCESS_TYPES = "productProcessTypes";
    public static final String COUNTRY = "_country";
    public static final String NAME = "_name";
    public static final String CODE = "_code";
    public static final String FAILURE_EXECUTING = "failure executing :(";
    public static final String SYSTEM_GENERATED = "SYSTEM_GENERATED";

    public static final List<String> ColumnsToBeDeletedForExport = List.of("sealNumber",DESCRIPTION_OF_GOODS,"noOfPackages",NET_WEIGHT,"netWeightUnit",
            GROSS_WEIGHT,"grossWeightUnit","grossVolume", "grossVolumeUnit","tareWeight","tareWeightUnit",
            "measurement","measurementUnit","hsCode","isShipperOwned","isEmpty","carrierSealNumber",
            "shipperSealNumber","terminalOperatorSealNumber","veterinarySealNumber","customsSealNumber","customsReleaseCode",
            "containerComments", CONTAINER_CODE,"isReefer","minTemp","minTempUnit", "hblDeliveryMode","dgClass","hazardous",
            "hazardousUn","commodityCode", IS_DELETED,"pacrNumber","serialNumber","innerPackageNumber","innerPackageType",
            "packageLength","packageBreadth","packageHeight","isTemperatureMaintained",
            "id", "bookingId", CONSOLIDATION_ID, SHIPMENT_ID, "containerTypeId", "maxTemp", "maxTempUnit", TENANT_ID,
            "containerTypeDescription", "allocationDate", "CommodityGstPercentage", CREATED_BY, CREATED_AT, UPDATED_BY,
            UPDATED_AT, "guid", "containerStuffingLocation","containerCount", "truckingDetails");

    public static final List<String> ColumnsToBeDeleted = List.of("id", "bookingId", "ShippingInstructionId", CONSOLIDATION_ID,
            SHIPMENT_ID, CONTAINER_CODE, "commodityCode", "maxTemp", "maxTempUnit", TENANT_ID,
            "containerTypeTEU", "perContainerCostRate","perContainerSellRate","currentCostRate",
            "minimumCost",  "totalCostValue","currentSellRate", "minimumSell", "totalSellValue", "pickupAddress",
            "deliveryAddress", "allocationDate", CREATED_BY, CREATED_AT, UPDATED_BY,
            UPDATED_AT, "eventsList", IS_DELETED, "truckingDetails");

    public static final List<String> ColumnsToBeDeletedForCargo = List.of("sealNumber","noOfPackages", "isOwnContainer", "ownType", IS_DELETED,
            "measurement","measurementUnit","isShipperOwned","isEmpty","carrierSealNumber",
            "shipperSealNumber","terminalOperatorSealNumber","veterinarySealNumber","customsSealNumber",
            CONTAINER_CODE,"isReefer",CONTAINER_NUMBER,
            "containerStuffingLocation","containerCount", TRANSPORT_MODE,"hazardousCheckBox");

    public static final List<String> ColumnsToBeDeletedForContainer = List.of(IS_DELETED,"serialNumber", "innerPackageNumber", "innerPackageType",
            "packageLength", "packageBreadth", "packageHeight", "innerPackageMeasurementUnit",
            "isTemperatureMaintained","chargeable", "chargeableUnit", TRANSPORT_MODE,
            "hazardousCheckBox");

    public static final List<String> ColumnsToBeDeletedForConsolidationCargo = List.of( "id", CONSOLIDATION_ID, "DGGoodsId",
            SHIPMENT_ID, "commodityId", TENANT_ID, CONTAINER_ID, TRANSPORT_MODE,
            CREATED_BY, CREATED_AT, UPDATED_BY, UPDATED_AT, IS_DELETED, "vinNumber");

    public static final String WITH_REQUEST_ID_MSG = " with Request Id {}";
    public static final String UNUSED = "Unused";
    public static final String NON = "NON";
    public static final String EAW = "EAW";
    public static final String EVENT = "event";
    public static final String TYPE = "Type";
    public static final String DESCARTES = "Descartes";
}
