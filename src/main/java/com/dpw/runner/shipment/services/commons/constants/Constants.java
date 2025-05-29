package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class Constants {

    public static final String TRANSPORT_INSTRUCTION_TYPES = "TransportInstructionTypes";
    public static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    public static final String WRK = "WRK";
    public static final String ORGANIZATION_CODE = "OrganizationCode";
    public static final String ORG_ID = "OrgId";
    public static final String ACTIVE_CLIENT = "ActiveClient";
    public static final String LOCAL_REFERENCE_NUMBER = "LocalReferenceNumber";

    public static final String GLOBALFF = "GLOBALFF";
    public static final String GLOBAL = "GLOBAL";
    public static final String SUBMITTED = "Submitted";
    public static final int TRANSPORT_MODE_INDEX = 0;
    public static final String DIRECTION = "direction";
    public static final String SHIPMENT_TYPE = "shipmentType";
    public static final String CARGO_TYPE = "cargoType";
    public static final String VOL = "VOL";
    public static final String CONTAINER_CATEGORY = "containerCategory";
    public static final int DIRECTION_INDEX = 1;
    public static final int SHIPMENT_TYPE_INDEX = 2;

    public static final String IS_DOMESTIC = "isDomestic";
    public static final String DOMESTIC = "domestic";
    public static final String INTERNATIONAL = "international";
    public static final int IS_DOMESTIC_INDEX = 3;
    public static final String DELIMITER = "-";
    public static final String CONTAINS_HAZARDOUS = "containsHazardous";
    public static final String SHIPMENT_LIST_PERMISSION = "ShipmentList";
    public static final String VIEW_PERMISSION = "View";
    public static final String CREATE_PERMISSION = "Create";
    public static final String MODIFY_PERMISSION = "Modify";
    public static final String CANCEL_PERMISSION = "Cancel";
    public static final String SHIPMENT_RETRIEVE_PERMISSION = "ShipmentRetrive";
    public static final String SHIPMENT_CREATE_PERMISSION = "ShipmentCreate";
    public static final String SHIPMENT_UPDATE_PERMISSION = "ShipmentUpdate";
    public static final String SHIPMENT_CANCEL_PERMISSION = "ShipmentCancel";
    public static final String CONSOLIDATION_LIST_PERMISSION = "ConsolidationList";
    public static final String CONSOLIDATION_RETRIEVE_PERMISSION = "ConsolidationRetrive";
    public static final String CONSOLIDATION_CREATE_PERMISSION = "ConsolidationCreate";
    public static final String CONSOLIDATION_UPDATE_PERMISSION = "ConsolidationUpdate";
    public static final String CONSOLIDATION_CANCEL_PERMISSION = "ConsolidationCancel";

    public static final String ALL = "all";
    public static final String BROWSER_TIMEZONE = "browser-timezone";
    public static final String USER_SERVICE_V1 = "v1";
    public static final String USER_SERVICE_MAVANI = "mavani";
    public static final String NO_DATA = "Not Found!";
    public static final String SHIPMENT = "SHIPMENT";
    public static final String SHIPMENT_CAMELCASE = "Shipment";
    public static final String CONSOLIDATION_CAMELCASE = "Consolidation";
    public static final String CUSTOMER_BOOKING = "CustomerBooking";
    public static final String BOOKING = "BOOKING";
    public static final String BOOKING_CHARGES = "BOOKING_CHARGES";
    public static final String CONSOLIDATION = "CONSOLIDATION";
    public static final String TENANT_SETTINGS = "TENANT_SETTINGS";
    public static final String ROUTING = "ROUTING";
    public static final String PACKING = "PACKING";
    public static final String REFERENCE_NUMBERS = "REFERENCE_NUMBERS";
    public static final String TOTAL_PACKAGES_TYPE = "TOTAL_PACKAGES_TYPE";
    public static final String DG_PACKAGES_TYPE = "DG_PACKAGES_TYPE";
    public static final String MPK = "MPK";
    public static final String PIECES = "Pieces";
    public static final String MULTI_PACK = "Multi Pack";
    public static final String CONTAINER = "CONTAINER";
    public static final String PACKAGES = "Packages";
    public static final String CARRIAGE = "CARRIAGE";
    public static final String PRE_CARRIAGE = "PreCarriage";
    public static final String SERVICE_DETAILS = "SERVICE_DETAILS";
    public static final String NOTES = "NOTES";
    public static final String MAIN = "Main";
    public static final String EVENTS = "EVENTS";
    public static final String JOBS = "JOBS";
    public static final String CONT_20 = "20";
    public static final String CONT_40 = "40";
    public static final String CONT_20_GP = "20GP";
    public static final String CONT_20_RE = "20RE";
    public static final String CONT_40_GP = "40GP";
    public static final String CONT_40_RE = "40RE";
    public static final String PAGE_NUMBER_ERROR = "Page Number should be Greater than or equal to 1";
    public static final String VALIDATION_ERRORS_OCCURRED = "Validation errors occurred.";
    public static final String MASS = "Mass";
    public static final String VOLUME = "Volume";
    public static final String LENGTH = "Length";
    public static final String VOLUME_UNIT_M3 = "M3";
    public static final String VOLUME_UNIT_LITRE = "L";
    public static final String VOLUME_UNIT_CC = "CC";
    public static final String VOLUME_UNIT_CM = "CM";
    public static final String VOLUME_UNIT_AF = "AF";
    public static final String VOLUME_UNIT_BARREL_OIL = "Barrel (oil)";
    public static final String VOLUME_UNIT_BOARD_FOOT = "Board foot";
    public static final String VOLUME_UNIT_BUSHEL_US = "Bushel (US)";
    public static final String VOLUME_UNIT_CUP = "Cup";
    public static final String VOLUME_UNIT_FLUID_OUNCE_US = "Fluid ounce (US)";
    public static final String VOLUME_UNIT_CF = "CF";
    public static final String VOLUME_UNIT_GI = "GI";
    public static final String VOLUME_UNIT_GA = "GA";
    public static final String VOLUME_UNIT_GALLON_US_LIQ = "Gallon (US,liq)";
    public static final String VOLUME_UNIT_GILL_UK = "Gill (UK)";
    public static final String VOLUME_UNIT_GILL_US = "Gill (US)";
    public static final String VOLUME_UNIT_CI = "CI";
    public static final String VOLUME_UNIT_LITER_OLD = "Liter (old)";
    public static final String VOLUME_UNIT_OUNCE_UK_FLD = "Ounce (UK,fluid)";
    public static final String VOLUME_UNIT_OUNCE_US_FLD = "Ounce (US,fluid)";
    public static final String VOLUME_UNIT_PECK_US = "Peck (US)";
    public static final String VOLUME_UNIT_PINT_US_DRY = "Pint (US,dry)";
    public static final String VOLUME_UNIT_PINT_US_LIQ = "Pint (US,liq)";
    public static final String VOLUME_UNIT_QUART_US_DRY = "Quart (US,dry)";
    public static final String VOLUME_UNIT_QUART_US_LIQ = "Quart (US,liq)";
    public static final String VOLUME_UNIT_STERE = "Stere";
    public static final String VOLUME_UNIT_TABLESPOON = "Tablespoon";
    public static final String VOLUME_UNIT_TEASPOON = "Teaspoon";
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
    public static final String WEIGHT_UNIT_SLUG = "Slug";
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
    public static final String SHIPMENT_TYPE_BLK = "BLK";
    public static final String SHIPMENT_TYPE_HSE = "HSE";
    public static final String SHIPMENT_TYPE_SCN = "SCN";
    public static final String SHIPMENT_TYPE_BCN = "BCN";
    public static final String CARGO_TYPE_FCL = "FCL";
    public static final String CARGO_TYPE_LSE = "LSE";
    public static final String CARGO_TYPE_FTL = "FTL";
    public static final String DIRECTION_EXP = "EXP";
    public static final String JOB_TYPE_CLB = "CLB";
    public static final String DIRECTION_IMP = "IMP";
    public static final String DIRECTION_CTS = "CTS";

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

    public static final String DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB = "CONSOLIDATION AS PER ATTACHED LIST";
    public static final double AIR_FACTOR_FOR_VOL_WT = 166.667;
    public static final double ROAD_FACTOR_FOR_VOL_WT = 333.0;

    /**
     * Enum Constants
     **/

    public static final String SHIPMENT_STATUS = "ShipmentStatus";
    public static final String BOOKING_STATUS = "BookingStatus";

    public static final String MASTER_LIST = "MasterList";
    public static final String OWNERSHIP = "OwnerShip";

    public static final String SERVICE = "Service";
    public static final String PENDING_ACTION = "Pending Action";
    public static final String OCEAN_DG_TASKTYPE = "DG Ocean Approval";

    public static final String TI_TEMPLATE_TYPE = "TransportInstructionTemplateType";
    public static final String  CUSTOMER_CATEGORY_RATES = "CustomerCategoryRates";
    public static final String CARRIER_BOOKING_STATUS = "CarrierBookingStatus";
    public static final String DATE_BEHAVIOR_TYPE = "DateBehaviorType";
    public static final String SHIPMENT_PACK_STATUS = "ShipmentPackStatus";
    public static final String REGULATED_AGENT = "RegulatedAgent";
    public static final String KNOWN_CONSIGNOR = "KnownConsignor";


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
    public static final String TENANT_MASTER_DATA = "TenantIdData";
    public static final String COLLECTION_TABLE = "CollectionTable";
    public static final String MASTER_DATA = "MasterData";


    public static final String DEFAULT_DATE_FORMAT = "MM/dd/yyyy";
    public static final String CONSOLIDATION_TYPE_CLD = "CLD";

    public static final String CONSOLIDATION_TYPE_AGT = "AGT";

    public static final String AGENT_PREFIX = "A";
    public static final String CARRIER_PREFIX = "C";

    public static final String MASTER_DATA_SOURCE_CARGOES_RUNNER = "Cargoes Runner";
    public static final String MASTER_DATA_SOURCE_CARGOES_TRACKING = "Cargoes Tracking";
    public static final String MASTER_DATA_SOURCE_CARGOES_USER = "Cargoes User";
    public static final String MASTER_DATA_SOURCE_DESCARTES = "Descartes";

    public static final String SHIPMENTS_WITH_SQ_BRACKETS = "[Shipments]";
    public static final String CONSOLIDATIONS_WITH_SQ_BRACKETS = "[Consolidations]";
    public static final String BOOKINGS_WITH_SQ_BRACKETS = "[Bookings]";

    public static final String IMP = "IMP";
    public static final String SHIPMENT_TYPE_DRT = "DRT";
    public static final String CONSOLIDATION_TYPE_DRT = "DRT";

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
    public static final String NETWORK_TRANSFER = "network_transfer";

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
    public static final String DEFAULT_DIMN_TEXT = "DIMS ";
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
    public static final String SHIPMENT_IMPORTED = "ShipmentImported";
    public static final String CONSOLIDATION_IMPORTED = "ConsolidationImported";

    public static final String CREDIT_LIMIT = "CREDIT_LIMIT";

    public static final String CARRIER_BOOKING_CREATE = "Operations:CarrierBookings:Create";
    public static final String CARRIER_BOOKING_VIEW = "Operations:CarrierBookings:View";
    public static final String CARRIER_BOOKING_MODIFY = "Operations:CarrierBookings:Modify";
    public static final String CARRIER_BOOKING_CANCEL= "Operations:CarrierBookings:Cancel";

    public static final String POUNDS = "pounds";
    public static final String KG = "kg";
    public static final String CONTAINER_CODE = "containerCode";
    public static final String IS_DELETED = "isDeleted";
    public static final String ID = "id";
    public static final String SHIPMENT_ID = "shipmentId";
    public static final String TENANT_ID = "tenantId";
    public static final String TENANTID = "TenantId";
    public static final String CONSOLIDATION_ID = "consolidationId";
    public static final String SHIPMENTS_LIST = "shipmentsList";
    public static final String CREATED_AT = "createdAt";
    public static final String UPDATED_BY = "updatedBy";
    public static final String CREATED_BY = "createdBy";
    public static final String UPDATED_AT = "updatedAt";
    public static final String TRANSPORT_MODE = "transportMode";
    public static final String LAT_DATE = "latDate";
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
    public static final String ROUTINGS = "routings";
    public static final String STATUS = "status";
    public static final String JOB_TYPE = "jobType";
    public static final String INCOTERMS = "incoterms";
    public static final String DELIVERY_DETAILS = "deliveryDetails";
    public static final String PICKUP_DETAILS = "pickupDetails";
    public static final String FLIGHT_NUMBER = "flightNumber";
    public static final String SHIPPING_LINE = "shippingLine";
    public static final String VESSEL = "vessel";
    public static final String VESSEL_GUID_V1 = "Guid";
    public static final String VOYAGE = "voyage";
    public static final String ORIGIN_PORT = "originPort";
    public static final String DESTINATION_PORT = "destinationPort";

    public static final String OCEAN_DG_ROLE = "OCEAN_DG_ROLE";
    public static final String COMMERCIAL_OCEAN_DG_ROLE = "COMMERCIAL_OCEAN_DG_ROLE";
    public static final String DG_APPROVER_NAME = "DG_APPROVER_NAME";
    public static final String DG_APPROVER_TIME = "DG_APPROVER_TIME";

    public static final String TIME = "time";
    public static final String USERNAME = "userName";

    public static final String IS_NULL = "ISNULL";
    public static final String CONTAINERS_LIST = "containersList";
    public static final String ROUTING_LIST = "routingsList";
    public static final String CONSOLIDATION_LIST = "consolidationList";
    public static final String ORDER_MANAGEMENT_ID = "orderManagementId";
    public static final String ORDER_NUMBER = "orderNumber";
    public static final String ORDER_MANAGEMENT_NUMBER = "orderManagementNumber";
    public static final String HANDLING_INFO = "handlingInfo";
    public static final String DESCRIPTION_OF_GOODS = "descriptionOfGoods";
    public static final String BOOKING_STATUS_FIELD = "bookingStatus";
    public static final String CONTAINER_NUMBER = "containerNumber";
    public static final String NET_WEIGHT = "netWeight";
    public static final String GROSS_WEIGHT = "grossWeight";
    public static final String PACKS = "packs";
    public static final String UNLOCATIONS = "Unlocations";
    public static final String ORGANIZATIONS = "Organizations";
    public static final String CONTAINER_TYPES = "ContainerTypes";
    public static final String FLASH_POINT = "flashpoint";
    public static final String BRANCH = "branch";
    public static final String PRODUCT_PROCESS_TYPES = "productProcessTypes";
    public static final String COUNTRY = "_country";
    public static final String NAME = "_name";
    public static final String CODE = "_code";
    public static final String IATA_CODE = "_iataCode";
    public static final String SCAC_CODE = "_scacCode";
    public static final String DISPLAY_NAME = "_displayName";
    public static final String FAILURE_EXECUTING = "failure executing :(";
    public static final String SYSTEM_GENERATED = "SYSTEM_GENERATED";
    public static final String SCO = "SCO";
    public static final String SPX = "SPX";
    public static final String SHR = "SHR";
    public static final String OPEN_FOR_ATTACHMENT = "openForAttachment";
    public static final String INTER_BRANCH_CONSOLE = "interBranchConsole";
    public static final String SHIPPER_COUNTRY = "shipperCountry";
    public static final String CONSIGNEE_COUNTRY = "consigneeCountry";
    public static final String ISSUING_AGENT_COUNTRY = "issuingAgentCountry";
    public static final String AWB_COUNTRY = "country";

    // Include Column fields
    public static final String BILLING_DATA = "billingData";
    public static final String SHIPPER_REFERENCE = "pickupDetails.shipperRef";
    public static final String ORDERS_COUNT = "ordersCount";
    public static final String SHIPMENT_STATUS_FIELDS = "shipmentStatus";


    public static final List<String> ColumnsToBeDeletedForExport = List.of("sealNumber",DESCRIPTION_OF_GOODS,NET_WEIGHT,"netWeightUnit",
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

    public static final List<String> ColumnsToBeDeletedForCargo = List.of("sealNumber", "isOwnContainer", "ownType", IS_DELETED,
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
    public static final String INSECURE = "Insecure";
    public static final String EXEMPTION_CARGO = "Exemption Cargo";
    public static final String UNUSED = "Unused";
    public static final String NON = "NON";
    public static final String EAW = "EAW";
    public static final String EAP = "EAP";
    public static final String AOM = "AOM";
    public static final String EVENT = "event";
    public static final String TYPE = "Type";
    public static final String DESCARTES = "Descartes";
    public static final String INTTRA = "INTTRA";
    public static final String PICKUP_DELIVERY = "PICKUP_DELIVERY";
    public static final String VIEWS = "Views";
    public static final String ENTITY = "entity";
    public static final String NAME_FILTER = "name";
    public static final String ACTIVE = "Active";
    public static final String FILE_STATUS = "FileStatus";
    public static final String FLOW = "Flow";
    public static final String TASK_TYPE = "TaskType";
    public static final String TASK_STATUS = "TaskStatus";
    public static final String AIR_AUTHORISING_ENTITY = "AirAuthorisingEntity";
    public static final String CONTAINER_PRA_STATUS_ENTITY = "ContainerPraStatus";
    public static final String REGIONAL_BRANCH_CODE = "Regional_Branch_Code";
    public static final String REGIONAL_BRANCH_NAME = "Regional_Branch_Name";
    public static final String HUB_BRANCH_CODE = "Hub_Branch_Code";
    public static final String HUB_BRANCH_NAME = "Hub_Branch_Name";

    public static final String SHIPMENT_CREATE_USER = "Shipment_Create_User";
    public static final String SHIPMENT_ASSIGNED_USER = "Shipment_Assigned_User";
    public static final String SHIPMENT_ASSIGNED_USER_WITH_SLASH = "Shipment_Assigned_User_With_Slash";
    public static final String CONSOL_BRANCH_CODE = "Consol_Branch_Code";
    public static final String CONSOL_BRANCH_NAME = "Consol_Branch_Name";
    public static final String CONSOLIDATION_NUMBER = "Consolidation_Number";
    public static final String SOURCE_CONSOLIDATION_NUMBER = "Source_Consolidation_Number";
    public static final String MAWB_NUMBER = "MAWB_Number";
    public static final String HAWB_NUMBER = "HAWB_Number";
    public static final String INTERBRANCH_SHIPMENT_NUMBER = "Interbranch_Shipment_Number";
    public static final String INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK = "Interbranch_Shipment_Number_Without_Link";
    public static final String CARRIER_CODE = "Carrier_Code";
    public static final String CARRIER_NAME = "Carrier_name";
    public static final String FLIGHT_NUMBER1 = "Flight_Number";
    public static final String POL_NAME = "POL_Name";
    public static final String POD_NAME = "POD_Name";
    public static final String ALLOCATED_WEIGHT = "Allocated_Weight";
    public static final String ALLOCATED_VOLUME = "Allocated_Volume";
    public static final String ALLOCATED_WEIGHT_UNIT = "Allocated_Weight_Unit";
    public static final String ALLOCATED_VOLUME_UNIT = "Allocated_Volume_Unit";
    public static final String REQUEST_DATE_TIME = "Request_Date_Time";
    public static final String BRANCH_TIME_ZONE = "Branch_TimeZone";
    public static final String USER_NAME = "User_name";
    public static final String USER_BRANCH = "User_Branch";
    public static final String USER_COUNTRY = "User_Country";
    public static final String IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL = "Import_Shipment_Pull_Attachment_Email_Template";

    public static final String CONSOLIDATION_CREATE_USER = "Consolidation_Create_User";
    public static final String SHIPMENT_BRANCH_CODE = "Shipment_Branch_Code";
    public static final String SHIPMENT_BRANCH_NAME = "Shipment_Branch_Name";
    public static final String INTERBRANCH_CONSOLIDATION_NUMBER = "Interbranch_Consolidation_Number";
    public static final String INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK = "Interbranch_Consolidation_Number_Without_Link";
    public static final String SHIPMENT_NUMBER = "Shipment_Number";
    public static final String APPROVER_NAME = "APPROVER_NAME";
    public static final String APPROVED_TIME = "APPROVED_TIME";
    public static final String LAT = "LAT";
    public static final String SHIPMENT_WEIGHT = "Shipment_Weight";
    public static final String SHIPMENT_WEIGHT_UNIT = "Shipment_Weight_Unit";
    public static final String SHIPMENT_VOLUME = "Shipment_Volume";
    public static final String SHIPMENT_VOLUME_UNIT = "Shipment_Volume_Unit";
    public static final String ACTIONED_USER_NAME = "Actioned_User_name";
    public static final String REJECT_REMARKS = "Reject_remarks";
    public static final String WITHDRAW_REMARKS = "Withdraw_remarks";
    public static final String REMARKS = "Remarks";
    public static final String AUTO_REJECTION_REMARK = "Target Shipment is attached to another consolidation already.";
    public static final String REQUESTED_USER_NAME = "Requested_User_Name";
    public static final String REQUESTER_REMARKS = "Requester_Remarks";
    public static final String ERROR_WHILE_SENDING_EMAIL = "Error while sending email";
    public static final String SHIPMENT_PULL_REQUESTED_EMAIL_TYPE = "Attach Shipment Request";
    public static final String SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE = "Consolidation Request - Accept";
    public static final String SHIPMENT_PULL_REJECTED_EMAIL_TYPE = "Consolidation Request - Rejected";
    public static final String SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE = "Attach Consolidation Request";
    public static final String SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE = "Shipment Request Accept";
    public static final String SHIPMENT_PUSH_REJECTED_EMAIL_TYPE = "Shipment Request Reject";
    public static final String SHIPMENT_DETACH_EMAIL_TYPE = "Shipment Detach";
    public static final String SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE = "Shipment Pull Withdraw";
    public static final String SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE = "Shipment Push Withdraw";
    public static final String CONSOLIDATION_IMPORT_EMAIL_TYPE = "CONSOLIDATION_IMPORT";
    public static final String DEFAULT_CONSOLIDATION_RECEIVED_SUBJECT = "Received consolidation {#CONSOLIDATION_NUMBER} with {#NUMBER_OF_SHIPMENTS} shipments from {#SOURCE_BRANCH}";
    public static final String DEFAULT_CONSOLIDATION_RECEIVED_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>This is to inform you that a consolidation with {#NUMBER_OF_SHIPMENTS} shipments has been sent from {#SOURCE_BRANCH} for you to import.</p>  <p>Below are its details:</p>  <p>&nbsp;</p>  <p><strong>Consolidation Details:</strong></p>  <p><strong>Sender</strong>: {#SENDER_USER_NAME}&nbsp;from {#SOURCE_BRANCH}</p>  <p><strong>Consolidation number:</strong>&nbsp;{#CONSOLIDATION_NUMBER}</p>  <p><strong>BL Numbers</strong>: {#BL_NUMBER}</p>  <p><strong>Shipment numbers:&nbsp;</strong>{#SHIPMENT_NUMBERS}</p>  <p><strong>MBL Number</strong>: {#MBL_NUMBER}</p>  <p><strong>Sent date</strong>: {#SENT_DATE}</p>  <p>&nbsp;</p>  <p>This email contains confidential content, kindly treat with caution.</p>";
    public static final String SHIPMENT_IMPORT_EMAIL_TYPE = "SHIPMENT_IMPORT";
    public static final String DEFAULT_SHIPMENT_RECEIVED_SUBJECT = "Received shipment {#SHIPMENT_NUMBER} from {#SOURCE_BRANCH}";
    public static final String DEFAULT_SHIPMENT_RECEIVED_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>This is to inform you that a shipment with following details has been sent from {#SOURCE_BRANCH} for you to import.</p>  <p>Below are its details:</p>  <p>&nbsp;</p>  <p><strong>Shipment Details:</strong><strong>&nbsp;&nbsp;</strong></p>  <p><strong>Sender</strong>: {#SENDER_USER_NAME}&nbsp;from {#SOURCE_BRANCH}</p>  <p><strong>Shipment number:</strong>&nbsp;{#SHIPMENT_NUMBER}</p>  <p><strong>BL Number</strong>: {#BL_NUMBER}</p>  <p><strong>MBL Number</strong>: {#MBL_NUMBER}</p>  <p><strong>Sent date</strong>: {#SENT_DATE}</p>  <p>&nbsp;</p>  <p>This email contains confidential content, kindly treat with caution.</p>";
    public static final String GROUPED_SHIPMENT_IMPORT_EMAIL_TYPE = "GROUPED_SHIPMENT_IMPORT";
    public static final String DEFAULT_GROUPED_SHIPMENT_RECEIVED_SUBJECT = "Shipment/s:  {#SD_ShipmentDetails}{SD_ShipmentNumber}{/SD_ShipmentDetails} created by consolidating branch – {GS_ConsolidationBranch}";
    public static final String DEFAULT_GROUPED_SHIPMENT_RECEIVED_BODY = "<p>Dear User,<br /> This is to inform you that the following shipments are created by the consolidating branch – {GS_ConsolidationBranch}<br /> Details as follows:</p> <p>━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━</p> <table> <tbody> <tr> <td><strong>Shipment Number</strong></td> <td><strong>Sending Branch</strong></td> <td><strong>HBL/HAWB Number</strong></td> <td><strong>MBL/MAWB Number</strong></td> <td><strong>Created Date</strong></td> </tr> <tr> <td>{#SD_ShipmentDetails}{SD_ShipmentNumber}</td> <td>{SD_SendingBranch}</td> <td>{SD_HBL_HAWB_Number}</td> <td>{SD_MAWB_Number}</td> <td>{SD_CreatedDate}{/SD_ShipmentDetails}</td> </tr> </tbody> </table> <p>&nbsp;</p> <p>This email contains confidential content, kindly treat with caution.</p> <p>Regards,&nbsp;<br /> CargoesRunner Team</p>";
    public static final String SOURCE_BRANCH_PLACEHOLDER = "{#SOURCE_BRANCH}";
    public static final String SHIPMENT_NUMBER_PLACEHOLDER = "{#SHIPMENT_NUMBER}";
    public static final String SENDER_USER_NAME_PLACEHOLDER = "{#SENDER_USER_NAME}";
    public static final String BL_NUMBER_PLACEHOLDER = "{#BL_NUMBER}";
    public static final String MBL_NUMBER_PLACEHOLDER = "{#MBL_NUMBER}";
    public static final String SENT_DATE_PLACEHOLDER = "{#SENT_DATE}";
    public static final String CONSOLIDATION_NUMBER_PLACEHOLDER = "{#CONSOLIDATION_NUMBER}";
    public static final String NUMBER_OF_SHIPMENTS_PLACEHOLDER = "{#NUMBER_OF_SHIPMENTS}";
    public static final String SHIPMENT_NUMBERS_PLACEHOLDER = "{#SHIPMENT_NUMBERS}";
    public static final String OCEAN_DG_APPROVER_EMAIL_TYPE = "OCEAN_DG_APPROVER";
    public static final String OCEAN_DG_SENDER_EMAIL_TYPE = "OCEAN_DG_SENDER";
    public static final String OCEAN_DG_COMMERCIAL_APPROVER_EMAIL_TYPE = "OCEAN_DG_COMMERCIAL_APPROVER";
    public static final String OCEAN_DG_COMMERCIAL_SENDER_EMAIL_TYPE = "OCEAN_DG_COMMERCIAL_SENDER";
    public static final String HTML_HREF_TAG_PREFIX = "<html><body>" + "<a href='";
    public static final String HTML_HREF_TAG_SUFFIX = "</a>" + "</body></html>";
    public static final String IMPORT_SHIPMENT_PUSH_ATTACHMENT_EMAIL = "Import_Shipment_Push_Attachment_Email_Template";

    //OCEAN DG Email Templates Type
    public static final String OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE = "OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE";
    public static final String OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE = "OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE";
    public static final String OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE = "OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE";
    public static final String OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE = "OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE";
    public static final String OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE = "OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE";
    public static final String OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE = "OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE";
    public static final String OCEAN_DG_CONTAINER_FIELDS_VALIDATION = "Please add DG class, UN Number and Proper Shipping Name to container before adding a DG Pack";
    public static final String AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION = "DG Shipment is not allowed to Consolidate with Interbranch Consolidation - %s.";
    public static final String AIR_DG_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_SHIPMENT = "DG Consolidation is not allowed for Interbranch Shipment - %s.";
    public static final String AIR_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_DG_SHIPMENT = "Consolidation is not allowed for Interbranch DG Shipment - %s.";
    public static final String AIR_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_DG_CONSOLIDATION = "Shipment is not allowed for Interbranch DG Consolidation - %s.";
    public static final String AIR_DG_CONSOLIDATION_NOT_ALLOWED_MORE_THAN_ONE_SHIPMENT = "DG Consolidation is not allowed to have more than one shipment attached.";
    public static final String CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL = "Cannot attach more shipments to a DG Consolidation - %s.";
    public static final String CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS = "Cannot update Shipment as DG Shipment, attached consolidation consists of multiple shipments.";
    public static final String NOT_ALLOWED_TO_VIEW_SHIPMENT_FOR_NTE = "You are not allowed to view this shipment as this tenant is not part of receiving agent or triangulation partner.";
    public static final String NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE = "You are not allowed to view this consolidation as this tenant is not part of receiving agent or triangulation partner.";

    public static final String SHIPMENTS_CAPS = "SHIPMENTS";

    public static final String CLIENT_ORG_CODE = "clientOrgCode";
    public static final String CONSIGNER_ORG_CODE = "consignerOrgCode";
    public static final String CONSIGNEE_ORG_CODE = "consigneeOrgCode";
    public static final String CLIENT_ADDRESS_CODE = "clientAddressCode";
    public static final String CONSIGNER_ADDRESS_CODE = "consignerAddressCode";
    public static final String CONSIGNEE_ADDRESS_CODE = "consigneeAddressCode";
    public static final String GUID = "guid";
    public static final String ROUTING_VALIDATION = "Carriage should be MAIN_CARRIAGE";
    public static final String SHIPMENTS_PERMISSION_KEY = "Shipments";
    public static final String CONSOLIDATIONS_PERMISSION_KEY = "Consolidations";
    public static final String SOURCE_SERVICE_TYPE = "SourceServiceType";

    public static final String NETWORK_TRANSFER_ENTITY = "NetworkTransfer";
    public static final String COMMON_ERROR_LOGS_ENTITY = "CommonErrorLogs";
    public static final String NETWORK_TRANSFER_ENTITY_STATUS = "NetworkTransferEntityStatus";
    public static final String NETWORK_TRANSFER_ENTITY_TYPES = "NetworkTransferEntityTypes";
    public static final String UAE_TWO_DIGIT_IATA_CODE = "AE";
    public static final String HOUR = "hour";
    public static final String MINUTE = "minute";
    public static final String SECOND = "second";
    public static final String TRACKING_PUSH_API = "TRACKING_PUSH_API";
    public static final String NOTIFICATION_ENTITY = "Notification";
    public static final String NOTIFICATION_REQUEST_TYPES = "NotificationRequestTypes";

    public static final String ADDRESS_SHORT_CODE = "AddressShortCode";
    public static final String ORG_ADDRESS = "orgAddress";
    public static final String IS_CSD_DOCUMENT_ADDED = "isCSDDocumentAdded";
    public static final String AIR_SECURITY_PERMISSION_MSG = "You do not have Air Security permissions for this.";
    public static final String SWITCH_DEFAULT_CASE_MSG = "Unhandled case in switch: {}";
    public static final String SEND_EMAIL_AIR_MESSAGING_FAILURE = "Send Email for Air Messaging Failure : {}";
    public static final String AUTO_OFFSET_RESET_CONFIG_LATEST = "latest";

    public static final String OUTBOUND = "Outbound";
    public static final String IGNORED_ERROR_MSG = "Ignored error.";
    public static final String EMPTY_STRING = "";
    public static final String YES = "Yes";
    public static final String NO = "No";
    public static final String APP_CONFIG_ID_NOT_VALID = "Application config id is not valid";
    public static final String APPLICATION_CONFIG_ID_EMPTY_ERROR_MESSAGE = "Application config id can not be empty";

    public static final String TESLA = "Tesla";
    public static final Integer EXPORT_EXCEL_DEFAULT_LIMIT = 1000;
    public static final String CONTAINS = "CONTAINS";
    public static final String CONTAINER_AFTER_SAVE = "CONTAINER_AFTER_SAVE";
    public static final String CONSOLIDATION_AFTER_SAVE = "CONSOLIDATION_AFTER_SAVE";
    public static final String CONSOLIDATION_AFTER_SAVE_TO_TRACKING = "CONSOLIDATION_AFTER_SAVE_TO_TRACKING";

    private Constants() {
    }

}
