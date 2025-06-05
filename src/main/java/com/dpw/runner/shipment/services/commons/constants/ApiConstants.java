package com.dpw.runner.shipment.services.commons.constants;

public class ApiConstants {

    private ApiConstants(){}
    public static final String BOOKING_API_CREATE_CONTAINERS = "/create/booking/containers";
    public static final String BOOKING_API_UPDATE_CONTAINERS = "/update/booking/containers";
    public static final String BOOKING_API_LIST_CONTAINERS = "/list/booking/containers";
    public static final String BOOKING_API_DELETE_CONTAINERS = "/delete/booking/containers";

    public static final String BOOKING_API_CREATE_REFERENCES = "/create/booking/references";
    public static final String BOOKING_API_UPDATE_REFERENCES = "/update/booking/references";
    public static final String BOOKING_API_LIST_REFERENCES = "/list/booking/references";
    public static final String BOOKING_API_DELETE_REFERENCES = "/delete/booking/references";

    public static final String BOOKING_API_CREATE_PARTIES = "/create/booking/parties";
    public static final String BOOKING_API_UPDATE_PARTIES = "/update/booking/parties";
    public static final String BOOKING_API_LIST_PARTIES = "/list/booking/parties";
    public static final String BOOKING_API_DELETE_PARTIES = "/delete/booking/parties";
    public static final String CONSOLIDATION_API_LIST = "consolidation-routing";
    public static final String CONSOLIDATION_API_UPDATE_BULK = "consolidation/update/bulk";
    public static final String UPDATE_SAILING_SCHEDULE = "/update-sailing-schedule";

    public static final String GET_CARGO_DETAILS = "/get/cargoDetails";


    public static final String API_CREATE = "/create";
    public static final String API_CREATE_TAGS_SHIPMENT = "/tags/shipment";
    public static final String API_UPLOAD = "/upload";
    public static final String API_UPLOAD_EVENTS = "/upload-events";


    public static final String API_DOWNLOAD = "/download";


    public static final String API_UPDATE = "/update";
    public static final String API_UPDATE_BULK = "/update/bulk";

    public static final String API_RETRIEVE = "/retrieve";

    public static final String API_RETRIEVE_BY_ID = "/retrieve/id";
    public static final String API_RETRIEVE_BY_BOOKING_NUMBER = "/retrieve/booking-number";

    public static final String API_GET_NEXT_MAWB = "/retrieve/nextmawb/id";
    public static final String API_CLONE = "/clone";
    public static final String EXPORT_LIST = "/export-list";

    // Shipment endpoints
    public static final String SHIPMENT_API_CREATE = "shipment/create";
    public static final String SHIPMENT_API_UPDATE = "shipment/update";
    public static final String SHIPMENT_API_DELETE = "shipment/delete";
    public static final String SHIPMENT_API_LIST = "shipment-routing";
    public static final String SHIPMENT_API_UPDATE_BULK = "shipment/update/bulk";
    public static final String SHIPMENT_API_DELETE_BULK = "shipment/delete/bulk";
    public static final String BOOKING_API_CREATE_PACKAGES = "/create/booking/packages";
    public static final String BOOKING_API_UPDATE_PACKAGES = "/update/booking/packages";
    public static final String BOOKING_API_LIST_PACKAGES = "/list/booking/packages";
    public static final String BOOKING_API_DELETE_PACKAGES = "/delete/booking/packages";


    public static final String API_RETRIEVE_BY_ID_PARTIAL="/retrieve/partial/id";
    public static final String API_COMPLETE_RETRIEVE_BY_ID = "/retrieve/complete/id";

    public static final String API_UPDATE_SHIPMENT = "/update/shipment";

    public static final String API_UPDATE_CONSOLIDATION = "/update/consolidation";
    public static final String API_UPDATE_SETTINGS = "/update/settings";


    public static final String API_UPDATE_BOOKING = "/update/booking";
    public static final String API_CANCEL_BOOKING = "/cancel/booking";

    public static final String API_LIST = "/list";
    public static final String CREATE_NON_BILLABLE_CUSTOMER = "/createNonBillableCustomer";
    public static final String API_LIST_EXTERNAL = "/list/external";
    public static final String API_LIST_WITHOUT_FILTER = "/listWithoutFilter";
    public static final String API_CONSOLE_SHIPMENT_LIST = "/console-shipment-list";
    public static final String API_DELETE = "/delete";
    public static final String API_DELETE_BULK = "/delete/bulk";
    public static final String API_SAVE_FROM_V1 = "/save/v1";
    public static final String API_PARTIAL_UPDATE = "/patch";
    public static final String TOGGLE_LOCK = "/toggleLock";
    public static final String API_LIST_CONTAINERS_TO_ASSIGN = "/list/containers/assign";
    public static final String API_CHANGE_UNIT_ALLOCATED_ACHIEVED = "/calculate/achieved/unit";
    public static final String API_CHECK_ALLOCATED_DATA_CHANGE = "/check/allocated";
    public static final String API_CALCULATE_ACHIEVED_PACK_DETACH = "/detach/packs";
    public static final String API_VALIDATE_CONTAINER_NUMBER = "/number/validate";
    public static final String API_CALCULATE_UTILIZATION = "/calculate/utilization";
    public static final String API_CALCULATE_CHARGEABLE = "/calculate/chargeable";
    public static final String API_CALCULATE_ACHIEVED_VALUES = "/calculate/achieved";
    public static final String API_CREATE_FROM_BOOKING = "/create/from/booking";
    public static final String STATE_BASED_LIST = "/state-based-list";
    public static final String API_LIST_BILL_CHARGES_SHIPMENTS = "/list-for-bill-charges";

    public static final String ENUM_API_HANDLE = "/api/v2/enums";
    public static final String ATTACH_SHIPMENTS = "attach-shipments";
    public static final String DETACH_SHIPMENTS = "detach-shipments";
    public static final String ATTACH_CONSOLIDATION = "attach-consolidation";
    public static final String SAVE_FROM_V1 = "save/from/v1";
    public static final String API_GET_CUSTOM_REQ = "getCustomReq";
    public static final String API_SYNC_PRODUCT_SEQ = "getCustomProdSeqSync";
    public static final String API_SYNC_EVENTS = "getEvents";
    public static final String API_SYNC_CONTAINERS = "getContainers";
    public static final String SYNC = "/sync";
    public static final String AUDIT_LOG_SYNC = "/sync/auditLogs";
    public static final String LIST_TI = "/list/TI";
    public static final String LIST_CONTAINER_FOR_TI = "/list/TI/containers";
    public static final String API_DOWNLOAD_EVENTS = "/download-events";
    public static final String API_LIST_PACKS_TO_DETACH = "/list/packs/detach";
    public static final String API_RETRIEVE_BY_TENANT_ID = "/retrieve/tenantId";
    public static final String API_LIST_COLOAD_STATION_ID = "/list-coload-stations";
    public static final String API_LIST_HUBS_STATION_ID = "/list-hub-stations";
    public static final String API_ASSIGN_SHIPMENT_CONTAINERS = "/assign/containers";
    public static final String API_ASSIGN_ALL_CONTAINERS = "/assign/all/containers";
    public static final String API_RETRIEVE_BY_ORDER_ID = "retrieve/orderId";
    public static final String BULK_SYNC = "/bulk-sync";
    public static final String API_DEFAULT_SHIPMENT = "/getDefaultShipment";
    public static final String GET_MASTER_DATA_MAPPING = "/get/masterDataMapping";
    public static final String CALCULATE_CONTAINER_SUMMARY = "/container/summary";
    public static final String CALCULATE_PACK_SUMMARY = "/pack/summary";
    public static final String  GET_PACK_UTILISATION = "/get/packUtilisation";
    public static final String HIDE_MANIFEST = "/hideManifest";

    public static final String SHIPMENT_SUMMARY = "/get/shipmentSummary";
    public static final String CALCULATE_AUTO_UPDATE_WT_VOL_SHIPMENT = "/calculate/auto";
    public static final String CALCULATE_WT_VOL_SHIPMENT_ON_CHANGES = "/calculate/changes";
    public static final String LIST_PACKS_FOR_ASSIGN_DETACH = "/list/packs";
    public static final String ASSIGN_PACKS_SHIPMENTS = "/assign/shipments";
    public static final String DETACH_PACKS_SHIPMENTS = "/detach/shipments";
    public static final String GET_ALL_MASTER_DATA = "/get/master-data";
    public static final String AUTO_CALCULATE_PACKS_DATA = "/auto-calc-packs-data";
    public static final String AUTO_CALCULATE_CHARGABLE = "/auto-calc-chargable";
    public static final String AUTO_CALCULATE_VOLUME = "/auto-calc-volume";
    public static final String GET_AUTO_UPDATE_DESC_GOODS = "/get/auto-update/goods";
    public static final String GET_CONT_PACK_SUMMARY = "/get/container-pack-summary";
    public static final String API_DEFAULT_CONSOLIDATION = "/getDefaultConsolidation";
    public static final String GET_ID_BY_GUID = "/get/id";
    public static final String RETIEVE_BY_MAWB_ID = "/get/mawbId";
    public static final String VALIDATE_MAWB = "/validate-mawb";
    public static final String AUTO_ATTACH_CONSOLIDATION = "/auto-attach-Consolidation";
    public static final String ATTACHMENT_FLAG = "/attachment-flag";
    public static final String API_UPLOAD_CONTAINER_DETAILS_SUCCESS_MESSAGE = "Container Details uploaded successfully.";
    public static final String API_UPLOAD_CONTAINER_EVENTS_SUCCESS_MESSAGE = "Container Events uploaded successfully.";

    public static final String API_UPLOAD_PACKING_DETAILS_SUCCESS_MESSAGE = "Packing Details uploaded successfully.";
    public static final String CALCULATE_VOLUMETRIC_WEIGHT = "/calculate-volumetric-weight";
    public static final String POPULATE_CHARGE_TYPE_DETAILS = "/get/chargeType";
    public static final String VALIDATE_IATA_AGENT = "/validate-iata-agent";
    public static final String GET_GUID_BY_ID= "/get/guid";
    public static final String GET_CONTAINER_EDIT_ALLOW= "/get/dg-container/edit-allow";
    public static final String FNM_STATUS_MESSAGE = "/get/fnmStatus";
    public static final String FETCH_IATA_RATES = "/fetch-iata-rate";
    public static final String GET_DG_SHIPMENT= "/get/dgShipment";

    public static final String X_API_KEY = "x-api-key";
    public static final String X_ACCESS_TOKEN = "x-access-token";
    public static final String X_DPW_APPLICATION_ID = "X-DPW-ApplicationId";

    // Bridge service
    public static final String X_CLIENT_TYPE = "X-Client-Type";

    public static final String GET_ALL_SHIPMENTS_COUNT = "/all/shipments/count";
    public static final String GET_ALL_CONSOLE_SHIPMENTS_LATEST_DATE = "/console/shipments/latestDate";
    public static final String UPDATE_SHIPMENT_STATUS = "/status/update";
    public static final String REQUEST_INTER_BRANCH_CONSOLE = "/request-inter-branch-console";

    // Notifications
    public static final String GET_PENDING_NOTIFICATIONS = "/get/pendingNotifications";

    public static final String OCEAN_DG_SEND_FOR_APPROVAL = "/DGSendForApproval";
    public static final String OCEAN_DG_APPROVAL_RESPONSE = "/DGApprovalResponse";
    public static final String LIST_SHIPMENT_CONSOLIDATION = "/list/shipment/requested-console";
    public static final String ATTACH_DETACH_ORDER = "/attach-detach-order";

    public static final String CANCEL = "/cancel";
    public static final String LIST_BRANCHES_BY_DEFAULT_ORG_AND_ADDRESS = "/listBranchesByDefaultOrgAndAddress";
    public static final String MATCHING_RULES_BY_GUID = "/get-matching-rules/guid";
    public static final String MATCHING_RULES_BY_GUID_AND_EXECUTION_STATE = "/get-matching-rules";

    // Runner V3.0 API Endpoints
    public static final String API_CREATE_V3 = "/createV3";
    public static final String API_UPDATE_V3 = "/updateV3";
    public static final String API_LIST_V3 = "/listV3";
    public static final String API_RETRIEVE_BY_ID_V3 = "/retrieveV3/id";

    public static final String API_RETRIEVE_PENDING_NOTIFICATION_DATA = "/pending/notification/data";

    public static final String API_GET_SHIPMENT_ASSIGN_CONTAINER_TRAY = "/get/shipments/assignContainerTray";

    public static final String API_GET_SHIPMENT_UN_ASSIGN_CONTAINER_TRAY = "/get/shipments/unAssignContainerTray";

    public static final String SHIPMENT = "/shipment";
    public static final String CONSOLIDATION = "/consolidation";
    public static final String CUSTOMER_BOOKING = "/customer-booking";
    public static final String SHIPMENT_PACKINGS = "/shipment-packings";
    public static final String CONSOLIDATION_PACKINGS = "/consolidation-packings";


}