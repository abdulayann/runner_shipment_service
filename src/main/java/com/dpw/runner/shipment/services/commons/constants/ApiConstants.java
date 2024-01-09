package com.dpw.runner.shipment.services.commons.constants;

public class ApiConstants {
    public static final String API_CREATE = "/create";
    public static final String API_UPLOAD = "/upload";
    public static final String API_UPLOAD_EVENTS = "/upload-events";


    public static final String API_DOWNLOAD = "/download";


    public static final String API_UPDATE = "/update";

    public static final String API_RETRIEVE = "/retrieve";

    public static final String API_RETRIEVE_BY_ID = "/retrieve/id";

    public static final String API_GET_NEXT_MAWB = "/retrieve/nextmawb/id";
    public static final String API_CLONE = "/clone";
    public static final String EXPORT_LIST = "/export-list";


    public static final String API_RETRIEVE_BY_ID_PARTIAL="/retrieve/partial/id";
    public static final String API_COMPLETE_RETRIEVE_BY_ID = "/retrieve/complete/id";

    public static final String API_UPDATE_SHIPMENT = "/update/shipment";

    public static final String API_UPDATE_CONSOLIDATION = "/update/consolidation";
    public static final String API_UPDATE_SETTINGS = "/update/settings";


    public static final String API_UPDATE_BOOKING = "/update/booking";

    public static final String API_LIST = "/list";

    public static final String API_DELETE = "/delete";
    public static final String API_SAVE_FROM_V1 = "/save/v1";
    public static final String API_PARTIAL_UPDATE = "/patch";
    public static final String TOGGLE_LOCK = "/toggleLock";
    public static final String API_LIST_CONTAINERS_TO_ASSIGN = "/list/containers/assign";
    public static final String API_CHANGE_UNIT_ALLOCATED_ACHIEVED = "/calculate/achieved/unit";
//    public static final String API_CALCULATE_ACHIEVED_PACK_ASSIGN = "/assign/packs";
    public static final String API_CALCULATE_ACHIEVED_PACK_DETACH = "/detach/packs";
    public static final String API_VALIDATE_CONTAINER_NUMBER = "/number/validate";
    public static final String API_CALCULATE_UTILIZATION = "/calculate/utilization";
    public static final String API_CALCULATE_CHARGEABLE = "/calculate/chargeable";
    public static final String API_CALCULATE_ACHIEVED_VALUES = "/calculate/achieved";

    public static final String ENUM_API_HANDLE = "/api/v2/enums";
    public static final String ATTACH_SHIPMENTS = "attach-shipments";
    public static final String DETACH_SHIPMENTS = "detach-shipments";
    public static final String SAVE_FROM_V1 = "save/from/v1";
    public static final String API_GET_CUSTOM_REQ = "getCustomReq";
    public static final String SYNC = "/sync";
    public static final String LIST_TI = "/list/TI";
    public static final String LIST_CONTAINER_FOR_TI = "/list/TI/containers";
    public static final String API_DOWNLOAD_EVENTS = "/download-events";
    public static final String API_LIST_PACKS_TO_DETACH = "/list/packs/detach";
    public static final String API_RETRIEVE_BY_TENANT_ID = "/retrieve/tenantId";
    public static final String API_ASSIGN_SHIPMENT_CONTAINERS = "/assign/containers";
    public static final String API_ASSIGN_ALL_CONTAINERS = "/assign/all/containers";
    public static final String API_RETRIEVE_BY_ORDER_ID = "retrieve/orderId";
    public static final String BULK_SYNC = "/bulk-sync";
    public static final String API_DEFAULT_SHIPMENT = "/getDefaultShipment";
    public static final String GET_MASTER_DATA_MAPPING = "/get/masterDataMapping";
    public static final String CALCULATE_CONTAINER_SUMMARY = "/container/summary";
    public static final String CALCULATE_PACK_SUMMARY = "/pack/summary";
    public static final String CALCULATE_AUTO_UPDATE_WT_VOL_SHIPMENT = "/calculate/auto";
    public static final String CALCULATE_WT_VOL_SHIPMENT_ON_CHANGES = "/calculate/changes";
    public static final String LIST_PACKS_FOR_ASSIGN_DETACH = "/list/packs";
    public static final String ASSIGN_PACKS_SHIPMENTS = "/assign/shipments";
    public static final String DETACH_PACKS_SHIPMENTS = "/detach/shipments";
    public static final String GET_ALL_MASTER_DATA = "/get/master-data";
    public static final String AUTO_CALCULATE_VOLUMETRIC_WEIGHT = "/auto-calc-volumetric-weight";
    public static final String AUTO_CALCULATE_CHARGABLE = "/auto-calc-chargable";
    public static final String AUTO_CALCULATE_VOLUME = "/auto-calc-volume";
    public static final String GET_AUTO_UPDATE_DESC_GOODS = "/get/auto-update/goods";
    public static final String GET_CONT_PACK_SUMMARY = "/get/container-pack-summary";
    public static final String API_DEFAULT_CONSOLIDATION = "/getDefaultConsolidation";
    public static final String GET_ID_BY_GUID = "/get/id";
    public static final String RETIEVE_BY_MAWB_ID = "/get/mawbId";
}