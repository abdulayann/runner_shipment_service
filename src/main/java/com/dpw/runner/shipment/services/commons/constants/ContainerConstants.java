package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import java.util.Map;

public class ContainerConstants {

    public static final String CONTAINER_API_HANDLE = "/api/v2/containers";
    public static final String CONTAINER_V3_API_HANDLE = "/api/v3/containers";
    public static final String CONTAINER_CREATE_SUCCESSFUL = "Successful Container Data Creation";
    public static final String CONTAINER_UPDATE_SUCCESSFUL = "Successful Container Data Update";
    public static final String CONTAINER_LIST_SUCCESSFUL = "Successful Container Data List Retrieval";
    public static final String CONTAINER_DELETE_SUCCESSFUL = "Successful Container Delete";
    public static final String CALCULATION_SUCCESSFUL = "Successful Required Calculation";
    public static final String NO_DATA = "Not Found!";
    public static final String CONTAINER_ID = "Container Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String CONTAINER_EVENTS_CREATE_SUCCESSFUL = "Container Events Successfully Created";
    public static final String CONTAINER_DETACH_SUCCESSFUL = "Container Detached Successfully";
    public static final String CONTAINER_VALIDATED = "Container Validation Completed";
    public static final String GET_CONTAINERS = "/get/containers";
    public static final String ASSIGN_CONTAINERS = "/assignContainers";
    public static final String ASSIGN_PACKAGES = "/assignPackages";
    public static final String ASSIGN_PACKAGES_SHIPMENT = "/shipment-assign-packages";
    public static final String UN_ASSIGN_CONTAINERS = "/unAssignContainers";
    public static final String UN_ASSIGN_PACKAGES = "/unAssignPackages";
    public static final String UN_ASSIGN_PACKAGES_SHIPMENT = "/shipment-unAssign-packages";
    public static final String ASSIGN_SUCCESS = "Container Assignment Successful";
    public static final String UN_ASSIGN_SUCCESS = "Container Detachment Successful";
    public static final String LIST_BY_MODULE_GUID_AND_MODULE_TYPE = "/listByModuleGuidAndModuleType";
    public static final String CHECK_CONTAINERS_DELETE = "/delete/checkMultipleShipment";
    public static final String SUCCESS = "API Successfully implemented";
    public static final String EMPTY_EXCEL_SHEET = "Empty excel sheet uploaded.";
    public static final String INVALID_EXCEL_COLUMNS = "Excel Sheet is invalid. All column should have column name.";
    public static final String GUID_DUPLICATE = "GUID is duplicate at row: ";
    public static final String GUID_NOT_EXIST_FOR_CONSOLIDATION = "GUID at row: %d doesn't exist for this consolidation.";
    public static final String GUID_NOT_VALID = "GUID not valid at row: ";
    public static final String EXCEL_SHEET_NOT_VALID = "Excel sheet is not valid. {}";
    public static final String EXCEL_SHEET_INVALID = "Excel sheet is not valid.";
    public static final String CONTAINER_UPDATE_MSG = "ContainerUpdate";
    public static final String HAZ = "HAZ";
    public static final String ERROR_SYNCING_CONTAINERS = "Error syncing containers";
    public static final String SHIPMENT_CONTAINERS = "/shipment-containers";
    public static final String CONSOLIDATION_CONTAINERS = "/consolidation-containers";
    public static final String CONSOLIDATION_CONTAINERS_FOR_PACKAGE_ASSIGNMENT = "/consolidation-containers-assign-package";
    public static final String SHIPMENT_CONTAINERS_FOR_PACKAGE_ASSIGNMENT = "/shipment-containers-assign-package";
    // Table names
    public static final String TABLE_CONTAINERS = "containers";
    public static final String TABLE_SHIPMENTS_LIST = "shipmentsList";
    // Keys for TABLES_NAMES
    public static final String KEY_CONTAINER_NUMBER = "containerNumber";
    public static final String KEY_SHIPMENT_ID = "shipmentId";
    public static final String KEY_PACKS = "packs";
    public static final String KEY_PACKS_TYPE = "packsType";
    public static final String KEY_COMMODITY_CODE = "commodityCode";
    public static final String KEY_COMMODITY_GROUP = "commodityGroup";
    public static final String KEY_HS_CODE = "hsCode";
    public static final String KEY_CARRIER_SEAL_NUMBER = "carrierSealNumber";
    public static final String KEY_MARKS_NUMS = "marksNums";
    public static final String KEY_DG_CLASS = "dgClass";
    public static final String KEY_UN_NUMBER = "unNumber";
    public static final String KEY_SHIPMENT_TYPE = "shipmentType";
    public static final String KEY_CONSOLIDATION_ID = "consolidationId";
    public static final String KEY_UPDATED_AT = "updatedAt";
    public static final String KEY_CONTAINER_CODE = "containerCode";
    public static final String CONTAINER_ALREADY_ASSIGNED_MSG = "Shipment already Assigned to Container - ";
    public static final String HS_CODE_OR_COMMODITY_IS_INVALID = "HsCode/Commodity is invalid at row: %d";

    public static final Map<String, RunnerEntityMapping> TABLES_NAMES = Map.ofEntries(
            Map.entry(KEY_CONTAINER_NUMBER,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_CONTAINER_NUMBER).isContainsText(true).build()),
            Map.entry(KEY_SHIPMENT_ID,
                    RunnerEntityMapping.builder().tableName(TABLE_SHIPMENTS_LIST).dataType(String.class).fieldName(KEY_SHIPMENT_ID).isContainsText(true).build()),
            Map.entry(KEY_PACKS,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_PACKS).isContainsText(true).build()),
            Map.entry(KEY_PACKS_TYPE,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_PACKS_TYPE).isContainsText(true).build()),
            Map.entry(KEY_COMMODITY_CODE,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_COMMODITY_CODE).isContainsText(true).build()),
            Map.entry(KEY_COMMODITY_GROUP,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_COMMODITY_GROUP).isContainsText(true).build()),
            Map.entry(KEY_HS_CODE,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_HS_CODE).isContainsText(true).build()),
            Map.entry(KEY_CARRIER_SEAL_NUMBER,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_CARRIER_SEAL_NUMBER).isContainsText(true).build()),
            Map.entry(KEY_MARKS_NUMS,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_MARKS_NUMS).isContainsText(true).build()),
            Map.entry(KEY_DG_CLASS,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_DG_CLASS).isContainsText(true).build()),
            Map.entry(KEY_UN_NUMBER,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_UN_NUMBER).isContainsText(true).build()),
            Map.entry(KEY_SHIPMENT_TYPE,
                    RunnerEntityMapping.builder().tableName(TABLE_SHIPMENTS_LIST).dataType(String.class).fieldName(KEY_SHIPMENT_TYPE).isContainsText(true).build()),
            Map.entry(KEY_CONSOLIDATION_ID,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(Long.class).fieldName(KEY_CONSOLIDATION_ID).build()),
            Map.entry(KEY_UPDATED_AT,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(Long.class).fieldName(KEY_UPDATED_AT).build()),
            Map.entry(KEY_CONTAINER_CODE,
                    RunnerEntityMapping.builder().tableName(TABLE_CONTAINERS).dataType(String.class).fieldName(KEY_CONTAINER_CODE).isContainsText(true).build()),
            Map.entry(TABLE_SHIPMENTS_LIST,
                    RunnerEntityMapping.builder().tableName(TABLE_SHIPMENTS_LIST).dataType(Long.class).fieldName(TABLE_SHIPMENTS_LIST).build())

    );


    private ContainerConstants() {
    }

}
