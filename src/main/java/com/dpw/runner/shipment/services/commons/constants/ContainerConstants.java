package com.dpw.runner.shipment.services.commons.constants;

public class ContainerConstants {
    private ContainerConstants(){}
    public static final String CONTAINER_API_HANDLE = "/api/v2/containers";
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
    public static final String LIST_BY_MODULE_GUID_AND_MODULE_TYPE = "/listByModuleGuidAndModuleType";
    public static final String CHECK_CONTAINERS_DELETE = "/delete/checkMultipleShipment";
    public static final String SUCCESS = "API Successfully implemented";
    public static final String EMPTY_EXCEL_SHEET = "Empty excel sheet uploaded.";
    public static final String INVALID_EXCEL_COLUMNS = "Excel Sheet is invalid. All column should have column name.";
    public static final String GUID_DUPLICATE = "GUID is duplicate at row: ";
    public static final String GUID_NOT_EXIST_FOR_CONSOLIDATION= "GUID at row: %d doesn't exist for this consolidation.";
    public static final String GUID_NOT_VALID = "GUID not valid at row: ";
    public static final String EXCEL_SHEET_NOT_VALID = "Excel sheet is not valid. {}";
    public static final String EXCEL_SHEET_INVALID = "Excel sheet is not valid.";
    public static final String CONTAINER_UPDATE_MSG = "ContainerUpdate";
    public static final String HAZ = "HAZ";
    public static final String ERROR_SYNCING_CONTAINERS = "Error syncing containers";
}
