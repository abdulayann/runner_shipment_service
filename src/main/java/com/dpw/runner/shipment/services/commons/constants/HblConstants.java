package com.dpw.runner.shipment.services.commons.constants;

public class HblConstants {

    public static final String HBL_API_HANDLE = "api/v2/hbl";
    public static final String HBL_CREATE_SUCCESS = "HBL created successfully !";
    public static final String HBL_UPDATE_SUCCESS = "HBL updated successfully !";
    public static final String HBL_LIST_SUCCESS = "HBL fetched successfully !";
    public static final String HBL_DELETE_SUCCESS = "HBL deleted successfully !";
    public static final String HBLS_RETRIEVE_BY_ID_SUCCESSFUL = "Successful HBLs Data Retrieval By Id";
    public static final String HBL_ID = "HBLs Id";
    public static final String HBL_SHIPMENT_ID = "HBL Shipment Id";
    public static final String HBL_GENERATION_SUCCESS = "HBL generated successfully !";
    public static final String HBL_RESET_SUCCESSFULL = "HBL reset successfully !";
    public static final String HBL_DATA_FOUND = "Existing HBL data found for given shipment id - %s.";
    public static final String HBL_NO_DATA_FOUND_SHIPMENT = "No HBL data found for given shipment id - %s.";
    public static final String HBL_NO_DATA_FOUND_ID = "No HBL data found for given shipment id - %s.";
    /**
     * API Constants
     */
    public static final String API_RETRIEVE_BY_SHIPMENT_ID = "/retrieve/shipment";
    public static final String API_GENERATE_HBL = "/generate";
    public static final String API_PARTIAL_UPDATE = "/partial-update";
    public static final String API_RESET_HBL = "/reset";
    /**
     * HBL Types
     */
    public static final String ORIGINAL_HBL = "ORIGINAL_HBL";


    private HblConstants() {
    }


}
