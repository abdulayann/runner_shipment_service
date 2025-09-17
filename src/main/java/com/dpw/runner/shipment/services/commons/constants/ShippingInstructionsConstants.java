package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;

import java.util.List;
import java.util.Map;

public class ShippingInstructionsConstants {

    private ShippingInstructionsConstants() {
    }

    public static final String SI_API_HANDLE = "/api/v3/si";
    public static final String CREATE_SUCCESSFUL = "Successful Shipping Instruction Creation";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Shipping Instruction Retrieval By Id";
    public static final String DELETE_SUCCESSFUL = "Successful Shipment Instruction Delete";
    public static final String UPDATE_SUCCESSFUL = "Successful Shipment Instruction Update";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String DATA_NOT_FOUND = "Shipping Instructions Not Found {}";
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of("freightDetailList", "commonPackagesList", "commonContainersList", "referenceNumbers", "sailingInformation");
    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries();
    public static final String PAYMENT_TERM_PREPAID = "PPD";
    public static final String PAYMENT_TERM_COLLECT = "CCX";
    public static final String RETRIEVE_DEFAULT_SUCCESS = "Successful Default Shipping Instruction Retrieval";
    public static final String SUBMIT_SUCCESSFUL = "Shipping Instruction submitted successfully.";
    public static final String AMEND_SUCCESSFUL = "Shipping Instruction amend successfully.";
    public static final String INVALID_ENTITY_TYPE = "Invalid value of Shipping Instruction Type";

}
