package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class ShippingInstructionsConstants {
    public ShippingInstructionsConstants() {
    }


    public static final String SI_API_HANDLE = "/api/v3/si";
    public static final String CREATE_SUCCESSFUL = "Successful Shipping Instruction Creation";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Shipping Instruction Retrieval By Id";
    public static final String DELETE_SUCCESSFUL = "Successful Shipment Instruction Delete";
    public static final String UPDATE_SUCCESSFUL = "Successful Shipment Instruction Update";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String DATA_NOT_FOUND = "Shipping Instructions Not Found {}";
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of("carrierRoutingList","containersList");
}
