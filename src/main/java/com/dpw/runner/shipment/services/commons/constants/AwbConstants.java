package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class AwbConstants {
    public static final String AWB_API_HANDLE = "/api/v2/awb";
    public static final String AWB_LIST_SUCCESSFUL = "Successful AWB Info List Retrieval";
    public static final String AWB_CREATE_SUCCESSFUL = "Successful AWB Creation";
    public static final String AWB_UPDATE_SUCCESSFUL = "Successful AWB update";
    public static final String AWB_RETRIEVE_BY_ID_SUCCESSFUL = "Successful AWB Data Retrieval By Id";
    public static final String AWB_ID = "AWB Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String MAWB_CREATE_SUCCESSFUL = "Successful MAWB Creation";
    public static final String MAWB_GOODS_AND_PACKS_UPDATE_SUCCESSFUL = "Successful Goods and Packs for MAWB Update";
    //Entity Type
    public static final String MAWB = "MAWB";
    public static final String DMAWB = "DMAWB";
    public static final String T1 = "T1";
    public static final String FREIGHT_AMOUNT = "Freight Amount";
    public static final String OTHER_AMOUNT = "Other Amount";
    public static final String AWB_SYNC_SUCCESSFUL = "Successful AWB Syncing";
    public static final String ISSUING_AGENT_NAME = "Issuing Agent Name";
    public static final String AWB_NUMBER = "AWB Number";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String PAYMENT_INFO_RETRIEVE_SUCCESS = "Payment information created successfully !";
    public static final String GET_AWB_PAYMENT_INFO = "/generate/awb-payment-info";
    public static final String DIMS_TEXT = "/dims/text";
    public static final String DIMS_TEXT_RETERIEVE_SUCCESS = "Dims text created successfully !";
    public static final String CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL = "Successfully retrieved data for charge type";
    public static final String CHARGE_TYPE_ID = "ChargeTypeId";
    public static final String AWB_RETRIEVE_ERROR = "AWB is null for Id {} with Request Id {}";
    public static final String AWB_RETRIEVE_REQUEST_NULL_ERROR = "Request is empty for AWB retrieve with Request Id {}";
    public static final String GENERATE_HAWB_BEFORE_MAWB_EXCEPTION = "To Generate Mawb, Please create Hawb for all the shipments attached";
    public static final String DUPLICATE_PAIR_AWB_OCI_INFO_VALIDATION = "Combinations of Information Identifier and Trade identification code already exists. Please enter unique combinations";
    public static final String IATA_DESCRIPTION_FIELD_VALIDATION = "IATA description shouldn't be more than 3 in other charges";
    public static final String FNM_STATUS_FETCH_SUCCESS = "Successfully fetch fnm status message";
    public static final String IATA_FETCH_RATE_SUCCESS = "Successfully fetched rates from iata";
    public static final String SERVICE_HTTP_STATUS_CODE = "SERVICE_HTTP_STATUS_CODE";
    public static final String FSU_LOCK_EVENT_CODE = "RCS";
    public static final String RESUBMIT_FZB_VALIDATION = "HAWB is updated, please resubmit the FZB by printing the original MAWB";
    public static final String RESUBMIT_FWB_VALIDATION = "MAWB is updated, please resubmit the FWB by printing the original MAWB";
    public static final String ORDER_NUMBER = "ORDER NO: %s";
    public static final List<String> SecurityStatusList = List.of("SPX", "SHR", "SCO", "Exemption Cargo");
    public static final String AOM_SCREENING_STATUS = "AOM";
    public static final String NOT_SCREENING_STATUS = "NOT";
    public static final String EXEMPTION_CARGO_SECURITY_STATUS = "Exemption Cargo";
    public static final String SPX = "SPX";
    private AwbConstants() {
    }

}
