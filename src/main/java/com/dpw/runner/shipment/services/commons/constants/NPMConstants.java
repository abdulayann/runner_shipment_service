package com.dpw.runner.shipment.services.commons.constants;

public class NPMConstants {
    private NPMConstants(){}
    public static final String NPM_API_HANDLE = "/api/v2/external/npm";
    public static final String LIST_CONTRACT = "/contract";
    public static final String RETRIEVE_CONTRACT = "/retrieve/contract";
    public static final String RETRIEVE_CONTRACT_SHIPMENT = "/retrieve/contracts";
    public static final String CONTRACT_LIST_SUCCESSFUL = "Successful Contract Data List Retrieval";
    public static final String CONTRACT_LIST_FAILED = "Error occurred while trying to list the contracts. Exception raised is: ";
    public static final String GET_OFFERS = "/runner-offers";
    public static final String GET_OFFERS_V8 = "/runner-offers-v8";
    public static final String LIST_SUCCESSFUL = "Successfully Fetched Offers";
    public static final String ANY = "ANY";
    public static final String SQSN = "SQSN";
    public static final String FAK = "FAK";
    public static final String FCL = "FCL";
    public static final String LCL = "LCL";
    public static final String AIR = "AIR";
    public static final String UNIT = "unit";
    public static final String SELL_COST_MARGIN = "SELL_COST_MARGIN";
    public static final String CHEAPEST_OFFER_TYPE = "CHEAPEST";
    public static final String LIST_CONTRACTS_MULTI_PARTY = "/retrieve/contracts/count";

    public static final String OFFERS_V2 = "OFFERS-V2";

    public static final String OFFERS_V8 = "OFFERS-V8";

    public static final String QUOTE_CONTRACTS_DATA_API_HANDLE = "/api/v2/quoteContractsData";

    public static final String QUOTE_CONTRACTS_LIST_SUCCESSFUL = "Successful quote contracts Data List Retrieval";

    public static final String CONTAINER_COUNT = "Container_Count";
    public static final String VOLUME = "Volume";
    public static final String SHIPMENT_CAMEL_CASE = "Shipment";
    public static final String SHIPMENT = "SHIPMENT";
    public static final String WEIGHT = "Weight";
    public static final String PACKAGE = "Package";
    public static final String CHARGEABLE = "Chargeable";
    public static final String PER_MILE = "Per_Mile";
    public static final String PER_KILOMETER = "Per_Kilometer";
    public static final String PER_BL_AWB = "Per_Bl_Awb";
    public static final String CUSTOM = "Custom";

    public static final String ORIGIN = "origin";
    public static final String DESTINATION = "destination";
    public static final String LOAD_TYPE = "load_type";
    public static final String COMMODITY_CLASSIFICATION = "commodity_classification";
    public static final String PARENT_CONTRACT_ID = "parent_contract_id";
    public static final String MIN_TRANSIT_HOURS = "min_transit_hours";
    public static final String MAX_TRANSIT_HOURS = "max_transit_hours";
    public static final String VALID_TILL = "valid_till";

    public static final String HAZARDOUS = "HAZ";
    public static final int HOURS_PER_DAY = 24;

    public static final String MIN_TRANSIT_DAYS = "minTransitDays";
    public static final String MAX_TRANSIT_DAYS = "maxTransitDays";

}
