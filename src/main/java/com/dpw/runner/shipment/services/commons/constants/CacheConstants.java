package com.dpw.runner.shipment.services.commons.constants;

public class CacheConstants {
    private CacheConstants(){}
    public static final String CACHE = "Cache";
    public static final String CACHE_KEY = "Shipment_Service";
    public static final String CACHE_KEY_USER = "Shipment_Service_UserData";
    public static final String CACHE_KEY_MASTER_DATA = "Shipment_Service_MasterData";
    public static final String CACHE_API_HANDLE = "/api/v2/cache";
    public static final String EVICT_ALL_CACHE = "/evict-all-cache";
    public static final String EVICT_CACHE_BY_KEY = "/evict-cache";
    public static final String EVICTION_EXCEPTION = "Redis cache eviction exception";
    public static final String GET_USER_BY_TOKEN = "getUserByToken";
    public static final String USER_DEFINITION = "UserDefinition:";
    public static final String GET_TENANT_SETTINGS = "getV1TenantSettings";
    public static final String GET_SHIPMENT_SETTINGS = "getSettingsByTenantIdWithCache";
    public static final String SHIPMENT_SETTINGS = "ShipmentSettings:";
    public static final String TENANT_SETTINGS = "TenantSettings:";
    public static final String UNLOCATIONS = "Unlocations";
    public static final String UNLOCATIONS_AWB = "UnlocationsAWB";
    public static final String CONTAINER_TYPE = "ContainerTypes";
    public static final String CHARGE_TYPE = "ChargeTypes";
    public static final String MASTER_LIST = "MasterLists";
    public static final String VESSELS = "Vessels";
    public static final String CARRIER = "Carriers";
    public static final String CURRENCIES = "Currencies";
    public static final String COMMODITY = "Commodity";
    public static final String TENANTS = "Tenants";
    public static final String WAREHOUSES = "WareHouses";
    public static final String DG_SUBSTANCES = "DGSubstances";
    public static final String ACTIVITY_TYPE = "ActivityType";
    public static final String SALES_AGENT = "SalesAgent";
    public static final String BILLING = "Billing";
    public static final String COUNTRIES = "Countries";
    public static final String ORGANIZATIONS_WITH_ADDRESSES = "OrganizationsWithAddresses";
    public static final String COUSIN_BRANCHES_CACHE = "CousinBranchesCache";
    public static final String CUSTOMER_BOOKING = "CustomerBooking";
    public static final String CUSTOMER_BOOKING_ID = "CustomerBookingId";
    public static final String CUSTOMER_BOOKING_GUID = "CustomerBookingGuid";
}
