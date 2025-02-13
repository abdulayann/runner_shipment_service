package com.dpw.runner.shipment.services.syncing.constants;

public class SyncingConstants {
    public static final String SHIPMENT = "SHIPMENT";
    public static final String SHIPMENT_LOCK = "SHIPMENT_LOCK";
    public static final String CONSOLIDATION = "CONSOLIDATION";
    public static final String CONSOLIDATION_LOCK = "CONSOLIDATION_LOCK";
    public static final String HBL = "HBL";
    public static final String TENANT_SETTINGS = "TENANT_SETTINGS";
    public static final String AWB = "AWB";
    public static final String BULK_CONTAINERS = "BULK_CONTAINERS";
    public static final String BULK_PACKAGES = "BULK_PACKAGES";
    public static final String CONTAINERS = "CONTAINERS";
    public static final String PACKAGES = "PACKAGES";
    public static final String ROUTINGS = "ROUTINGS";
    public static final String EL_DETAILS = "EL_DETAILS";
    public static final String EVENTS = "EVENTS";
    public static final String PICKUP_DELIVERY = "PICKUP_DELIVERY";
    public static final String ALL = "ALL";
    public static final String PACKINGS = "PACKINGS";
    public static final String PRODUCT_SEQUENCE = "PRODUCT_SEQUENCE";
    public static final String ERROR_PERFORMING_AWB_SYNC = "Error performing sync on AWB entity, {}";
    public static final String ERROR_SYNCING_PACKS = "Error syncing packings";
    public static final String ERROR_PERFORMING_HBL_SYNC = "Error performing sync on hbl entity, {}";
    public static final String MAWB_STOCKS = "MAWB_STOCKS";
    public static final String ERROR_SYNCING_SHIPMENTS = "Error performing sync on shipment entity, {}";
    private SyncingConstants() {
    }
}