package com.dpw.runner.shipment.services.commons.constants;

public final class PermissionConstants {
    private PermissionConstants(){}
    public static final String CUSTOMER_BOOKINGS_VIEW = "Operations:CustomerBookings:View";
    public static final String CUSTOMER_BOOKINGS_CREATE = "Operations:CustomerBookings:Create";
    public static final String CUSTOMER_BOOKINGS_MODIFY = "Operations:CustomerBookings:Modify";
    public static final String CUSTOMER_BOOKINGS_CANCEL = "Operations:CustomerBookings:Cancel";
    public static final String TENANT_SUPER_ADMIN = "Administration.TenantSuperAdmin";
    public static final String COMPANY_SUPER_ADMIN = "Administration.CompanySuperAdmin";
    public static final String CROSS_TENANT_LIST = "Cross_Tenant:List";
    public static final String CROSS_TENANT_RETRIEVE_PERMISSION = "Cross_Tenant:Retrieve";
    public static final String CROSS_TENANT_CREATE_PERMISSION = "Cross_Tenant:Create";
    public static final String CROSS_TENANT_UPDATE_PERMISSION = "Cross_Tenant:Update";
    public static final String AIR_DG = "AirDG";
    public static final String AIR_SECURITY_PERMISSION = "Manage:AirSecurity";
    public static final String OCEAN_DG_APPROVER = "OceanDgPermission";
    public static final String OCEAN_DG_COMMERCIAL_APPROVER = "OceanDgCommercialPermission";
    public static final String CONSOLIDATIONS_AIR_INTER_BRANCH = "Consolidations:Air:Interbranch Consolidation";
    public static final String SHIPMENT_IN_PIPELINE_MODIFY = "Operations:ShipmentInPipeline:Modify";
    public static final String SHIPMENT_IN_PIPELINE_VIEW = "Operations:ShipmentInPipeline:View";
}
