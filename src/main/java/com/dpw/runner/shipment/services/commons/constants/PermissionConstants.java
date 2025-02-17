package com.dpw.runner.shipment.services.commons.constants;

public final class PermissionConstants {
    public static final String customerBookingView = "Operations:CustomerBookings:View";
    public static final String customerBookingCreate = "Operations:CustomerBookings:Create";
    public static final String customerBookingUpdate = "Operations:CustomerBookings:Modify";
    public static final String customerBookingCancel = "Operations:CustomerBookings:Cancel";
    public static final String tenantSuperAdmin = "Administration.TenantSuperAdmin";
    public static final String companySuperAdmin = "Administration.CompanySuperAdmin";
    public static final String crossTenantListPermission = "Cross_Tenant:List";
    public static final String crossTenantRetrievePermission = "Cross_Tenant:Retrieve";
    public static final String crossTenantCreatePermission = "Cross_Tenant:Create";
    public static final String crossTenantUpdatePermission = "Cross_Tenant:Update";
    public static final String airDG = "AirDG";
    public static final String OCEAN_DG_APPROVER = "OceanDgPermission";
    public static final String OCEAN_DG_COMMERCIAL_APPROVER = "OceanDgCommercialPermission";
    public static final String CONSOLIDATIONS_AIR_INTER_BRANCH = "Consolidations:Air:Interbranch Consolidation";
    public static final String SHIPMENT_IN_PIPELINE_MODIFY = "Operations:ShipmentInPipeline:Modify";
    public static final String SHIPMENT_IN_PIPELINE_VIEW = "Operations:ShipmentInPipeline:View";
    private PermissionConstants() {
    }
}
