package com.dpw.runner.shipment.services.commons.constants;

public final class PermissionConstants {
    private PermissionConstants(){}
    public static final String customerBookingView = "CustomerBooking:View";
    public static final String customerBookingCreate = "CustomerBooking:Create";
    public static final String customerBookingUpdate = "CustomerBooking:Update";
    public static final String tenantSuperAdmin = "Administration.TenantSuperAdmin";
    public static final String companySuperAdmin = "Administration.CompanySuperAdmin";
    public static final String crossTenantListPermission = "Cross_Tenant:List";
    public static final String crossTenantRetrievePermission = "Cross_Tenant:Retrieve";
    public static final String crossTenantCreatePermission = "Cross_Tenant:Create";
    public static final String crossTenantUpdatePermission = "Cross_Tenant:Update";
    public static final String airDG = "AirDG";
    public static final String oceanDGApprover = ""; // TODO- Add permission constant value
    public static final String oceanDGCommercialApprover = ""; // TODO- Add permission constant value
    public static final String CONSOLIDATIONS_AIR_INTER_BRANCH = "Consolidations:Air:Interbranch Consolidation";
}
