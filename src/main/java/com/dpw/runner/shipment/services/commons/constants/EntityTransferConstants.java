package com.dpw.runner.shipment.services.commons.constants;


public class EntityTransferConstants {
    private EntityTransferConstants(){}
    public static final String ENTITY_TRANSFER_API_HANDLE = "/api/v2/entity-transfer";
    public static final String SEND_SHIPMENT = "/send-shipment";
    public static final String SEND_SHIPMENT_VALIDATION = "/send-shipment-validation";
    public static final String CHECK_TASK_EXIST = "/check-task-exist";
    public static final String SEND_CONSOLIDATION = "/send-consolidation";
    public static final String SEND_CONSOLIDATION_VALIDATION = "/send-consolidation-validation";
    public static final String IMPORT_SHIPMENT = "/import-shipment";
    public static final String IMPORT_CONSOLIDATION = "/import-consolidation";
    public static final String SEND_SHIPMENT_SUCCESSFUL = "Shipment is sent successfully";
    public static final String SEND_CONSOLIDATION_SUCCESSFUL = "Consolidation is sent successfully";
    public static final String VALIDATION_SUCCESSFUL = "All validation passes successfully";
    public static final String CHECK_TASK_SUCCESSFUL = "Check task present for given entity successfully";
    public static final String IMPORT_SHIPMENT_SUCCESSFUL = "Shipment is imported successfully";
    public static final String IMPORT_CONSOLIDATION_SUCCESSFUL = "Consolidation is imported successfully";
    public static final String SEND_SHIPMENT_NO_SHIPMENT_FOUND = "No Shipment Present With Shipment Id: ";
    public static final String SEND_CONSOLIDATION_NO_CONSOLIDATION_FOUND = "No Consolidation Present With Consolidation Id: ";
    public static final String UNLOCATION_CODE = "LocCode";
    public static final String LOCATION_SERVICE_GUID = "LocationsReferenceGUID";
    public static final String ITEM_VALUE = "ItemValue";
    public static final String CODE = "Code";
    public static final String CURRENCY_CODE = "CurrenyCode";
    public static final String GUID = "Guid";
    public static final String SELECT_SENDTOBRANCH_OR_SENDTOORG = "Please select atleast one send to branch or send to org option";
    public static final String APPROVAL_ROLE_NOT_ASSIGNED = "Approval role not assigned to tenant: ";
    public static final String CHARGE_CODE = "ChargeCode";
    public static final String TENANT_ID = "TenantId";
    public static final String ID = "Id";
    public static final String ACTIVITY_CODE = "ActivityCode";
    public static final String NAME_WO_DIACRITICS = "NameWoDiacritics";
    public static final String NAME = "Name";

}
