package com.dpw.runner.shipment.services.commons.constants;


public class EntityTransferConstants {
    private EntityTransferConstants(){}
    public static final String ENTITY_TRANSFER_API_HANDLE = "/api/v2/entity-transfer";
    public static final String SEND_SHIPMENT = "/send-shipment";
    public static final String SEND_ENTITY_TO_EXTERNAL_SYSTEM = "/send-file-to-external";
    public static final String CHECK_RETRANSFER_ACCEPTED = "/check-retransfer-accepted";
    public static final String SEND_SHIPMENT_VALIDATION = "/send-shipment-validation";
    public static final String CHECK_TASK_EXIST = "/check-task-exist";
    public static final String CHECK_ENTIRY_EXIST = "/check-entity-exist";
    public static final String SEND_CONSOLIDATION = "/send-consolidation";
    public static final String SEND_CONSOLIDATION_VALIDATION = "/send-consolidation-validation";
    public static final String IMPORT_SHIPMENT = "/import-shipment";
    public static final String IMPORT_CONSOLIDATION = "/import-consolidation";
    public static final String POST_AR_VALIDATION = "/post-ar-validation";
    public static final String SEND_SHIPMENT_SUCCESSFUL = "Shipment is sent successfully";
    public static final String RETRANSFER_ACCEPTED = "Accepted status checked successfully";
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
    public static final String IDENTIFIER1 = "Identifier1";
    public static final String CODE = "Code";
    public static final String CURRENCY_CODE = "CurrenyCode";
    public static final String GUID = "Guid";
    public static final String SELECT_SENDTOBRANCH_OR_SENDTOORG = "Please select atleast one send to branch or send to org option";
    public static final String APPROVAL_ROLE_NOT_ASSIGNED = "Approval role not assigned to tenant: ";
    public static final String APPROVAL_ROLE_ACTION_NOT_ALLOWED = "Action not allowed. Only users with ShipmentInPipeline Modify permission can accept or reassign this transfer.";
    public static final String CHARGE_CODE = "ChargeCode";
    public static final String TENANT_ID = "TenantId";
    public static final String ID = "Id";
    public static final String ACTIVITY_CODE = "ActivityCode";
    public static final String NAME_WO_DIACRITICS = "NameWoDiacritics";
    public static final String NAME = "Name";
    public static final String PARENT_TENANT_ID = "ParentTenantId";
    public static final String SHIPMENT_NUMBER_PLACEHOLDER = "{#SHIPMENT_NUMBER}";
    public static final String CONSOLIDATION_NUMBER_PLACEHOLDER = "{#CONSOLIDATION_NUMBER}";
    public static final String USER_NAME_PLACEHOLDER = "{#USER_NAME}";
    public static final String BRANCH_NAME_PLACEHOLDER = "{#BRANCH_NAME}";
    public static final String CANCELLATION_REASON_PLACEHOLDER = "{#CANCELLATION_REASON}";
    public static final String CANCELLED_USER_EMAIL_PLACEHOLDER = "{#CANCELLED_USER_EMAIL_ID}";

    public static final String MISSING_RECEIVING_BRANCH_VALIDATION = "Please enter the receiving agent in entity transfer tab before proceeding with the transfer !";

    public static final String TRANSFERRED_ENTITY_ALREADY_PRESENT = "%s already exists, do you want to update the entry?";
    public static final String ALREADY_ACCEPTED_NETWORK_TRANSFER = "One or more network transfer requests are already in the ACCEPTED status: ";
    public static final String CANCELLATION_FAILURE_MSG = "Cancellation is only supported for Retransfer.";
    public static final String NETWORK_TRANSFER_NOT_ENABLED_ERROR = "Network Transfer feature is not enabled";

    public static final String MISSING_FIELD_VESSEL = "Vessel";
    public static final String MISSING_FIELD_VOYAGE = "Voyage";
    public static final String MISSING_FIELD_FLIGHT_NUMBER = "Flight Number";
    public static final String PLEASE_ENTER_THE = "Please enter the ";
    public static final String FOR_THE_CONSOLIDATION = " for the consolidation";
    public static final String TO_RE_TRIGGER_THE_TRANSFER = " to retrigger the transfer.";
    public static final String TO_TRANSFER_THE_FILES = " to transfer the files.";
    public static final String SELECT_BRANCH_FOR_ET = "one of the branches in the entity transfer details section";
    public static final String SHIPMENT_REJECTION_EMAIL_SUBJECT = "{#SHIPMENT_NUMBER} Retransfer Cancellation Notification";
    public static final String SHIPMENT_REJECTION_EMAIL_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The retransfer request for shipment {#SHIPMENT_NUMBER} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
    public static final String CONSOLIDATION_REJECTION_EMAIL_SUBJECT = "{#CONSOLIDATION_NUMBER} Retransfer Cancellation Notification";
    public static final String CONSOLIDATION_REJECTION_EMAIL_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The retransfer request for consolidation {#CONSOLIDATION_NUMBER} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
}
