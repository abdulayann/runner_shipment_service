package com.dpw.runner.shipment.services.commons.constants;

public class NotificationConstants {
    private NotificationConstants() {}

    public static final String NOTIFICATION_RETRIEVE_BY_ID_ERROR = "Notification is null for Id {} with Request Id {}";
    public static final String NOTIFICATION_API_HANDLE = "/api/v2/notifications";
    public static final String NOTIFICATION_V3_API_HANDLE = "/api/v3/notifications";
    public static final String LIST_SUCCESSFUL = "Successful List";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Notification Data Retrieval By Id";
    public static final String NOTIFICATION_ID = "Notification Id";
    public static final String NOTIFICATION_GUID = "Notification Guid";
    public static final String RESPONSE_LIST = "List";
    public static final String NOTIFICATION_ACCEPT = "/acceptNotification";
    public static final String NOTIFICATION_REJECT = "/rejectNotification";
    public static final String NOTIFICATION_ACCEPT_SUCCESSFUL = "Notifications accept successful";
    public static final String NOTIFICATION_REJECT_SUCCESSFUL = "Notifications reject successful";
    public static final String NOTIFICATION_REJECT_ERROR = "Notifications reject failed";
    public static final String NOTIFICATION_ACCEPT_ERROR = "Notifications accept failed";
    public static final String NOTIFICATION_CONFIRMATION_MSG_API = "/confirmationMessage";
    public static final String NOTIFICATION_CONFIRMATION_ERROR = "Notification confirmation message failed";
    public static final String RECEIVING_BRANCH_MSG = "Receiving branch would be changed from %s to %s.";
    public static final String TRAINGULATION_BRANCH_MSG = "Triangulation branch would be changed from %s to %s.";
    public static final String RECEIVING_BRANCH = "ReceivingBranch";
    public static final String TRAINGULATION_BRANCH = "TriangulationBranch";
    public static final String REASSIGN_BRANCH_ERROR = "Branch Reassignment failed.";
    public static final String RECEIVING_BRANCH_ID_FIELD = "reassignedFromBranchId";
    public static final String REASSIGNED_TO_BRANCH_ID_FIELD = "reassignedToBranchId";
    public static final String NOTIFICATION_CONFIRMATION_SUCCESSFUL = "Notifications confirmation message successful";

    // Reassignment cancellation email templates
    public static final String EMAIL_TEMPLATE_REASSIGNMENT_CANCELLATION_SHIPMENT_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The reassignment request for shipment {#SHIPMENT_NUMBER} to {#ASSIGNED_TO_BRANCH} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
    public static final String EMAIL_TEMPLATE_REASSIGNMENT_CANCELLATION_SHIPMENT_SUBJECT = "{#SHIPMENT_NUMBER} Reassignment Cancellation Notification";
    public static final String EMAIL_TEMPLATE_REASSIGNMENT_CANCELLATION_CONSOLIDATION_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The reassignment request for consolidation {#CONSOLIDATION_NUMBER} to {#ASSIGNED_TO_BRANCH} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
    public static final String EMAIL_TEMPLATE_REASSIGNMENT_CANCELLATION_CONSOLIDATION_SUBJECT = "{#CONSOLIDATION_NUMBER} Reassignment Cancellation Notification";

    // Request to transfer cancellation email templates
    public static final String EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CANCELLATION_SHIPMENT_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The early transfer request for shipment {#SHIPMENT_NUMBER} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
    public static final String EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CANCELLATION_SHIPMENT_SUBJECT = "Early Transfer Request Cancellation Notification of Shipment – {#SHIPMENT_NUMBER}";
    public static final String EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CANCELLATION_CONSOLIDATION_BODY = "<p>Dear user,</p>  <p>&nbsp;</p>  <p>The early transfer request for consolidation {#CONSOLIDATION_NUMBER} has been cancelled by {#USER_NAME} at {#BRANCH_NAME}. The reason for cancellation is: “{#CANCELLATION_REASON}”.</p> <p>&nbsp;</p> <p>In case of any further clarification, please reach out to {#CANCELLED_USER_EMAIL_ID}.</p> <p>Thanks,&nbsp;<br /> Cargoes Runner</p>";
    public static final String EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CANCELLATION_CONSOLIDATION_SUBJECT = "Early Transfer Request Cancellation Notification of Consolidation – {#CONSOLIDATION_NUMBER}";


    public static final String SHIPMENT_NUMBER_PLACEHOLDER = "{#SHIPMENT_NUMBER}";
    public static final String CONSOLIDATION_NUMBER_PLACEHOLDER = "{#CONSOLIDATION_NUMBER}";
    public static final String USER_NAME_PLACEHOLDER = "{#USER_NAME}";
    public static final String BRANCH_NAME_PLACEHOLDER = "{#BRANCH_NAME}";
    public static final String CANCELLATION_REASON_PLACEHOLDER = "{#CANCELLATION_REASON}";
    public static final String CANCELLED_USER_EMAIL_PLACEHOLDER = "{#CANCELLED_USER_EMAIL_ID}";
    public static final String ASSIGN_TO_BRANCH_PLACEHOLDER = "{#ASSIGNED_TO_BRANCH}";
}
