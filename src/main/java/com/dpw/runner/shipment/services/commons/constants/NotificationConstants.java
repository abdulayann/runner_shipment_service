package com.dpw.runner.shipment.services.commons.constants;

public class NotificationConstants {
    private NotificationConstants() {}

    public static final String NOTIFICATION_RETRIEVE_BY_ID_ERROR = "Notification is null for Id {} with Request Id {}";
    public static final String NOTIFICATION_API_HANDLE = "/api/v2/notifications";
    public static final String LIST_SUCCESSFUL = "Successful List";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Notification Data Retrieval By Id";
    public static final String NOTIFICATION_ID = "Notification Id";
    public static final String NOTIFICATION_GUID = "Notification Guid";
    public static final String RESPONSE_LIST = "List";
    public static final String NOTIFICATION_ACCEPT = "/acceptNotification";
    public static final String NOTIFICATION_ACCEPT_SUCCESSFUL = "Notifications accept successful";
    public static final String NOTIFICATION_ACCEPT_ERROR = "Notifications accept failed";
    public static final String NOTIFICATION_CONFIRMATION_MSG_API = "/confirmationMessage";
    public static final String NOTIFICATION_CONFIRMATION_ERROR = "Notification confirmation message failed";
    public static final String RECEIVING_BRANCH_MSG = "Receiving branch would be changed from %s to %s.";
    public static final String TRAINGULATION_BRANCH_MSG = "Triangulation branch would be changed from %s to %s.";
    public static final String RECEIVING_BRANCH = "ReceivingBranch";
    public static final String TRAINGULATION_BRANCH = "TriangulationBranch";
    public static final String REASSIGN_BRANCH_ERROR = "Branch Reassignment failed.";
    public static final String RECEIVING_BRANCH_ID_FIELD = "requestedBranchId";
    public static final String REASSIGNED_TO_BRANCH_ID_FIELD = "reassignedToBranchId";
    public static final String NOTIFICATION_CONFIRMATION_SUCCESSFUL = "Notifications confirmation message successful";
}
