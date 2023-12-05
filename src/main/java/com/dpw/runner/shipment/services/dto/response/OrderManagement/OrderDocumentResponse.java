package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import lombok.Data;

@Data
public class OrderDocumentResponse {
    private String documentId;
    private String documentName;
    private String uploadName;
    private String documentDescription;
    private String docType;
    private String eventCode;
    private String orderId;
    private String createdBy;
    private String createdOn;
}
