package com.dpw.runner.booking.services.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;

@Data
public class OrderDocumentResponse implements Serializable {
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
