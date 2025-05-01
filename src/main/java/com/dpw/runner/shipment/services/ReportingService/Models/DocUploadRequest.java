package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class DocUploadRequest implements IDocumentModel{
    private String key;
    private String docType;
    private String childType;
    private String entityType;
    private String opt;
    private long id;
    private String reportId;
    private String transportMode;
    private String templateCode;
    private Boolean isTransferEnabled;
    private String shipmentType;
    private String fileName;

    // Copy Constructor
    public DocUploadRequest(DocUploadRequest other) {
        this.key = other.key;
        this.docType = other.docType;
        this.childType = other.childType;
        this.entityType = other.entityType;
        this.opt = other.opt;
        this.id = other.id;
        this.reportId = other.reportId;
        this.transportMode = other.transportMode;
        this.templateCode = other.templateCode;
        this.isTransferEnabled = other.isTransferEnabled;
        this.shipmentType = other.shipmentType;
        this.fileName = other.fileName;
    }

    public DocUploadRequest(){

    }
}
