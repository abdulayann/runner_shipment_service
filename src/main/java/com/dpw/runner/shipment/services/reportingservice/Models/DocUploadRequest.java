package com.dpw.runner.shipment.services.reportingservice.Models;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DocUploadRequest implements IDocumentModel{
    private String key;
    private String type;
    private String entityType;
    private String opt;
    private long id;
    private String reportId;
    private String transportMode;
    private String templateCode;
    private Boolean isTransferEnabled;

    // Copy Constructor
    public DocUploadRequest(DocUploadRequest other) {
        this.key = other.key;
        this.type = other.type;
        this.entityType = other.entityType;
        this.opt = other.opt;
        this.id = other.id;
        this.reportId = other.reportId;
        this.transportMode = other.transportMode;
        this.templateCode = other.templateCode;
        this.isTransferEnabled = other.isTransferEnabled;
    }

    public DocUploadRequest(){

    }
}
