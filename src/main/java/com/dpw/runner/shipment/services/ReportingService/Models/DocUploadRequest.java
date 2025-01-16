package com.dpw.runner.shipment.services.ReportingService.Models;

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
}
