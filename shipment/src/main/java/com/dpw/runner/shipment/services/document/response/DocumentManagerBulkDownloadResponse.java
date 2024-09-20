package com.dpw.runner.shipment.services.document.response;

import lombok.Data;

import java.io.Serializable;

@Data
public class DocumentManagerBulkDownloadResponse implements Serializable {
    private String fileName;
    private String secureDownloadLink;
}
