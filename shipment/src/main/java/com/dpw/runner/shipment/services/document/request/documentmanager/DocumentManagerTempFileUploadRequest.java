package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DocumentManagerTempFileUploadRequest {
    private String fileName;
    private String encodedfile;
}
