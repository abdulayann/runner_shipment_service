package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.Data;

@Data
public class DocumentManagerFileAndRulesRequest {

    private String moduleName;

    private Long entityId;
}
