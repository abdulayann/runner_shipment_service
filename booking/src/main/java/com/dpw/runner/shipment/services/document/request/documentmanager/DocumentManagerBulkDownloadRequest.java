package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class DocumentManagerBulkDownloadRequest implements Serializable {
    private List<String> storagePaths;
}
