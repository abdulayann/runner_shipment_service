package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DocumentManagerMultipleEntityFileRequest {
    private List<DocumentManagerEntityFileRequest> entities;
}
