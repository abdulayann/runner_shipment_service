package com.dpw.runner.shipment.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class BulkUploadRequest {
    private MultipartFile file;
    private Long shipmentId;
    private Long consolidationId;
    private String transportMode;
    private Boolean override;
}
