package com.dpw.runner.booking.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BulkDownloadRequest {
    private String shipmentId;
    private String consolidationId;
    private String transportMode;
    private boolean isExport;
}
