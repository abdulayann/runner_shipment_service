package com.dpw.runner.shipment.services.dto.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.List;

@Data
@Builder
@Schema("Bulk Reference Number Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkReferenceNumbersResponse {
    private String message;
    private List<ReferenceNumbersResponse> referenceNumbersResponseList;
}
