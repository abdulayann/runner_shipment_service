package com.dpw.runner.shipment.services.dto.response;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@Builder
@ApiModel("Bulk Reference Number Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkReferenceNumbersResponse {
    private String message;
    private List<ReferenceNumbersResponse> referenceNumbersResponseList;
}
