package com.dpw.runner.shipment.services.dto.response;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@Schema("Bulk Container Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkContainerResponse {
    private String message;
    private List<ContainerResponse> containerResponseList = new ArrayList<>();
}
