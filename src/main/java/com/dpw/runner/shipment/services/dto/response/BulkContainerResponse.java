package com.dpw.runner.shipment.services.dto.response;

import io.swagger.annotations.ApiModel;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@ApiModel("Bulk Container Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkContainerResponse {
    private String message;
    private List<ContainerResponse> containerResponseList;
}
