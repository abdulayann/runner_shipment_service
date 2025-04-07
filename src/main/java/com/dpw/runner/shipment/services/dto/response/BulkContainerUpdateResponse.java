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
@ApiModel("Bulk Container Update Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkContainerUpdateResponse {
    private String message;
    private List<ContainerResponse> containerResponseList;
}
