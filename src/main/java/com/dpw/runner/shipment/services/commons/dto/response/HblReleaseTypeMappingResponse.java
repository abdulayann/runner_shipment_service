package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class HblReleaseTypeMappingResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long hblId;
    private String releaseType;
    private Integer copiesPrinted;
}
