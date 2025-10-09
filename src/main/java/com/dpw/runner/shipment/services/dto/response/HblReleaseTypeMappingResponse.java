package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@Schema("Customer Booking Response Model")
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
