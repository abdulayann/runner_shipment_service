package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Builder
@Getter
@Schema("HblReleaseTypeMappingListRequest Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblReleaseTypeMappingListRequest implements IRunnerRequest {
    private Long hblId;
    private String releaseType;
}