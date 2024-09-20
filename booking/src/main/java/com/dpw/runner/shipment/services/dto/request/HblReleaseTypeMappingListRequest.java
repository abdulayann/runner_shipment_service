package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@Getter
@ApiModel("HblReleaseTypeMappingListRequest Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblReleaseTypeMappingListRequest implements IRunnerRequest {
    private Long hblId;
    private String releaseType;
}