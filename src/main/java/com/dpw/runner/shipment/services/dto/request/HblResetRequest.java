package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.HblReset;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;


@Data
@Builder
@Schema("Hbl Reset Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblResetRequest implements IRunnerRequest {
    private Long id;
    private HblReset resetType;
}