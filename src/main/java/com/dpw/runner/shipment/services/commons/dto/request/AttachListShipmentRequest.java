package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Getter
@Setter
public class AttachListShipmentRequest extends ListCommonRequest implements IRunnerRequest {
    private Boolean etdMatch;
    private Boolean etaMatch;
    private Boolean scheduleMatch;
    private Long consolidationId;
}
