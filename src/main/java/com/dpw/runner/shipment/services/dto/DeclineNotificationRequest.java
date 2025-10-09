package com.dpw.runner.shipment.services.dto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Getter
@Setter
@Schema("Decline Notification Request Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DeclineNotificationRequest implements IRunnerRequest {
    Long id;
    String reason;
}
