package com.dpw.runner.shipment.services.dto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Decline Notification Request Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DeclineNotificationRequest implements IRunnerRequest {
    Long id;
    String reason;
}
