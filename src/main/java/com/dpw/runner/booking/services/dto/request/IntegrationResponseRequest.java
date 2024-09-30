package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.*;


@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IntegrationResponseRequest implements IRunnerRequest {
    private Long entityId;
    private String entityType;
}
