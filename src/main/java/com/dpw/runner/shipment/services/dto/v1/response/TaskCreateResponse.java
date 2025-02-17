package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class TaskCreateResponse implements IRunnerResponse {
    private String tasksId;
}
