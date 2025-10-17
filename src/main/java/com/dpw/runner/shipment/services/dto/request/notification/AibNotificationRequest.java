package com.dpw.runner.shipment.services.dto.request.notification;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AibNotificationRequest implements IRunnerRequest {
    private Long id;
    private String containsText;
    private transient SortRequest sortRequest;
    @Builder.Default
    private Integer pageNo = 1;
    @Builder.Default
    private Integer pageSize = Integer.MAX_VALUE;
}
