package com.dpw.runner.shipment.services.dto.request.notification;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PendingNotificationRequest implements IRunnerRequest {
    private List<Long> shipmentIdList;
    private List<Long> consolidationIdList;
    private String containsText;
    private transient SortRequest sortRequest;
}
