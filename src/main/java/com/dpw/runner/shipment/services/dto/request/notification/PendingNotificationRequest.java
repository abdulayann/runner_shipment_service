package com.dpw.runner.shipment.services.dto.request.notification;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
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
    List<Long> shipmentIdList;
    List<Long> consolidationIdList;
}
