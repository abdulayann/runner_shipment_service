package com.dpw.runner.shipment.services.dto.response.notification;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PendingNotificationResponse<T extends IPendingActionsResponse> implements IRunnerResponse {
    private transient Map<Long, List<T>> notificationMap;
}
