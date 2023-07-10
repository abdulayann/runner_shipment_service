package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Date;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class EventsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private String masterList;
    private Long tenantId;
    private Long shipmentId;
    private String description;
    private LocalDateTime estimated;
    private LocalDateTime actual;
    private Boolean isPublicTrackingEvent;
    private String placeName;
    private String placeDescription;
    private String latitude;
    private String longitude;
    private String source;
    private String event_estimate_update_reasons;
}
