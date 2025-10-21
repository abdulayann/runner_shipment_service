package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class EventsBulkRequest {
    private List<EventsRequest> eventsRequestList;
}
