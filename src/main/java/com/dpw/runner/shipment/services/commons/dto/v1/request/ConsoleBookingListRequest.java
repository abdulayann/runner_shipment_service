package com.dpw.runner.shipment.services.commons.dto.v1.request;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class ConsoleBookingListRequest {
    private List<UUID> guidsList;
}
