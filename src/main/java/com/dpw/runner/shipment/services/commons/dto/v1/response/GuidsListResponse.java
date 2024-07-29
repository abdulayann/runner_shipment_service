package com.dpw.runner.shipment.services.commons.dto.v1.response;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class GuidsListResponse {
    private List<UUID> guidsList;
}
