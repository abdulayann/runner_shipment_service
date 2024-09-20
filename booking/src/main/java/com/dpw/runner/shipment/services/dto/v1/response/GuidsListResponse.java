package com.dpw.runner.shipment.services.dto.v1.response;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class GuidsListResponse {
    private List<UUID> guidsList;
}
