package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class AlterLoadRequest {
    private String load_uuid;
}
