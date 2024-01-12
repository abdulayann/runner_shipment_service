package com.dpw.runner.shipment.services.dto.v1.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CompanySettingsResponse {
    private Boolean SeaLclContainerFlag;
}
