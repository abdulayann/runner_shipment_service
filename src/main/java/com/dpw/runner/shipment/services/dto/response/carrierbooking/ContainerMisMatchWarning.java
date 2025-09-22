package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ContainerMisMatchWarning {
    private String containerCode;
    private Integer bkgCount;
    private Integer consoleCount;
    private Integer difference;
}
