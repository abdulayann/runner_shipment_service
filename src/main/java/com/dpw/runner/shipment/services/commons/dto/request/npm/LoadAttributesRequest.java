package com.dpw.runner.shipment.services.commons.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.math.BigDecimal;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class LoadAttributesRequest implements IRunnerRequest {
    private BigDecimal weight;
    private String weight_uom;
    private Long quantity;
    private String quantity_uom;
    private BigDecimal volume;
    private String volume_uom;
    private DimensionRequest dimensions;
}
