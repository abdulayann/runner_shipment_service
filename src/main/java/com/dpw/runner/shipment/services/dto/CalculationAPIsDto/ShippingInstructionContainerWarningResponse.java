package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Setter
@Getter
@NoArgsConstructor
public class ShippingInstructionContainerWarningResponse implements IRunnerResponse {
    private String containerNumber;
    private String packagePrev;
    private String packagePost;
    private String weightPrevious;
    private String weightPost;
}
