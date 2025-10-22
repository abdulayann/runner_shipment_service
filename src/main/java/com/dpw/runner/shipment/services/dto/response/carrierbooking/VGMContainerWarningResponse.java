package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public class VGMContainerWarningResponse implements IRunnerResponse {
    private String containerNumber;
    private String vgmOldWeightValue;
    private String vgmNewWeightValue;
    private String oldGrossWeightValue;
    private String newGrossWeightValue;
    private String oldNetWeightValue;
    private String newNetWeightValue;
    private String oldTareWeightValue;
    private String newTareWeightValue;
}
