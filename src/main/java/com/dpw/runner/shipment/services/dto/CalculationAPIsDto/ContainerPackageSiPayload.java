package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.CommonPackages;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class ContainerPackageSiPayload {
    private List<CommonContainers> containerDetail;
    private List<CommonPackages> packageDetail;
}
