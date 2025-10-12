package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.hbl.*;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
//@Builder
@ApiModel("Hbl Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class HblResponse extends HblDataDto implements IRunnerResponse {
    private UUID shipmentGuid;
    private List<HblCargoDto> cargoes;
    private List<HblContainerDto> containers;
    private List<HblPartyDto> notifyParties;
    private List<HblRevenueChargeDto> revenueCharges;
}