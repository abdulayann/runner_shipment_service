package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.commons.dto.request.HblPartyDto;
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
}