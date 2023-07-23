package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
//@Builder
@ApiModel("Hbl Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class HblResponse extends HblDataDto implements IRunnerResponse {
    private List<HblCargoDto> cargoes;
    private List<HblContainerDto> containers;
    private List<HblPartyDto> notifyParties;
}