package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

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