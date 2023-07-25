package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;


@Data
//@Builder
@ApiModel("Hbl Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblRequest extends HblDataDto implements IRunnerRequest {

    private List<HblCargoDto> cargoes;
    private List<HblContainerDto> containers;
    private List<HblPartyDto> notifyParties;
}