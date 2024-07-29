package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.commons.dto.request.hbl.HblDataDto;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;
import java.util.UUID;


@Data
//@Builder
@ApiModel("Hbl Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblRequest extends HblDataDto implements IRunnerRequest {
    private UUID shipmentGuid;
    private List<HblCargoDto> cargoes;
    private List<HblContainerDto> containers;
    private List<HblPartyDto> notifyParties;
}