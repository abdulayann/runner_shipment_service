package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class HblRequestV2 extends HblDataRequestV2 implements IRunnerRequest {
    private UUID shipmentGuid;
    private List<HblCargoRequestV2> cargoes;
    private List<HblContainerRequestV2> containers;
    private List<HblPartyRequestV2> notifyParties;
}
