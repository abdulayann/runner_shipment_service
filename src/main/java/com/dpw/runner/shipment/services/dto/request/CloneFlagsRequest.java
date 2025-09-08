package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ApiModel("Clone Flags Request")
public class CloneFlagsRequest extends CommonRequest implements IRunnerRequest {

    // Header
    @Builder.Default
    private boolean header = true;
    @Builder.Default
    private boolean mode = true;
    @Builder.Default
    private boolean shipmentType = true;
    @Builder.Default
    private boolean cargoType = true;
    @Builder.Default
    private boolean serviceType = true;
    @Builder.Default
    private boolean paymentTerms = true;
    @Builder.Default
    private boolean origin = true;
    @Builder.Default
    private boolean pol = true;
    @Builder.Default
    private boolean pod = true;
    @Builder.Default
    private boolean destination = true;

    // Party
    @Builder.Default
    private boolean party = true;
    @Builder.Default
    private boolean client = true;
    @Builder.Default
    private boolean shipper = true;
    @Builder.Default
    private boolean consignee = true;
    @Builder.Default
    private boolean notifyParty = true;
    @Builder.Default
    private boolean additionalParty = true;

    // General
    @Builder.Default
    private boolean general = true;
    @Builder.Default
    private boolean incoterms = true;
    @Builder.Default
    private boolean carrier = true;

    // Container(s)
    @Builder.Default
    private boolean containers = true;
    @Builder.Default
    private boolean containerType = true;
    @Builder.Default
    private boolean containerCount = true;
    @Builder.Default
    private boolean packagesPerContainer = true;
    @Builder.Default
    private boolean cargoWeightPerContainer = true;
    @Builder.Default
    private boolean containerCommodityCategory = true;

    // Package(s)
    @Builder.Default
    private boolean packages = true;
    @Builder.Default
    private boolean packageCount = true;
    @Builder.Default
    private boolean volumePerPack = true;
    @Builder.Default
    private boolean cargoWeightPerPack = true;
    @Builder.Default
    private boolean packageCommodityCategory = true;
    @Builder.Default
    private boolean dimensionPerPack = true;
    @Builder.Default
    private boolean volume = true;
    @Builder.Default
    private boolean cargoWeight = true;

    // Cargo Summary
    @Builder.Default
    private boolean cargoSummary = true;
    @Builder.Default
    private boolean description = true;
    @Builder.Default
    private boolean marksAndNumbers = true;
    @Builder.Default
    private boolean additionalTerms = true;
}
