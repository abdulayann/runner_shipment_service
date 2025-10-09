package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema("Clone Flags Request")
public class CloneFlagsRequest extends CommonRequest implements IRunnerRequest {

    // Header
    private boolean header;
    private boolean mode;
    private boolean shipmentType;
    private boolean cargoType;
    private boolean serviceType;
    private boolean paymentTerms;
    private boolean origin;
    private boolean pol;
    private boolean pod;
    private boolean destination;

    // Party
    private boolean party;
    private boolean client;
    private boolean shipper;
    private boolean consignee;
    private boolean notifyParty;
    private boolean additionalParty;

    // General
    private boolean general;
    private boolean incoterms;
    private boolean carrier;

    // Container(s)
    private boolean containers;
    private boolean containerType;
    private boolean containerCount;
    private boolean packagesPerContainer;
    private boolean cargoWeightPerContainer;
    private boolean containerCommodityCategory;

    // Package(s)
    private boolean packages;
    private boolean packageType;
    private boolean packageCount;
    private boolean volumePerPack;
    private boolean cargoWeightPerPack;
    private boolean packageCommodityCategory;
    private boolean dimensionPerPack;
    private boolean volume;
    private boolean cargoWeight;

    // Cargo Summary
    private boolean cargoSummary;
    private boolean description;
    private boolean marksAndNumbers;
    private boolean additionalTerms;
}
